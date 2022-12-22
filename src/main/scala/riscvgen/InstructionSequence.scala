package riscvgen
import randomapi.Gen
import randomapi.RNG
import randomapi.ParametricRandom
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.sys.process.*
import util.control.Breaks.*

enum InstructionSequenceType:
  case PrimitiveInstruction 
  case ReadAfterWrite
  case ForLoop
  case BranchSequence
  case WhileLoop

enum InstructionSequence:
   // PrimitiveInstruction: Wraps some Instruction type
  case PrimitiveInstruction(operator: RiscvOperator)
  case ReadAfterWrite(body: InstructionSequence)
  case ForLoop(body: InstructionSequence, count: Int)
  // For now: assume everything is a basic block.
  case BranchSequence(basicBlock1: InstructionSequence, basicBlock2: InstructionSequence, cond: RiscvBranch)
  case WhileLoop(body: InstructionSequence, cond: RiscvBranch)

object InstructionSequence:
  // Could be made easier with reflection?
  val MaxDepth = 10
  val InstructionSequenceBiases = {(depth: Int) => 
    val ProbPrimitive = depth.toDouble / MaxDepth
    // Currently excluding while loops
    val ProbNonPrimitive = (1 - depth.toDouble / MaxDepth) / (InstructionSequenceType.values.length - 2)
    ProbPrimitive +: Seq.fill(InstructionSequenceType.values.length - 2)(ProbNonPrimitive) :+ 0d
    }
  val MinAddress: Long = 0x80004000
  val AddressSpaceSize = 1000
  val WordSize = 4

  def gen(depth: Int = 0): Gen[InstructionSequence] = 
    for {
      sequenceType <- Gen.oneOf(InstructionSequenceType.values, biases = Some(InstructionSequenceBiases(depth)))
      sequence <- {sequenceType match
        case InstructionSequenceType.ReadAfterWrite => for {
          body <- gen(depth + 1)
        } yield (ReadAfterWrite(body))

        case InstructionSequenceType.ForLoop => for {
          count <- Gen.range(0, 100)
          body <- gen(depth + 1)
        } yield (ForLoop(body, count))

        case InstructionSequenceType.WhileLoop => for {
          body <- gen(depth + 1)
          cond <- Gen.oneOf(RiscvBranch.values)
        } yield (WhileLoop(body, cond))

        case InstructionSequenceType.PrimitiveInstruction => for {
          op <- Gen.oneOf(RiscvOperator.values)
        } yield(PrimitiveInstruction(op))

        case InstructionSequenceType.BranchSequence => for {
          basicBlock1 <- gen(depth + 1)
          basicBlock2 <- gen(depth + 1)
          cond <- Gen.oneOf(RiscvBranch.values)
        } yield(BranchSequence(basicBlock1, basicBlock2, cond)) }
    } yield (sequence)

  def genInstrsForSeq(instrseq: InstructionSequence, sequenceNumber: Int): Gen[Seq[String]] =
    genInstrsForSeqHelper(instrseq, RiscvReg.values.toIndexedSeq, sequenceNumber, 0).map(p => p(0))

  def genInstrsForSeqHelper(instrseq: InstructionSequence, allowedRegs: Seq[RiscvReg], sequenceNumber: Int, labelIdx: Int): Gen[(Seq[String], Int)] =
    if (allowedRegs.length == 1) then Gen.lift((Seq.empty, labelIdx)) else
    instrseq match  
      case ReadAfterWrite(body) =>
        for {
          dest <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs)
          src <- Gen.oneOf(allowedRegs)
          base <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs).ensureNot(src) // TODO: fix
          locationMove <- RiscvOperator.LI.makeGen(rdGen=Gen.lift(base), immGen=Gen.range(0, AddressSpaceSize / 4).map(MinAddress + _.toLong * 4))
          store <- RiscvMem.SW.makeGen(rs1Gen=Gen.lift(base), rs2Gen=Gen.lift(src), immGen=Gen.lift(0))
          b <- genInstrsForSeqHelper(body, allowedRegs, sequenceNumber, labelIdx)
          load <- RiscvMem.LW.makeGen(rs1Gen=Gen.lift(base), rdGen=Gen.lift(dest), immGen=Gen.lift(0))
      } yield ( (Seq(locationMove, store) ++ b(0) ++ Seq(load), b(1)) )

      case ForLoop(body, count) => 
        for {
          counterRegIdx <- Gen.range(1, allowedRegs.length)
          counterReg <- Gen.lift(allowedRegs(counterRegIdx))
          mov <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rdGen=Gen.lift(counterReg), immGen=Gen.lift(count))
          b <- genInstrsForSeqHelper(body, allowedRegs.patch(counterRegIdx, Nil, 1), sequenceNumber, labelIdx + 1)
          sub <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(counterReg), rdGen=Gen.lift(counterReg), immGen=Gen.lift(-1))
          j <- RiscvJump.J.makeGen(label=s"loop_${sequenceNumber}_$labelIdx")
          check <- RiscvBranch.BGE.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rs2Gen=Gen.lift(counterReg), label=s"continue_${sequenceNumber}_$labelIdx")
          forloop <- LABEL(s"loop_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield ((Seq(mov, forloop, check) ++ b(0) ++ Seq(sub, j, continue), b(1)))

      case WhileLoop(body, cond) =>
        for {
          b <- genInstrsForSeqHelper(body, allowedRegs, sequenceNumber, labelIdx + 1)
          c <- cond.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          whileloop <- LABEL(s"while_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
          j <- RiscvJump.J.makeGen(label=s"while_${sequenceNumber}_$labelIdx")
        } yield((Seq(whileloop, c) ++ b(0) ++ Seq(j, continue), b(1)))

      case PrimitiveInstruction(operator) => 
        for {
          instr <- operator.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), rdGen = Gen.oneOf(allowedRegs))
        } yield ((Seq(instr), labelIdx))

      case BranchSequence(basicBlock1, basicBlock2, cond) => 
        for {
          bb1 <- genInstrsForSeqHelper(basicBlock1, allowedRegs, sequenceNumber, labelIdx + 1)
          bb2 <- genInstrsForSeqHelper(basicBlock2, allowedRegs, sequenceNumber, bb1(1))
          j <- RiscvJump.J.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          c <- cond.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), label=s"then_${sequenceNumber}_$labelIdx")
          thenbranch <- LABEL(s"then_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield ((Seq(c) ++ bb1(0) ++ Seq(j, thenbranch) ++ bb2(0) ++ Seq(continue), bb2(1)))

  def makeRandomInstructions(random: RNG, i: Int): Unit =
    val nSeqs = 5
    val sequenceGenerator = Gen.seqToGen(Seq.fill(nSeqs)(gen()))
    val instructions = 
      sequenceGenerator.flatMap(
        instrSequences => Gen.seqToGen(instrSequences.zipWithIndex.map((is, idx) => genInstrsForSeq(is, idx)))
      )
      
    val instSeq: Seq[String] = instructions.generate(random).flatten

    val prologue = "#include \"riscv_test.h\"\n#include \"test_macros.h\"\nRVTEST_RV64U\nRVTEST_CODE_BEGIN\n"
    val epilogue = "RVTEST_PASS\nRVTEST_CODE_END\n.data\nRVTEST_DATA_BEGIN\nTEST_DATA\nRVTEST_DATA_END\n"
    
    Files.write(Paths.get(s"test/rv64ui/out$i.S"), 
      (instSeq.foldLeft(prologue)((sofar, s) => sofar + s + '\n') + epilogue).getBytes(StandardCharsets.UTF_8))

  // Main Zest Loop

  def zestLoop(seeds: Seq[Int]): Unit =
    val NumBoilerPlateAccesses = 1666
    val NumBoilerPlateMisses = 1
    val SpikeTimeout = 2000 // ms
    val PollingFactor = 8

    val numIterations = 100
    val numMutationTrials = 5
    val numMutations = 6
    val topRandomMisses: Array[Double] = Array.fill(10)(0d)
    var successRandoms: Seq[ParametricRandom] = seeds.map(i => ParametricRandom.fromSeed(i))
    var failureRandoms: Seq[ParametricRandom] = Seq()
    var compileLog: ProcessLogger = ProcessLogger(_ => (), _ => ())
    
    "mkdir -p logs".!!
    for (j <- 1 to numIterations) {
      println(s"Running outer iteration $j")
      var currRandoms: Seq[ParametricRandom] = Seq()
      successRandoms.zipWithIndex.foreach { (random, randomIdx) =>
        if (randomIdx % 10 == 0) {
          println(s"$randomIdx/${successRandoms.length} seeds")
        }
        val makeBuilder = StringBuilder("rv64ui_sc_tests = \\\n")
        var mutRandoms: Seq[ParametricRandom] = Seq()
        currRandoms = currRandoms :+ random // Recycle this random
        
        // Generation
        for (i <- 1 to numMutationTrials) {
          val mutRandom = random.mutate(numMutations)
          makeRandomInstructions(mutRandom, i)
          mutRandoms = mutRandoms :+ mutRandom
          makeBuilder.addAll(s"out$i \\\n")
        }

        // Compilation
        Files.write(Paths.get(s"test/rv64ui/Makefrag"), makeBuilder.toString().getBytes(StandardCharsets.UTF_8))
        val makeResult = "make -C ./test".!(compileLog)
        if makeResult != 0 then throw Exception("Stimulus compilation failed")

        // Running
        for (i <- 1 to numMutationTrials) {
          val spikeCmd = s"spike -l --log=./logs/out$i.log --dc=32:1:64 ./test/rv64ui-p-out$i"
          var failedFlag = false
          var successFlag = true
          var lines: Seq[String] = Seq()
          val spikeProcess = stringToProcess(spikeCmd).run(ProcessLogger(
            spikeResult => {
              lines = lines :+ spikeResult
              if (lines.length == 8) {
                if (lines(0).matches("Assertion failed.*")) {
                  failureRandoms = failureRandoms :+ mutRandoms(i - 1) // Maybe an invalid instead
                } else {
                  val getStat = (idx: Int) => lines(idx).split(":")(1).strip().toInt
                  val totalAccesses = getStat(2) + getStat(3) - NumBoilerPlateAccesses
                  val totalMisses = getStat(4) + getStat(5) - NumBoilerPlateMisses
                  // Miss Rate from spike is not correct because there are some accesses made by boilerplate
                  val missRate = if totalAccesses == 0 then 0d else totalMisses.toDouble / totalAccesses
                  topRandomMisses.sortInPlace()
                  if (missRate > topRandomMisses(0)) {
                    // if (missRate > 1.0) {
                    //   println(s"Accesses: $totalAccesses, Misses: $totalMisses")
                    // }
                    currRandoms = currRandoms :+ mutRandoms(i - 1)
                    topRandomMisses(0) = missRate
                  }
                }
              }
            },
            _ => { if(!failedFlag) { 
                failureRandoms = failureRandoms :+ mutRandoms(i - 1)
                failedFlag = true
              }
            }
          ))
          breakable {
            for (_ <- 1 to PollingFactor) {
              Thread.sleep(SpikeTimeout / PollingFactor)
              if !spikeProcess.isAlive() then break()
            }
            spikeProcess.destroy()
            if spikeProcess.exitValue() != 0 && !failedFlag then failureRandoms = failureRandoms :+ mutRandoms(i - 1)
          }
        }
      }
      topRandomMisses.sortInPlace()
      println(s"The Best are ${topRandomMisses.toList}")
      println(s"Length of re-randomizing list: ${currRandoms.length}")
      successRandoms = currRandoms
    }

  def main(args: Array[String]): Unit =
    val seeds = List(2, 194, 6509, 347)
    zestLoop(seeds)



