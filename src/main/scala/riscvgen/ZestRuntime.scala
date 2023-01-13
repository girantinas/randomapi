package riscvgen

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.sys.process.*
import randomapi.{ParametricRandom, PseudoRandom, Gen}
import util.control.Breaks.*
import java.nio.file.{Files, Paths, StandardCopyOption}
import riscvgen.InstructionSequence
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption

object ZestRuntime:
  def makeRandomInstructions(random: ParametricRandom,
                             numMutationTrials: Int,
                             numMutations: Int,
                             numSeqs: Int,
                             mutRandoms: ArrayBuffer[ParametricRandom]): Unit =
    val makeBuilder = StringBuilder("rv64ui_sc_tests = \\\n")
    // Generation
    for (i <- 1 to numMutationTrials) {
      val mutRandom = random.mutate(numMutations)
      val sequenceGenerator = Gen.seqToGen(Seq.fill(numSeqs)(InstructionSequence.gen()))
      val instructions = 
        sequenceGenerator.flatMap(
          instrSequences => Gen.seqToGen(instrSequences.zipWithIndex.map((is, idx) => InstructionSequence.genInstrsForSeq(is, idx + 1)))
        )
      
      val instSeq: Seq[String] = instructions.generate(random).flatten

      val prologue = "#include \"riscv_test.h\"\n#include \"test_macros.h\"\nRVTEST_RV64U\nRVTEST_CODE_BEGIN\n"
      val epilogue = "RVTEST_PASS\nRVTEST_CODE_END\n.data\nRVTEST_DATA_BEGIN\nTEST_DATA\nRVTEST_DATA_END\n"
      
      Files.write(Paths.get(s"test/rv64ui/out$i.S"), 
        (instSeq.foldLeft(prologue)((sofar, s) => sofar + s + '\n') + epilogue).getBytes(StandardCharsets.UTF_8))
      mutRandoms += mutRandom
      makeBuilder.addAll(s"out$i \\\n")
    }

    // Compilation
    Files.write(Paths.get(s"test/rv64ui/Makefrag"), makeBuilder.toString().getBytes(StandardCharsets.UTF_8))
    val makeResult = "make -C ./test".!(ProcessLogger(_ => (), _ => ()))
    if makeResult != 0 then throw Exception("Stimulus compilation failed")

  def zestLoop(seeds: ArrayBuffer[Int]): Unit =
    val NumBoilerPlateAccesses = 1666
    val NumBoilerPlateMisses = 1
    val NumBoilerPlateInsts = 5079
    val SpikeTimeout = 2000 // ms
    val PollingFactor = 8
    val ScalaSeed = 1000
    val NumSequences = 2

    val numIterations = 100
    val numMutationTrials = 5
    val numMutationSchedule = (currBest: Double) => if currBest < 0.2 then (30 * (0.2 - currBest) + 5).round else 5
    val topRandomMisses: Array[Double] = Array.fill(10)(0d)
    var successRandoms: ArrayBuffer[ParametricRandom] = seeds.map(i => ParametricRandom.fromSeed(i))
    val failureRandoms: ArrayBuffer[ParametricRandom] = ArrayBuffer()
    val newSeedGen = Random(ScalaSeed)
    
    "mkdir -p logs".!!
    for (j <- 1 to numIterations) {
      println(s"Running outer iteration $j")

      val currRandoms: ArrayBuffer[ParametricRandom] = ArrayBuffer()
      for ((random, randomIdx) <- successRandoms.zipWithIndex) {
        if (randomIdx % 10 == successRandoms.length % 10) {
          println(s"$randomIdx/${successRandoms.length} seeds")
        }

        val oldNumRandoms = currRandoms.length

        val mutRandoms: ArrayBuffer[ParametricRandom] = ArrayBuffer()
        topRandomMisses.sortInPlace()
        val numMutations = numMutationSchedule(topRandomMisses(topRandomMisses.length - 1)).toInt
        makeRandomInstructions(random, numMutationTrials, numMutations, NumSequences, mutRandoms)

        // Running
        for (i <- 1 to numMutationTrials) {
          println(s"------------------- $i")
          val spikeCmd = s"spike -l --log=./logs/out.log --dc=16:1:16 ./test/rv64ui-p-out$i"
          var failedFlag = false
          var lines: Seq[String] = Seq()
          val spikeProcess = stringToProcess(spikeCmd).run(ProcessLogger(
            spikeResult => lines = lines :+ spikeResult,
            err => {
              if(!failedFlag) { 
                failureRandoms += mutRandoms(i - 1)
                println(s"ERROR: $err")
              }
              failedFlag = true
            }
          ))

          breakable {
            for (_ <- 1 to PollingFactor) {
              Thread.sleep(SpikeTimeout / PollingFactor)
              if !spikeProcess.isAlive() then break()
            }
          }

          spikeProcess.destroy()
          
          if (spikeProcess.exitValue() != 0 && !failedFlag) {
            println("ERROR: Timed out") 
            failureRandoms += mutRandoms(i - 1)
            failedFlag = true
          } else if (lines.length > 0 && lines(0).matches("Assertion failed.*")) {
            println("ERROR: Runtime Error")
            failureRandoms += mutRandoms(i - 1) // Maybe an invalid instead
          } else if (!failedFlag && lines.length == 8) {
            val getStat = (idx: Int) => lines(idx).split(":")(1).strip().toInt
            val totalAccesses = getStat(2) + getStat(3) - NumBoilerPlateAccesses
            val totalMisses = getStat(4) + getStat(5) - NumBoilerPlateMisses
            val totalInsts = io.Source.fromFile("./logs/out.log").getLines.size - NumBoilerPlateInsts
            val missPerInst = totalMisses.toDouble / (totalInsts)
            topRandomMisses.sortInPlace()
            if (missPerInst > topRandomMisses(0)) {
              if (missPerInst >= 0.2) {
                println(s"out$i is optimal")
                println(s"Seed: ${mutRandoms(i - 1).rng.asInstanceOf[PseudoRandom].seed}")
                sys.exit()
              }
              currRandoms += mutRandoms(i - 1)
              topRandomMisses(0) = missPerInst
            }
          }
        }

        // Recycle the random only if it made valid programs
        if (currRandoms.length > oldNumRandoms) {
          currRandoms += random
        } else {
          currRandoms += ParametricRandom.fromSeed(newSeedGen.nextInt())
        }
      }
      topRandomMisses.sortInPlace()
      println(s"The Best are ${topRandomMisses.toList}")

      val csvOutput = topRandomMisses.foldLeft("")((soFar, miss) => soFar + "," + miss.toString()) + "\n"
      Files.write(Paths.get("./logs/misses.csv"), csvOutput.getBytes(), StandardOpenOption.APPEND, StandardOpenOption.CREATE)

      println(s"Length of re-randomizing list: ${currRandoms.length}")
      successRandoms = currRandoms
    }

  def main(args: Array[String]): Unit =
    val seeds = ArrayBuffer(2, 194, 6509, 347)
    zestLoop(seeds)
