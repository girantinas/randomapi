package riscvgen

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.sys.process.*
import randomapi.{ParametricRandom, PseudoRandom, ScalaRandom, Gen, RNG}
import randomapi.Gen.*
import util.control.Breaks.*
import java.nio.file.{Files, Paths, StandardCopyOption}
import riscvgen.InstructionSequence
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption

object RandomInstructor:
  def makeRandomInstructions(random: ParametricRandom[Any],
                             randomIdx: Int,
                             numMutationTrials: Int,
                             numMutations: Int,
                             numSeqs: Int,
                             mutRng: RNG): Unit =
    // Generation
    for (i <- 1 to numMutationTrials) {
      val mutRandom = random.mutate(numMutations, mutRng)
      val sequenceGenerator = Gen.seqToGen(Seq.fill(numSeqs)(InstructionSequence.gen()))
      val instructions = 
        sequenceGenerator.flatMap(
          instrSequences => Gen.seqToGen(instrSequences.zipWithIndex.map((is, idx) => InstructionSequence.genInstrsForSeq(is, idx + 1)))
        )
      
      val instSeq: Seq[String] = instructions.generate(mutRandom).flatten

      val prologue = "#include \"riscv_test.h\"\n#include \"test_macros.h\"\nRVTEST_RV64U\nRVTEST_CODE_BEGIN\n"
      val epilogue = "RVTEST_PASS\nRVTEST_CODE_END\n.data\nRVTEST_DATA_BEGIN\nTEST_DATA\nRVTEST_DATA_END\n"
      
      Files.write(Paths.get(s"test/asm/out${i + numMutationTrials * randomIdx}.S"), 
        (instSeq.foldLeft(prologue)((sofar, s) => sofar + s + '\n') + epilogue).getBytes(StandardCharsets.UTF_8))
    }

  def makeSeededInstructions(seeds: ArrayBuffer[Int]): Unit =
    val ScalaSeedMut = 999

    var randoms: ArrayBuffer[ParametricRandom[Any]] = seeds.map(i => ParametricRandom.fromSeed(i))
    val mutGen = ScalaRandom(ScalaSeedMut)

    for ((random, randomIdx) <- randoms.zipWithIndex) {
      makeRandomInstructions(random, randomIdx, 5, 10, 2, mutGen)
    }

  def main(args: Array[String]): Unit =
    val seeds = ArrayBuffer(2, 194, 6509, 347, 1234, 4444, 2136, 1000, 934, 312, 3094, 54983, 19203, 2389, 129, 442, 3)
    makeSeededInstructions(seeds)
