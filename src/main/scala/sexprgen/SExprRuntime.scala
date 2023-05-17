package sexprgen

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.nio.file.{Files, Paths, StandardCopyOption, StandardOpenOption}
import randomapi.{Gen, ParametricRandom, RNG, ScalaRandom, ParameterState}
import sexprgen.{DecisionType}
import sexprgen.SExpr
import SExpr.Expression
import java.nio.charset.StandardCharsets
import sexprgen.DecisionType.*

object SExprRuntime:
  private type PRDT = ParametricRandom[DecisionType]
  private type MarkingMap = Map[DecisionType, Double]
  val NumIterations = 2000
  val NumMutationTrials = 5
  val ScalaSeedNew = 1000
  val ScalaSeedMut = 999
  val NumSeeds = 30
  val NumMutations = 6

  def randomStimulus(currStimulus: PRDT, markingProbs: MarkingMap, externalRng: RNG): (SExpr, PRDT) = 
    val random = ParametricRandom.fromSeed(externalRng.nextInt()(0))
    val (sexpr: SExpr, prdt: PRDT) = SExpr.genSExprRec().generateWithRNG(random) : @unchecked
    return (sexpr, prdt)

  def mutateStimulus(currStimulus: PRDT, markingProbs: MarkingMap, externalRng: RNG): (SExpr, PRDT) =
    val random = currStimulus.mutate(NumMutations, externalRng).reset()
    val (sexpr: SExpr, prdt: PRDT) = SExpr.genSExprRec().generateWithRNG(random) : @unchecked
    return (sexpr, prdt)

  def markMutateStimulus(currStimulus: PRDT, markingProbs: MarkingMap, externalRng: RNG): (SExpr, PRDT) =
    val clearStimulus = currStimulus.reset()
    val (_, fullRandom: PRDT) = SExpr.genSExprRec().generateWithRNG(currStimulus) : @unchecked
    val random = fullRandom.mutate(markingProbs, externalRng)
    val (sexpr: SExpr, prdt: PRDT) = SExpr.genSExprRec().generateWithRNG(random) : @unchecked
    return (sexpr, prdt)

  def fuzzingLoop(seeds: ArrayBuffer[Int], 
                  markingProbs: MarkingMap, 
                  stimulusStrategy: (PRDT, MarkingMap, RNG)  => (SExpr, PRDT),
                  outfile: String = ""): (SExpr, Double, Int) =

    if (outfile != "") {
      Files.write(Paths.get(outfile), "".getBytes(), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
    }
    val topRandomValues: ArrayBuffer[(Double, PRDT)] = seeds.map(i => (0d, ParametricRandom.fromSeed(i)))
    val newSeedGen = Random(ScalaSeedNew)
    val mutGen = ScalaRandom(ScalaSeedMut)
    implicit val doubleOrdering : Ordering[(Double, PRDT)] = Ordering.by(a => a(0))
    var jBest = 0

    for (j <- 1 to NumIterations) {

      val currStimulus = topRandomValues(Math.abs(newSeedGen.nextInt()) % topRandomValues.size)(1)

      for (i <- 1 to NumMutationTrials) {
        val (expr, newStimulus) = stimulusStrategy(currStimulus, markingProbs, mutGen)
        val output: Long = expr.evaluate match
          case Some(value) => value
          case None => 0L
        val objective = output / Math.pow(1.5, expr.length)
        topRandomValues.sortInPlace()
        if (objective > topRandomValues(0)(0)) {
          topRandomValues(0) = (objective, newStimulus)
          if (objective > topRandomValues(topRandomValues.size - 1)(0)) { jBest = j }
        }
      }
      if (outfile != "") {
        topRandomValues.sortInPlace()
        Files.write(Paths.get(outfile), s"${topRandomValues(topRandomValues.size - 1)(0)},".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
      }
    }
    topRandomValues.sortInPlace()
    val (value, prdt) = topRandomValues(topRandomValues.size - 1)
    val finalPrdt = prdt.reset()
    return (SExpr.genSExprRec().generate(finalPrdt), value, jBest)

  def main(args: Array[String]): Unit =
    val random = Random(30)
    val seeds = ArrayBuffer.fill(NumSeeds)(Math.abs(random.nextInt))
    // val bestRandom = fuzzingLoop(seeds, Map((Number, 1.0), (Operator, 1.0), (Structure, 1.0)), randomStimulus)
    // println(s"Full Random: ")
    // println(s"Got ${bestRandom(1)}")
    // println(s"Expression ${bestRandom(0)}")
    // println(s"Converged in ${bestRandom(2)}")
    // println("----------------------------")
    // val bestMutate = fuzzingLoop(seeds, Map((Number, 1.0), (Operator, 1.0), (Structure, 1.0)), mutateStimulus, "./logs/sexprNaiveMutateSeries.csv")
    // println(s"Mutate: ")
    // println(s"Got ${bestMutate(1)}")
    // println(s"Expression ${bestMutate(0)}")
    // println(s"Converged in ${bestMutate(2)}")
    // println("----------------------------")

    val ptupleList: List[(Double, Double, Double)] = List((0.8,1.0,1.0), (0.9,0.6,0.7), (0.7,0.4,0.4), (0.2,0.6,0.9), (0.7,0.6,1.0))
    ptupleList.zipWithIndex.foreach((ptuple, i) => {
      val (p1, p2, p3) = ptuple
      fuzzingLoop(seeds, Map((DecisionType.Number, p1), (DecisionType.Operator, p2), (DecisionType.Structure, p3)), markMutateStimulus, s"./logs/sexpr${i * 25}PercentileMutateSeries.csv") 
    })
    // val bestMarkMutate = fuzzingLoop(seeds, Map((Number, 1.0), (Operator, 1.0), (Structure, 1.0)), markMutateStimulus)
    // println(s"Marked Mutate: ")
    // println(s"Got ${bestMarkMutate(1)}")
    // println(s"Expression ${bestMarkMutate(0)}")
    // println(s"Converged in ${bestMarkMutate(2)}")
    // println("----------------------------")

    // Files.write(Paths.get("./logs/bestProbSExprLog.txt"), "".getBytes(), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)

    // val Steps = 10
    // val StepSize = 1.0/Steps
    // var best = Double.MinValue
    // var argBest = (0.0, 0.0, 0.0)
    // for (p1 <- 1 to Steps by 1) {
    //   for (p2 <- 1 to Steps by 1) {
    //     for (p3 <- 1 to Steps by 1) {
    //       val random = Random(10)
    //       val seeds = ArrayBuffer.fill(NumSeeds)(Math.abs(random.nextInt))
    //       val out = fuzzingLoop(seeds, Map((DecisionType.Number, p1 * StepSize),
    //                                     (DecisionType.Operator, p2 * StepSize),
    //                                     (DecisionType.Structure, p3 * StepSize)), markMutateStimulus)
    //       Files.write(Paths.get("./logs/bestProbSExprLog.txt"), s"Probabilities ${(p1 * StepSize, p2 * StepSize, p3 * StepSize)} achieve value ${out(1)} in ${out(2)} steps \n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)   
    //       if (out(1) > best) {
    //         best = out(1)
    //         argBest = (p1 * StepSize, p2 * StepSize, p3 * StepSize)
    //       }
    //     }
    //   }
    // }
    // println(s"Best probabilities $argBest achieve value $best") 