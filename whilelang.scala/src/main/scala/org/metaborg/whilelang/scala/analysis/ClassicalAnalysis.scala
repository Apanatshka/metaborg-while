package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{IntraControlFlow, IntraMaximalFixedPoint, SetLattice}
import org.metaborg.scalaterms.spoofax.AnalysisResult

/**
  * Classical analysis, with gen/kill transfer function etc.
  */
trait ClassicalAnalysis[Label, Prop, SStart, RelevantASTNodes] {
  def labelMap(ast: SStart): Map[Label, RelevantASTNodes]

  def lattice(ast: SStart): SetLattice[Prop]

  def flow(ast: SStart): IntraControlFlow[Label]

  def extremalValue(ast: SStart): Set[Prop]

  def run(ast: SStart): AnalysisResult

  def kill(nodes: RelevantASTNodes, property: Set[Prop]): Set[Prop]

  def gen(nodes: RelevantASTNodes): Set[Prop]

  def analyze(ast: SStart): Map[Label, (Set[Prop], Set[Prop])] = {
    val labelMap: Map[Label, RelevantASTNodes] = this.labelMap(ast)

    val intraMaximalFixedPoint =
      new IntraMaximalFixedPoint[Label, Set[Prop]](lattice(ast),
                                                   ClassicalAnalysis.transfer(kill(labelMap), gen(labelMap)),
                                                   flow(ast),
                                                   extremalValue(ast))
    intraMaximalFixedPoint.result
  }

  def kill(labelMap: Map[Label, RelevantASTNodes])(label: Label, property: Set[Prop]): Set[Prop] =
    kill(labelMap(label), property)

  def gen(labelMap: Map[Label, RelevantASTNodes])(label: Label): Set[Prop] =
    gen(labelMap(label))

  def prepareProperties(res: (Set[Prop], Set[Prop])): (String, String) =
    (prepareProperty(res._1), prepareProperty(res._2))

  def prepareProperty(property: Set[Prop]): String = property.map(prepareProp).mkString("{", ", ", "}")

  def prepareProp(prop: Prop): String = prop.toString
}

object ClassicalAnalysis {
  def transfer[Label, Prop](kill: (Label, Set[Prop]) => Set[Prop], gen: Label => Set[Prop])
                           (label: Label, prop: Set[Prop]): Set[Prop] = {
    prop.diff(kill(label, prop)).union(gen(label))
  }
}
