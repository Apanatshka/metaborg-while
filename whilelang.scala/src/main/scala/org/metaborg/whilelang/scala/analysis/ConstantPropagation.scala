package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{BasicAnalysis, IntraControlFlow, Lattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax.{AnalysisResult, EditorMessage}
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.Labeled1
import org.metaborg.whilelang.scala.ast.Utils

/**
  * Constant propagate as an example of a non-distributive analysis
  */
object ConstantPropagation extends BasicAnalysis {
  type Label = Int
  type Property = Option[Map[String, Constant]]
  type Start = Labeled1
  type RelevantNode = SLabeledStatement
  type Result = AnalysisResult

  override def flow(ast: Labeled1): IntraControlFlow[Label] = Utils.flow(ast)

  override def extremalValue(ast: Labeled1): Property = Some(Map.empty.withDefaultValue(Top))

  override def labelMap(ast: Labeled1): Map[Label, RelevantNode] = Utils.labelToAstMap(ast)

  override def lattice(ast: Labeled1): Lattice[Property] = new Lattice[Property] {
    override val top: Property = Some(Map.empty.withDefaultValue(Top))
    override val bottom: Property = None

    override def lte(one: Property,
                     other: Property): Boolean = (one, other) match {
      case (Some(l), Some(r)) => l.forall({ case (k, v) => r.contains(k) && v <= r(k) })
      case (None, _) => true
      case _ => false
    }

    override def glb(one: Property,
                     other: Property): Property = (one, other) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(l), Some(r)) =>
        val glbs = l.keySet.union(r.keySet).toSeq.map(k => Constant.glb(l(k), r(k)).map((k, _)))
        if (glbs.forall(_.nonEmpty)) {
          Some(glbs.map(_.get).toMap.withDefaultValue(Top))
        } else {
          None
        }
    }

    override def lub(one: Property,
                     other: Property): Property = (one, other) match {
      case (None, other_) => other_
      case (one_, None) => one_
      case (Some(l), Some(r)) => Some(l.keySet.union(r.keySet).map(k => (k, Constant.lub(l(k), r(k)))).toMap
                                        .withDefaultValue(Top))
    }
  }

  override def run(ast: Labeled1): AnalysisResult = {
    val analysisResult = analyze(ast)
    val labelToOrigin: Map[Label, Origin] = Utils.labelToOriginMap(ast)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  override def transfer(node: SLabeledStatement, property: Property): Property = node match {
    case LabeledAssign3(id1, expr2, int3) => property.map { mapping =>
      mapping.updated(id1.string, Constant.constEvaluate(expr2)(mapping))
    }
    case LabeledSkip1(int1) => property
    case Seq2(labeledstatement1, labeledstatement2) => property
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => property
    case LabeledWhile3(expr1, int2, labeledstatement3) => property
  }

  override def prepareProperty(property: Option[Map[String, Constant]]): String = property match {
    case None => "⊥"
    case Some(m) => if (m.values.forall(_ == Top)) "⊤" else m.mkString("[", ";", "]")
  }
}
