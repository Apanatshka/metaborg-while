package org.metaborg.lang.whilelang.ast

import org.metaborg.lang.whilelang.ast.MCommon.SID
import org.metaborg.lang.whilelang.ast.MExpr.SExpr
import org.metaborg.lang.whilelang.ast.MExpr.SExpr._
import org.metaborg.lang.whilelang.ast.MStatement.SLabeledStatement.{LabeledAssign3, LabeledIfThenElse4, LabeledSkip1, LabeledWhile3}
import org.metaborg.lang.whilelang.ast.MStatement.SStatement._
import org.metaborg.lang.whilelang.ast.MStatement.{SLabeledStatement, SStatement}
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart.{Labeled1, While1}
import org.metaborg.popa.mfp.{IntraControlFlow, SingleICF}
import org.metaborg.scalaterms.Origin

/**
  * Created by jeff on 28/11/16.
  */
object Utils {

  def labelToAstMap(labeledstatement: SLabeledStatement): Map[Int, SLabeledStatement] = labeledstatement match {
    case LabeledAssign3(id1, expr2, int3, origin) => Map((int3, labeledstatement))
    case LabeledSkip1(int1, origin) => Map((int1, labeledstatement))
    case SLabeledStatement.Seq2(labeledstatement1,
                                labeledstatement2,
                                origin) => labelToAstMap(labeledstatement1) ++ labelToAstMap(labeledstatement2)
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) => labelToAstMap(
      labeledstatement3) ++ labelToAstMap(labeledstatement4) + ((int2, labeledstatement))
    case LabeledWhile3(expr1,
                       int2,
                       labeledstatement3,
                       origin) => labelToAstMap(labeledstatement3) + ((int2, labeledstatement))
  }

  def labelToAstMap(labeledAst: Labeled1): Map[Int, SLabeledStatement] = labelToAstMap(labeledAst.labeledstatement1)

  def collectExprs(s: SStart): Set[SExpr] = s match {
    case While1(stmt, _) => collectExprs(stmt)
    case Labeled1(stmt, _) => collectExprs(stmt)
  }

  def collectExprs(stmt: SStatement): Set[SExpr] = stmt match {
    case Assign2(id1, expr2, origin) => collectExprs(expr2) + expr2
    case Skip0(origin) => Set.empty
    case Seq2(statement1, statement2, origin) => collectExprs(statement2) union collectExprs(statement1)
    case IfThenElse3(expr1, statement2, statement3, origin) =>
      collectExprs(statement3) union collectExprs(statement2) union collectExprs(expr1)
    case While2(expr1, statement2, origin) => collectExprs(statement2) union collectExprs(expr1)
  }

  def collectExprs(stmt: SLabeledStatement): Set[SExpr] = stmt match {
    case LabeledAssign3(id1, expr2, int3, origin) => collectExprs(expr2)
    case LabeledSkip1(int1, origin) => Set.empty
    case SLabeledStatement.Seq2(statement1, statement2, origin) =>
      collectExprs(statement2) union collectExprs(statement1)
    case LabeledIfThenElse4(expr1, int2, statement3, statement4, origin) =>
      collectExprs(statement4) union collectExprs(statement3) union collectExprs(expr1)
    case LabeledWhile3(expr1, int2, statement3, origin) => collectExprs(statement3) union collectExprs(expr1)
  }

  def collectExprs(expr: SExpr): Set[SExpr] = expr match {
    case Ref1(id1, origin) => Set(expr)
    case Num1(int1, origin) => Set(expr)
    case Add2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Sub2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Mul2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Div2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case True0(origin) => Set(expr)
    case False0(origin) => Set(expr)
    case Not1(expr1, origin) => collectExprs(expr1) + expr
    case And2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Or2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Eq2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Gt2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Gte2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Lt2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
    case Lte2(expr1, expr2, origin) => collectExprs(expr1) union collectExprs(expr2) + expr
  }

  def collectRefs(expr: SExpr): Set[String] = expr match {
    case Ref1(id1, origin) => Set(id1.string)
    case Num1(int1, origin) => Set.empty
    case Add2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Sub2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Mul2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Div2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case True0(origin) => Set.empty
    case False0(origin) => Set.empty
    case Not1(expr1, origin) => collectRefs(expr1)
    case And2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Or2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Eq2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Gt2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Gte2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Lt2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
    case Lte2(expr1, expr2, origin) => collectRefs(expr1) union collectRefs(expr2)
  }

  def flow(labeled1: Labeled1): IntraControlFlow[Int] = flow(labeled1.labeledstatement1)

  def flow(labeledStatement: SLabeledStatement): IntraControlFlow[Int] = labeledStatement match {
    case LabeledAssign3(id1, expr2, int3, origin) => new SingleICF[Int](int3)
    case LabeledSkip1(int1, origin) => new SingleICF[Int](int1)
    case SLabeledStatement.Seq2(labeledstatement1, labeledstatement2, origin) =>
      flow(labeledstatement1).andThen(flow(labeledstatement2))
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) =>
      flow(labeledstatement3).branch(int2, flow(labeledstatement4))
    case LabeledWhile3(expr1, int2, labeledstatement3, origin) =>
      flow(labeledstatement3).pushFront(int2).loop
  }

  def label(sStart: SStart): Labeled1 = sStart match {
    case While1(statement1, origin) => Labeled1(label(statement1)._1, origin)
    case labeled: Labeled1 => labeled
  }

  def label(sStatement: SStatement)(implicit counter: Int = 1): (SLabeledStatement, Int) = sStatement match {
    case Assign2(id1, expr2, origin) => (LabeledAssign3(id1, expr2, counter, origin), counter + 1)
    case Skip0(origin) => (LabeledSkip1(counter, origin), counter + 1)
    case Seq2(statement1, statement2, origin) =>
      val (labeledStatement1, counter2) = label(statement1)
      val (labeledStatement2, counter3) = label(statement2)(counter2)
      (SLabeledStatement.Seq2(labeledStatement1, labeledStatement2, origin), counter3)
    case IfThenElse3(expr1, statement2, statement3, origin) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      val (labeledStatement3, counter3) = label(statement3)(counter2)
      (LabeledIfThenElse4(expr1, counter, labeledStatement2, labeledStatement3, origin), counter3)
    case While2(expr1, statement2, origin) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      (LabeledWhile3(expr1, counter, labeledStatement2, origin), counter2)
  }

  def labelToOriginMap(labeled1: Labeled1): Map[Int, Origin] = labelToOriginMap(labeled1.labeledstatement1)

  def labelToOriginMap(labeledStatement: SLabeledStatement): Map[Int, Origin] = labeledStatement match {
    case LabeledAssign3(id1, expr2, int3, origin) => Map((int3, origin))
    case LabeledSkip1(int1, origin) => Map((int1, origin))
    case SLabeledStatement.Seq2(labeledstatement1,
                                labeledstatement2,
                                origin) => labelToOriginMap(labeledstatement1) ++ labelToOriginMap(labeledstatement2)
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) => labelToOriginMap(
      labeledstatement3) ++ labelToOriginMap(labeledstatement4) + ((int2, origin))
    case LabeledWhile3(expr1, int2, labeledstatement3, origin) => labelToOriginMap(labeledstatement3) + ((int2, origin))
  }
}
