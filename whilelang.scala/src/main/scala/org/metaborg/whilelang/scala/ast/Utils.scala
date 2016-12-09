package org.metaborg.whilelang.scala.ast

import org.metaborg.popa.mfp.IntraControlFlow
import org.metaborg.scalaterms.Origin
import org.metaborg.whilelang.scala.ast.MCommon.SINT
import org.metaborg.whilelang.scala.ast.MExpr.SExpr
import org.metaborg.whilelang.scala.ast.MExpr.SExpr._
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement.{LabeledAssign3, LabeledIfThenElse4, LabeledSkip1, LabeledWhile3}
import org.metaborg.whilelang.scala.ast.MStatement.SStatement._
import org.metaborg.whilelang.scala.ast.MStatement.{SLabeledStatement, SStatement}
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.{Labeled1, While1}

/**
  * Traversals over de AST to collect interesting things
  */
object Utils {
  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[((A, B))] = for {a <- as; b <- bs} yield (a, b)

  def collectRefs(labeledstatement: SLabeledStatement): Set[String] = labeledstatement match {
    case LabeledAssign3(id1, expr2, int3) => collectRefs(expr2)
    case LabeledSkip1(int1) => Set.empty
    case SLabeledStatement.Seq2(labeledstatement1,
                                labeledstatement2) => collectRefs(labeledstatement1) union collectRefs(labeledstatement2)
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => collectRefs(expr1) union collectRefs(
      labeledstatement3) union collectRefs(labeledstatement3)
    case LabeledWhile3(expr1, int2, labeledstatement3) => collectRefs(expr1) union collectRefs(labeledstatement3)
  }

  def collectRefs(labeled: Labeled1): Set[String] = collectRefs(labeled.labeledstatement1)

  def labelToAstMap(labeledstatement: SLabeledStatement): Map[Int, SLabeledStatement] = labeledstatement match {
    case LabeledAssign3(id1, expr2, int3) => Map((int3, labeledstatement))
    case LabeledSkip1(int1) => Map((int1, labeledstatement))
    case SLabeledStatement.Seq2(labeledstatement1,
                                labeledstatement2) => labelToAstMap(labeledstatement1) ++ labelToAstMap(
      labeledstatement2)
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => labelToAstMap(
      labeledstatement3) ++ labelToAstMap(labeledstatement4) + ((int2, labeledstatement))
    case LabeledWhile3(expr1,
                       int2,
                       labeledstatement3) => labelToAstMap(labeledstatement3) + ((int2, labeledstatement))
  }

  def labelToAstMap(labeledAst: Labeled1): Map[Int, SLabeledStatement] = labelToAstMap(labeledAst.labeledstatement1)

  def collectAExprs(s: SStart): Set[SExpr] = s match {
    case While1(stmt) => collectAExprs(stmt)
    case Labeled1(stmt) => collectAExprs(stmt)
  }

  def collectAExprs(stmt: SStatement): Set[SExpr] = stmt match {
    case Assign2(id1, expr2) => Set(expr2)
    case Skip0() => Set.empty
    case Seq2(statement1, statement2) => collectAExprs(statement2) union collectAExprs(statement1)
    case IfThenElse3(expr1, statement2, statement3) =>
      collectAExprs(statement3) union collectAExprs(statement2) union collectAExprs(expr1)
    case While2(expr1, statement2) => collectAExprs(statement2) union collectAExprs(expr1)
  }

  def collectAExprs(stmt: SLabeledStatement): Set[SExpr] = stmt match {
    case LabeledAssign3(id1, expr2, int3) => Set(expr2)
    case LabeledSkip1(int1) => Set.empty
    case SLabeledStatement.Seq2(statement1, statement2) =>
      collectAExprs(statement2) union collectAExprs(statement1)
    case LabeledIfThenElse4(expr1, int2, statement3, statement4) =>
      collectAExprs(statement4) union collectAExprs(statement3) union collectAExprs(expr1)
    case LabeledWhile3(expr1, int2, statement3) => collectAExprs(statement3) union collectAExprs(expr1)
  }

  def collectAExprs(expr: SExpr): Set[SExpr] = expr match {
    case Ref1(_) |
         Num1(_) |
         True0() |
         False0() => Set.empty
    case Add2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Sub2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Mul2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Div2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Not1(expr1) => collectAExprs(expr1) + expr
    case And2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Or2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Eq2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Gt2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Gte2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Lt2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Lte2(expr1, expr2) => collectAExprs(expr1) union collectAExprs(expr2) + expr
  }

  def collectRefs(expr: SExpr): Set[String] = expr match {
    case Ref1(id1) => Set(id1.string)
    case Num1(int1) => Set.empty
    case Add2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Sub2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Mul2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Div2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case True0() => Set.empty
    case False0() => Set.empty
    case Not1(expr1) => collectRefs(expr1)
    case And2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Or2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Eq2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Gt2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Gte2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Lt2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
    case Lte2(expr1, expr2) => collectRefs(expr1) union collectRefs(expr2)
  }

  def flow(labeled1: Labeled1): IntraControlFlow[Int] = flow(labeled1.labeledstatement1)

  def flow(labeledStatement: SLabeledStatement): IntraControlFlow[Int] = labeledStatement match {
    case LabeledAssign3(id1, expr2, int3) => IntraControlFlow.single(int3)
    case LabeledSkip1(int1) => IntraControlFlow.single(int1)
    case SLabeledStatement.Seq2(labeledstatement1, labeledstatement2) =>
      flow(labeledstatement1).andThen(flow(labeledstatement2))
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) =>
      flow(labeledstatement3).branch(int2, flow(labeledstatement4))
    case LabeledWhile3(expr1, int2, labeledstatement3) =>
      flow(labeledstatement3).pushFront(int2).loop
  }

  def label(sStart: SStart): Labeled1 = sStart match {
    case While1(statement1) => Labeled1(label(statement1)._1)(sStart.origin)
    case labeled: Labeled1 => labeled
  }

  def label(sStatement: SStatement)(implicit counter: Int = 1): (SLabeledStatement, Int) = sStatement match {
    case Assign2(id1, expr2) => (LabeledAssign3(id1, expr2, SINT(counter.toString)(sStatement.origin))(sStatement
                                                                                                         .origin), counter + 1)
    case Skip0() => (LabeledSkip1(SINT(counter.toString)(sStatement.origin))(sStatement.origin), counter + 1)
    case Seq2(statement1, statement2) =>
      val (labeledStatement1, counter2) = label(statement1)
      val (labeledStatement2, counter3) = label(statement2)(counter2)
      (SLabeledStatement.Seq2(labeledStatement1, labeledStatement2)(sStatement.origin), counter3)
    case IfThenElse3(expr1, statement2, statement3) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      val (labeledStatement3, counter3) = label(statement3)(counter2)
      (LabeledIfThenElse4(expr1, SINT(counter.toString)(sStatement.origin), labeledStatement2, labeledStatement3)(
        sStatement.origin), counter3)
    case While2(expr1, statement2) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      (LabeledWhile3(expr1, SINT(counter.toString)(sStatement.origin), labeledStatement2)(sStatement.origin), counter2)
  }

  def labelToOriginMap(labeled1: Labeled1): Map[Int, Origin] = labelToOriginMap(labeled1.labeledstatement1)

  def labelToOriginMap(labeledStatement: SLabeledStatement): Map[Int, Origin] = labeledStatement match {
    case LabeledAssign3(id1, expr2, int3) => Map((int3, labeledStatement.origin))
    case LabeledSkip1(int1) => Map((int1, labeledStatement.origin))
    case SLabeledStatement.Seq2(labeledstatement1,
                                labeledstatement2) => labelToOriginMap(labeledstatement1) ++ labelToOriginMap(
      labeledstatement2)
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => labelToOriginMap(
      labeledstatement3) ++ labelToOriginMap(labeledstatement4) + ((int2, labeledStatement.origin))
    case LabeledWhile3(expr1, int2, labeledstatement3) => labelToOriginMap(labeledstatement3) + ((int2, labeledStatement
      .origin))
  }

  def toString(expr: SExpr): String = expr match {
    case Ref1(id1) => id1.string
    case Num1(int1) => int1.string
    case Add2(expr1, expr2) => s"${ Utils.toString(expr1) } + ${ Utils.toString(expr2) }"
    case Sub2(expr1, expr2) => s"${ Utils.toString(expr1) } - ${ Utils.toString(expr2) }"
    case Mul2(expr1, expr2) => s"${ Utils.toString(expr1) } * ${ Utils.toString(expr2) }"
    case Div2(expr1, expr2) => s"${ Utils.toString(expr1) } / ${ Utils.toString(expr2) }"
    case True0() => "true"
    case False0() => "false"
    case Not1(expr1) => s"!${ Utils.toString(expr1) }"
    case And2(expr1, expr2) => s"${ Utils.toString(expr1) } && ${ Utils.toString(expr2) }"
    case Or2(expr1, expr2) => s"${ Utils.toString(expr1) } || ${ Utils.toString(expr2) }"
    case Eq2(expr1, expr2) => s"${ Utils.toString(expr1) } == ${ Utils.toString(expr2) }"
    case Gt2(expr1, expr2) => s"${ Utils.toString(expr1) } > ${ Utils.toString(expr2) }"
    case Gte2(expr1, expr2) => s"${ Utils.toString(expr1) } >= ${ Utils.toString(expr2) }"
    case Lt2(expr1, expr2) => s"${ Utils.toString(expr1) } < ${ Utils.toString(expr2) }"
    case Lte2(expr1, expr2) => s"${ Utils.toString(expr1) } <= ${ Utils.toString(expr2) }"
  }
}
