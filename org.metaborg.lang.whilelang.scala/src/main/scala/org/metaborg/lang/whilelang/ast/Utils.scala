package org.metaborg.lang.whilelang.ast

import org.metaborg.lang.whilelang.ast.MCommon.SINT
import org.metaborg.lang.whilelang.ast.MExpr.SExpr
import org.metaborg.lang.whilelang.ast.MExpr.SExpr._
import org.metaborg.lang.whilelang.ast.MStatement.SLabeledStatement.{LabeledAssign3, LabeledIfThenElse4, LabeledSkip1, LabeledWhile3}
import org.metaborg.lang.whilelang.ast.MStatement.SStatement._
import org.metaborg.lang.whilelang.ast.MStatement.{SLabeledStatement, SStatement}
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart.{Labeled1, While1}
import org.metaborg.popa.mfp.IntraControlFlow
import org.metaborg.scalaterms.Origin

/**
  * Traversals over de AST to collect interesting things
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

  def collectAExprs(s: SStart): Set[SExpr] = s match {
    case While1(stmt, _) => collectAExprs(stmt)
    case Labeled1(stmt, _) => collectAExprs(stmt)
  }

  def collectAExprs(stmt: SStatement): Set[SExpr] = stmt match {
    case Assign2(id1, expr2, origin) => Set(expr2)
    case Skip0(origin) => Set.empty
    case Seq2(statement1, statement2, origin) => collectAExprs(statement2) union collectAExprs(statement1)
    case IfThenElse3(expr1, statement2, statement3, origin) =>
      collectAExprs(statement3) union collectAExprs(statement2) union collectAExprs(expr1)
    case While2(expr1, statement2, origin) => collectAExprs(statement2) union collectAExprs(expr1)
  }

  def collectAExprs(stmt: SLabeledStatement): Set[SExpr] = stmt match {
    case LabeledAssign3(id1, expr2, int3, origin) => Set(expr2)
    case LabeledSkip1(int1, origin) => Set.empty
    case SLabeledStatement.Seq2(statement1, statement2, origin) =>
      collectAExprs(statement2) union collectAExprs(statement1)
    case LabeledIfThenElse4(expr1, int2, statement3, statement4, origin) =>
      collectAExprs(statement4) union collectAExprs(statement3) union collectAExprs(expr1)
    case LabeledWhile3(expr1, int2, statement3, origin) => collectAExprs(statement3) union collectAExprs(expr1)
  }

  def collectAExprs(expr: SExpr): Set[SExpr] = expr match {
    case Ref1(_, _) |
         Num1(_, _) |
         True0(_) |
         False0(_) => Set.empty
    case Add2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Sub2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Mul2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Div2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Not1(expr1, origin) => collectAExprs(expr1) + expr
    case And2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Or2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Eq2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Gt2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Gte2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Lt2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
    case Lte2(expr1, expr2, origin) => collectAExprs(expr1) union collectAExprs(expr2) + expr
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
    case LabeledAssign3(id1, expr2, int3, origin) => IntraControlFlow.single(int3)
    case LabeledSkip1(int1, origin) => IntraControlFlow.single(int1)
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
    case Assign2(id1, expr2, o) => (LabeledAssign3(id1, expr2, SINT(counter.toString, o), o), counter + 1)
    case Skip0(o) => (LabeledSkip1(SINT(counter.toString, o), o), counter + 1)
    case Seq2(statement1, statement2, o) =>
      val (labeledStatement1, counter2) = label(statement1)
      val (labeledStatement2, counter3) = label(statement2)(counter2)
      (SLabeledStatement.Seq2(labeledStatement1, labeledStatement2, o), counter3)
    case IfThenElse3(expr1, statement2, statement3, o) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      val (labeledStatement3, counter3) = label(statement3)(counter2)
      (LabeledIfThenElse4(expr1, SINT(counter.toString, o), labeledStatement2, labeledStatement3, o), counter3)
    case While2(expr1, statement2, o) =>
      val (labeledStatement2, counter2) = label(statement2)(counter + 1)
      (LabeledWhile3(expr1, SINT(counter.toString, o), labeledStatement2, o), counter2)
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

  def toString(expr: SExpr): String = expr match {
    case Ref1(id1, origin) => id1.string
    case Num1(int1, origin) => int1.string
    case Add2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } + ${ Utils.toString(expr2) }"
    case Sub2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } - ${ Utils.toString(expr2) }"
    case Mul2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } * ${ Utils.toString(expr2) }"
    case Div2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } / ${ Utils.toString(expr2) }"
    case True0(origin) => "true"
    case False0(origin) => "false"
    case Not1(expr1, origin) => s"!${ Utils.toString(expr1) }"
    case And2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } && ${ Utils.toString(expr2) }"
    case Or2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } || ${ Utils.toString(expr2) }"
    case Eq2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } == ${ Utils.toString(expr2) }"
    case Gt2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } > ${ Utils.toString(expr2) }"
    case Gte2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } >= ${ Utils.toString(expr2) }"
    case Lt2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } < ${ Utils.toString(expr2) }"
    case Lte2(expr1, expr2, origin) => s"${ Utils.toString(expr1) } <= ${ Utils.toString(expr2) }"
  }

  def equals(expr1: SExpr, expr2: SExpr): Boolean = (expr1, expr2) match {
    case (Ref1(id1, _), Ref1(id2, _)) => id1.string == id2.string
    case (Num1(int1, _), Num1(int2, _)) => int1.string == int2.string
    case (True0(_), True0(_)) => true
    case (False0(_), False0(_)) => true
    case (Not1(e1, _), Not1(e2, _)) => equals(e1, e2)
    case (Add2(e1, e2, _), Add2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Sub2(e1, e2, _), Sub2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Mul2(e1, e2, _), Mul2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Div2(e1, e2, _), Div2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (And2(e1, e2, _), And2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Or2(e1, e2, _), Or2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Eq2(e1, e2, _), Eq2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Gt2(e1, e2, _), Gt2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Gte2(e1, e2, _), Gte2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Lt2(e1, e2, _), Lt2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case (Lte2(e1, e2, _), Lte2(e3, e4, _)) => equals(e1, e3) && equals(e2, e4)
    case _ => false
  }

  def hashCode(expr: SExpr): Int = expr match {
    case Ref1(id1, origin) => Seq(1, id1.string).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Num1(int1, origin) => Seq(2, int1.string).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Add2(expr1, expr2, origin) => Seq(3, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Sub2(expr1, expr2, origin) => Seq(4, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Mul2(expr1, expr2, origin) => Seq(5, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Div2(expr1, expr2, origin) => Seq(6, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case True0(origin) => 7
    case False0(origin) => 8
    case Not1(expr1, origin) => Seq(9, expr1).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case And2(expr1, expr2, origin) => Seq(10, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Or2(expr1, expr2, origin) => Seq(11, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Eq2(expr1, expr2, origin) => Seq(12, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Gt2(expr1, expr2, origin) => Seq(13, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Gte2(expr1, expr2, origin) => Seq(14, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Lt2(expr1, expr2, origin) => Seq(15, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    case Lte2(expr1, expr2, origin) => Seq(16, Utils.hashCode(expr1), Utils.hashCode(expr2)).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
