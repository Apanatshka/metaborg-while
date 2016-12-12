package org.metaborg.whilelang.scala.analysis

import org.metaborg.whilelang.scala.ast.MExpr.SExpr
import org.metaborg.whilelang.scala.ast.MExpr.SExpr._

import scala.language.implicitConversions
import scala.math.PartiallyOrdered

/**
  * The values space of the constants in `ConstantPropagation`
  */
sealed trait Constant extends PartiallyOrdered[Constant] {
  override def tryCompareTo[B >: Constant](that: B)
                                          (implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = {
    try {
      val left = this
      val right = that.asInstanceOf[Constant]
      (left, right) match {
        case (Top, Top) => Some(0) // top == top
        case (_, Top) => Some(-1) // value <= top
        case (Top, _) => Some(1) // top >= value
        case (CInt(l), CInt(r)) if l == r => Some(0) // left == right
        case (CBool(l), CBool(r)) if l == r => Some(0) // left == right
        case _ => None // otherwise incomparable
      }
    } catch {
      case _: ClassCastException => None
    }
  }
}

case class CInt(value: Int) extends Constant {
  override def toString: String = value.toString
}

case class CBool(value: Boolean) extends Constant {
  override def toString: String = value.toString
}

case object Top extends Constant {
  override def toString: String = "top"
}

object Constant {
  def lub(c1: Constant, c2: Constant): Constant = if (c1 == c2) c1 else Top

  def glb(c1: Constant, c2: Constant): Option[Constant] = if (c1 == c2) Some(c1) else None

  def constEvaluate(expr: SExpr)(implicit mapping: Map[String, Constant]): Constant = expr match {
    case Ref1(id1) => mapping(id1.string)
    case Num1(int1) => CInt(int1)
    case Add2(expr1, expr2) => liftArith(_ + _)(expr1, expr2)
    case Sub2(expr1, expr2) => liftArith(_ - _)(expr1, expr2)
    case Mul2(expr1, expr2) => liftArith(_ * _)(expr1, expr2)
    case Div2(expr1, expr2) => liftArith(_ / _)(expr1, expr2)
    case True0() => CBool(true)
    case False0() => CBool(false)
    case Not1(expr1) => constEvaluate(expr1) match {
      case CBool(b) => CBool(!b);
      case _ => Top
    }
    case And2(expr1, expr2) => liftBool(_ && _)(expr1, expr2)
    case Or2(expr1, expr2) => liftBool(_ || _)(expr1, expr2)
    case Eq2(expr1, expr2) => liftComp(_ == _)(expr1, expr2)
    case Gt2(expr1, expr2) => liftComp(_ > _)(expr1, expr2)
    case Gte2(expr1, expr2) => liftComp(_ >= _)(expr1, expr2)
    case Lt2(expr1, expr2) => liftComp(_ < _)(expr1, expr2)
    case Lte2(expr1, expr2) => liftComp(_ <= _)(expr1, expr2)
  }

  def liftArith(arithFun: (Int, Int) => Int)
               (e1: SExpr, e2: SExpr)
               (implicit mapping: Map[String, Constant]): Constant = (constEvaluate(e1), constEvaluate(e2)) match {
    case (CInt(l), CInt(r)) => CInt(arithFun(l, r))
    case _ => Top
  }

  def liftBool(boolFun: (Boolean, Boolean) => Boolean)
              (e1: SExpr, e2: SExpr)
              (implicit mapping: Map[String, Constant]): Constant = (constEvaluate(e1), constEvaluate(e2)) match {
    case (CBool(l), CBool(r)) => CBool(boolFun(l, r))
    case _ => Top
  }

  def liftComp(compFun: (Int, Int) => Boolean)
              (e1: SExpr, e2: SExpr)
              (implicit mapping: Map[String, Constant]): Constant = (constEvaluate(e1), constEvaluate(e2)) match {
    case (CInt(l), CInt(r)) => CBool(compFun(l, r))
    case _ => Top
  }
}
