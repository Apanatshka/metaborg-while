package org.metaborg.lang.whilelang.ast

object MExpr {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.lang.whilelang.ast.MCommon._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SExpr extends sdf.Constructor
  // Constructor definitions
  object SExpr extends scalaterms.TermLikeCompanion[SExpr] {
    override val fromSTerm: scalaterms.FromSTerm[SExpr] = new scalaterms.FromSTerm[SExpr] {
      override def unapply(term: STerm): Option[SExpr] = term match {
        case Ref1.fromSTerm(expr1) => scala.Some(expr1)
        case Num1.fromSTerm(expr1) => scala.Some(expr1)
        case Add2.fromSTerm(expr1) => scala.Some(expr1)
        case Sub2.fromSTerm(expr1) => scala.Some(expr1)
        case Mul2.fromSTerm(expr1) => scala.Some(expr1)
        case Div2.fromSTerm(expr1) => scala.Some(expr1)
        case True0.fromSTerm(expr1) => scala.Some(expr1)
        case False0.fromSTerm(expr1) => scala.Some(expr1)
        case Not1.fromSTerm(expr1) => scala.Some(expr1)
        case And2.fromSTerm(expr1) => scala.Some(expr1)
        case Or2.fromSTerm(expr1) => scala.Some(expr1)
        case Eq2.fromSTerm(expr1) => scala.Some(expr1)
        case Gt2.fromSTerm(expr1) => scala.Some(expr1)
        case Gte2.fromSTerm(expr1) => scala.Some(expr1)
        case Lt2.fromSTerm(expr1) => scala.Some(expr1)
        case Lte2.fromSTerm(expr1) => scala.Some(expr1)
        case _ => scala.None
      }
    }

    case class Ref1(id1: SID, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Ref", scala.List(id1.toSTerm), origin)
    }
    object Ref1 extends scalaterms.TermLikeCompanion[Ref1] {
      override val fromSTerm: scalaterms.FromSTerm[Ref1] = new scalaterms.FromSTerm[Ref1] {
        override def unapply(term: STerm): Option[Ref1] = term match {
          case STerm.Cons("Ref", scala.List(SID.fromSTerm(id1)), o) => scala.Some(Ref1(id1, o))
          case _ => None
        }
      }
    }
    case class Num1(int1: SINT, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Num", scala.List(int1.toSTerm), origin)
    }
    object Num1 extends scalaterms.TermLikeCompanion[Num1] {
      override val fromSTerm: scalaterms.FromSTerm[Num1] = new scalaterms.FromSTerm[Num1] {
        override def unapply(term: STerm): Option[Num1] = term match {
          case STerm.Cons("Num", scala.List(SINT.fromSTerm(int1)), o) => scala.Some(Num1(int1, o))
          case _ => None
        }
      }
    }
    case class Add2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Add", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Add2 extends scalaterms.TermLikeCompanion[Add2] {
      override val fromSTerm: scalaterms.FromSTerm[Add2] = new scalaterms.FromSTerm[Add2] {
        override def unapply(term: STerm): Option[Add2] = term match {
          case STerm.Cons("Add", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Add2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Sub2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Sub", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Sub2 extends scalaterms.TermLikeCompanion[Sub2] {
      override val fromSTerm: scalaterms.FromSTerm[Sub2] = new scalaterms.FromSTerm[Sub2] {
        override def unapply(term: STerm): Option[Sub2] = term match {
          case STerm.Cons("Sub", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Sub2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Mul2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Mul", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Mul2 extends scalaterms.TermLikeCompanion[Mul2] {
      override val fromSTerm: scalaterms.FromSTerm[Mul2] = new scalaterms.FromSTerm[Mul2] {
        override def unapply(term: STerm): Option[Mul2] = term match {
          case STerm.Cons("Mul", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Mul2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Div2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Div", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Div2 extends scalaterms.TermLikeCompanion[Div2] {
      override val fromSTerm: scalaterms.FromSTerm[Div2] = new scalaterms.FromSTerm[Div2] {
        override def unapply(term: STerm): Option[Div2] = term match {
          case STerm.Cons("Div", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Div2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class True0(origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("True", scala.List(), origin)
    }
    object True0 extends scalaterms.TermLikeCompanion[True0] {
      override val fromSTerm: scalaterms.FromSTerm[True0] = new scalaterms.FromSTerm[True0] {
        override def unapply(term: STerm): Option[True0] = term match {
          case STerm.Cons("True", scala.List(), o) => scala.Some(True0(o))
          case _ => None
        }
      }
    }
    case class False0(origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("False", scala.List(), origin)
    }
    object False0 extends scalaterms.TermLikeCompanion[False0] {
      override val fromSTerm: scalaterms.FromSTerm[False0] = new scalaterms.FromSTerm[False0] {
        override def unapply(term: STerm): Option[False0] = term match {
          case STerm.Cons("False", scala.List(), o) => scala.Some(False0(o))
          case _ => None
        }
      }
    }
    case class Not1(expr1: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Not", scala.List(expr1.toSTerm), origin)
    }
    object Not1 extends scalaterms.TermLikeCompanion[Not1] {
      override val fromSTerm: scalaterms.FromSTerm[Not1] = new scalaterms.FromSTerm[Not1] {
        override def unapply(term: STerm): Option[Not1] = term match {
          case STerm.Cons("Not", scala.List(SExpr.fromSTerm(expr1)), o) => scala.Some(Not1(expr1, o))
          case _ => None
        }
      }
    }
    case class And2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("And", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object And2 extends scalaterms.TermLikeCompanion[And2] {
      override val fromSTerm: scalaterms.FromSTerm[And2] = new scalaterms.FromSTerm[And2] {
        override def unapply(term: STerm): Option[And2] = term match {
          case STerm.Cons("And", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(And2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Or2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Or", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Or2 extends scalaterms.TermLikeCompanion[Or2] {
      override val fromSTerm: scalaterms.FromSTerm[Or2] = new scalaterms.FromSTerm[Or2] {
        override def unapply(term: STerm): Option[Or2] = term match {
          case STerm.Cons("Or", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Or2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Eq2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Eq", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Eq2 extends scalaterms.TermLikeCompanion[Eq2] {
      override val fromSTerm: scalaterms.FromSTerm[Eq2] = new scalaterms.FromSTerm[Eq2] {
        override def unapply(term: STerm): Option[Eq2] = term match {
          case STerm.Cons("Eq", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Eq2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Gt2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Gt", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Gt2 extends scalaterms.TermLikeCompanion[Gt2] {
      override val fromSTerm: scalaterms.FromSTerm[Gt2] = new scalaterms.FromSTerm[Gt2] {
        override def unapply(term: STerm): Option[Gt2] = term match {
          case STerm.Cons("Gt", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Gt2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Gte2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Gte", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Gte2 extends scalaterms.TermLikeCompanion[Gte2] {
      override val fromSTerm: scalaterms.FromSTerm[Gte2] = new scalaterms.FromSTerm[Gte2] {
        override def unapply(term: STerm): Option[Gte2] = term match {
          case STerm.Cons("Gte", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Gte2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Lt2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Lt", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Lt2 extends scalaterms.TermLikeCompanion[Lt2] {
      override val fromSTerm: scalaterms.FromSTerm[Lt2] = new scalaterms.FromSTerm[Lt2] {
        override def unapply(term: STerm): Option[Lt2] = term match {
          case STerm.Cons("Lt", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Lt2(expr1, expr2, o))
          case _ => None
        }
      }
    }
    case class Lte2(expr1: SExpr, expr2: SExpr, origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Lte", scala.List(expr1.toSTerm, expr2.toSTerm), origin)
    }
    object Lte2 extends scalaterms.TermLikeCompanion[Lte2] {
      override val fromSTerm: scalaterms.FromSTerm[Lte2] = new scalaterms.FromSTerm[Lte2] {
        override def unapply(term: STerm): Option[Lte2] = term match {
          case STerm.Cons("Lte", scala.List(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), o) => scala.Some(Lte2(expr1, expr2, o))
          case _ => None
        }
      }
    }
  }
}