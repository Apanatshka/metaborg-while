package org.metaborg.whilelang.scala.ast

object MExpr {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import MCommon._
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

    case class Ref1(id1: SID)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Ref", scala.Seq(id1.toSTerm), scala.Some(origin))
    }
    object Ref1 extends scalaterms.TermLikeCompanion[Ref1] {
      override val fromSTerm: scalaterms.FromSTerm[Ref1] = new scalaterms.FromSTerm[Ref1] {
        override def unapply(term: STerm): Option[Ref1] = term match {
          case STerm.Cons("Ref", scala.Seq(SID.fromSTerm(id1)), scala.Some(origin)) =>
            scala.Some(Ref1(id1)(origin))
          case _ => None
        }
      }
    }
    case class Num1(int1: SINT)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Num", scala.Seq(int1.toSTerm), scala.Some(origin))
    }
    object Num1 extends scalaterms.TermLikeCompanion[Num1] {
      override val fromSTerm: scalaterms.FromSTerm[Num1] = new scalaterms.FromSTerm[Num1] {
        override def unapply(term: STerm): Option[Num1] = term match {
          case STerm.Cons("Num", scala.Seq(SINT.fromSTerm(int1)), scala.Some(origin)) =>
            scala.Some(Num1(int1)(origin))
          case _ => None
        }
      }
    }
    case class Add2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Add", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Add2 extends scalaterms.TermLikeCompanion[Add2] {
      override val fromSTerm: scalaterms.FromSTerm[Add2] = new scalaterms.FromSTerm[Add2] {
        override def unapply(term: STerm): Option[Add2] = term match {
          case STerm.Cons("Add", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Add2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Sub2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Sub", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Sub2 extends scalaterms.TermLikeCompanion[Sub2] {
      override val fromSTerm: scalaterms.FromSTerm[Sub2] = new scalaterms.FromSTerm[Sub2] {
        override def unapply(term: STerm): Option[Sub2] = term match {
          case STerm.Cons("Sub", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Sub2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Mul2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Mul", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Mul2 extends scalaterms.TermLikeCompanion[Mul2] {
      override val fromSTerm: scalaterms.FromSTerm[Mul2] = new scalaterms.FromSTerm[Mul2] {
        override def unapply(term: STerm): Option[Mul2] = term match {
          case STerm.Cons("Mul", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Mul2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Div2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Div", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Div2 extends scalaterms.TermLikeCompanion[Div2] {
      override val fromSTerm: scalaterms.FromSTerm[Div2] = new scalaterms.FromSTerm[Div2] {
        override def unapply(term: STerm): Option[Div2] = term match {
          case STerm.Cons("Div", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Div2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class True0()(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("True", scala.Seq(), scala.Some(origin))
    }
    object True0 extends scalaterms.TermLikeCompanion[True0] {
      override val fromSTerm: scalaterms.FromSTerm[True0] = new scalaterms.FromSTerm[True0] {
        override def unapply(term: STerm): Option[True0] = term match {
          case STerm.Cons("True", scala.Seq(), scala.Some(origin)) =>
            scala.Some(True0()(origin))
          case _ => None
        }
      }
    }
    case class False0()(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("False", scala.Seq(), scala.Some(origin))
    }
    object False0 extends scalaterms.TermLikeCompanion[False0] {
      override val fromSTerm: scalaterms.FromSTerm[False0] = new scalaterms.FromSTerm[False0] {
        override def unapply(term: STerm): Option[False0] = term match {
          case STerm.Cons("False", scala.Seq(), scala.Some(origin)) =>
            scala.Some(False0()(origin))
          case _ => None
        }
      }
    }
    case class Not1(expr1: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Not", scala.Seq(expr1.toSTerm), scala.Some(origin))
    }
    object Not1 extends scalaterms.TermLikeCompanion[Not1] {
      override val fromSTerm: scalaterms.FromSTerm[Not1] = new scalaterms.FromSTerm[Not1] {
        override def unapply(term: STerm): Option[Not1] = term match {
          case STerm.Cons("Not", scala.Seq(SExpr.fromSTerm(expr1)), scala.Some(origin)) =>
            scala.Some(Not1(expr1)(origin))
          case _ => None
        }
      }
    }
    case class And2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("And", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object And2 extends scalaterms.TermLikeCompanion[And2] {
      override val fromSTerm: scalaterms.FromSTerm[And2] = new scalaterms.FromSTerm[And2] {
        override def unapply(term: STerm): Option[And2] = term match {
          case STerm.Cons("And", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(And2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Or2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Or", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Or2 extends scalaterms.TermLikeCompanion[Or2] {
      override val fromSTerm: scalaterms.FromSTerm[Or2] = new scalaterms.FromSTerm[Or2] {
        override def unapply(term: STerm): Option[Or2] = term match {
          case STerm.Cons("Or", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Or2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Eq2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Eq", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Eq2 extends scalaterms.TermLikeCompanion[Eq2] {
      override val fromSTerm: scalaterms.FromSTerm[Eq2] = new scalaterms.FromSTerm[Eq2] {
        override def unapply(term: STerm): Option[Eq2] = term match {
          case STerm.Cons("Eq", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Eq2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Gt2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Gt", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Gt2 extends scalaterms.TermLikeCompanion[Gt2] {
      override val fromSTerm: scalaterms.FromSTerm[Gt2] = new scalaterms.FromSTerm[Gt2] {
        override def unapply(term: STerm): Option[Gt2] = term match {
          case STerm.Cons("Gt", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Gt2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Gte2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Gte", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Gte2 extends scalaterms.TermLikeCompanion[Gte2] {
      override val fromSTerm: scalaterms.FromSTerm[Gte2] = new scalaterms.FromSTerm[Gte2] {
        override def unapply(term: STerm): Option[Gte2] = term match {
          case STerm.Cons("Gte", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Gte2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Lt2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Lt", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Lt2 extends scalaterms.TermLikeCompanion[Lt2] {
      override val fromSTerm: scalaterms.FromSTerm[Lt2] = new scalaterms.FromSTerm[Lt2] {
        override def unapply(term: STerm): Option[Lt2] = term match {
          case STerm.Cons("Lt", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Lt2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Lte2(expr1: SExpr, expr2: SExpr)(val origin: scalaterms.Origin) extends SExpr {
      override def toSTerm = STerm.Cons("Lte", scala.Seq(expr1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Lte2 extends scalaterms.TermLikeCompanion[Lte2] {
      override val fromSTerm: scalaterms.FromSTerm[Lte2] = new scalaterms.FromSTerm[Lte2] {
        override def unapply(term: STerm): Option[Lte2] = term match {
          case STerm.Cons("Lte", scala.Seq(SExpr.fromSTerm(expr1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Lte2(expr1, expr2)(origin))
          case _ => None
        }
      }
    }
  }
}