package org.metaborg.whilelang.scala.ast

object MStatement {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import MCommon._
  import MExpr._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SStatement extends sdf.Constructor
  sealed trait SLabeledStatement extends sdf.Constructor
  // Constructor definitions
  object SStatement extends scalaterms.TermLikeCompanion[SStatement] {
    override val fromSTerm: scalaterms.FromSTerm[SStatement] = new scalaterms.FromSTerm[SStatement] {
      override def unapply(term: STerm): Option[SStatement] = term match {
        case Assign2.fromSTerm(statement1) => scala.Some(statement1)
        case Skip0.fromSTerm(statement1) => scala.Some(statement1)
        case Seq2.fromSTerm(statement1) => scala.Some(statement1)
        case IfThenElse3.fromSTerm(statement1) => scala.Some(statement1)
        case While2.fromSTerm(statement1) => scala.Some(statement1)
        case _ => scala.None
      }
    }

    case class Assign2(id1: SID, expr2: SExpr)(val origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Assign", scala.Seq(id1.toSTerm, expr2.toSTerm), scala.Some(origin))
    }
    object Assign2 extends scalaterms.TermLikeCompanion[Assign2] {
      override val fromSTerm: scalaterms.FromSTerm[Assign2] = new scalaterms.FromSTerm[Assign2] {
        override def unapply(term: STerm): Option[Assign2] = term match {
          case STerm.Cons("Assign", scala.Seq(SID.fromSTerm(id1), SExpr.fromSTerm(expr2)), scala.Some(origin)) =>
            scala.Some(Assign2(id1, expr2)(origin))
          case _ => None
        }
      }
    }
    case class Skip0()(val origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Skip", scala.Seq(), scala.Some(origin))
    }
    object Skip0 extends scalaterms.TermLikeCompanion[Skip0] {
      override val fromSTerm: scalaterms.FromSTerm[Skip0] = new scalaterms.FromSTerm[Skip0] {
        override def unapply(term: STerm): Option[Skip0] = term match {
          case STerm.Cons("Skip", scala.Seq(), scala.Some(origin)) =>
            scala.Some(Skip0()(origin))
          case _ => None
        }
      }
    }
    case class Seq2(statement1: SStatement, statement2: SStatement)(val origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Seq", scala.Seq(statement1.toSTerm, statement2.toSTerm), scala.Some(origin))
    }
    object Seq2 extends scalaterms.TermLikeCompanion[Seq2] {
      override val fromSTerm: scalaterms.FromSTerm[Seq2] = new scalaterms.FromSTerm[Seq2] {
        override def unapply(term: STerm): Option[Seq2] = term match {
          case STerm.Cons("Seq", scala.Seq(SStatement.fromSTerm(statement1), SStatement.fromSTerm(statement2)), scala.Some(origin)) =>
            scala.Some(Seq2(statement1, statement2)(origin))
          case _ => None
        }
      }
    }
    case class IfThenElse3(expr1: SExpr, statement2: SStatement, statement3: SStatement)(val origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("IfThenElse", scala.Seq(expr1.toSTerm, statement2.toSTerm, statement3.toSTerm), scala.Some(origin))
    }
    object IfThenElse3 extends scalaterms.TermLikeCompanion[IfThenElse3] {
      override val fromSTerm: scalaterms.FromSTerm[IfThenElse3] = new scalaterms.FromSTerm[IfThenElse3] {
        override def unapply(term: STerm): Option[IfThenElse3] = term match {
          case STerm.Cons("IfThenElse", scala.Seq(SExpr.fromSTerm(expr1), SStatement.fromSTerm(statement2), SStatement.fromSTerm(statement3)), scala.Some(origin)) =>
            scala.Some(IfThenElse3(expr1, statement2, statement3)(origin))
          case _ => None
        }
      }
    }
    case class While2(expr1: SExpr, statement2: SStatement)(val origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("While", scala.Seq(expr1.toSTerm, statement2.toSTerm), scala.Some(origin))
    }
    object While2 extends scalaterms.TermLikeCompanion[While2] {
      override val fromSTerm: scalaterms.FromSTerm[While2] = new scalaterms.FromSTerm[While2] {
        override def unapply(term: STerm): Option[While2] = term match {
          case STerm.Cons("While", scala.Seq(SExpr.fromSTerm(expr1), SStatement.fromSTerm(statement2)), scala.Some(origin)) =>
            scala.Some(While2(expr1, statement2)(origin))
          case _ => None
        }
      }
    }
  }
  object SLabeledStatement extends scalaterms.TermLikeCompanion[SLabeledStatement] {
    override val fromSTerm: scalaterms.FromSTerm[SLabeledStatement] = new scalaterms.FromSTerm[SLabeledStatement] {
      override def unapply(term: STerm): Option[SLabeledStatement] = term match {
        case LabeledAssign3.fromSTerm(labeledstatement1) => scala.Some(labeledstatement1)
        case LabeledSkip1.fromSTerm(labeledstatement1) => scala.Some(labeledstatement1)
        case Seq2.fromSTerm(labeledstatement1) => scala.Some(labeledstatement1)
        case LabeledIfThenElse4.fromSTerm(labeledstatement1) => scala.Some(labeledstatement1)
        case LabeledWhile3.fromSTerm(labeledstatement1) => scala.Some(labeledstatement1)
        case _ => scala.None
      }
    }

    case class LabeledAssign3(id1: SID, expr2: SExpr, int3: SINT)(val origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledAssign", scala.Seq(id1.toSTerm, expr2.toSTerm, int3.toSTerm), scala.Some(origin))
    }
    object LabeledAssign3 extends scalaterms.TermLikeCompanion[LabeledAssign3] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledAssign3] = new scalaterms.FromSTerm[LabeledAssign3] {
        override def unapply(term: STerm): Option[LabeledAssign3] = term match {
          case STerm.Cons("LabeledAssign", scala.Seq(SID.fromSTerm(id1), SExpr.fromSTerm(expr2), SINT.fromSTerm(int3)), scala.Some(origin)) =>
            scala.Some(LabeledAssign3(id1, expr2, int3)(origin))
          case _ => None
        }
      }
    }
    case class LabeledSkip1(int1: SINT)(val origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledSkip", scala.Seq(int1.toSTerm), scala.Some(origin))
    }
    object LabeledSkip1 extends scalaterms.TermLikeCompanion[LabeledSkip1] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledSkip1] = new scalaterms.FromSTerm[LabeledSkip1] {
        override def unapply(term: STerm): Option[LabeledSkip1] = term match {
          case STerm.Cons("LabeledSkip", scala.Seq(SINT.fromSTerm(int1)), scala.Some(origin)) =>
            scala.Some(LabeledSkip1(int1)(origin))
          case _ => None
        }
      }
    }
    case class Seq2(labeledstatement1: SLabeledStatement, labeledstatement2: SLabeledStatement)(val origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("Seq", scala.Seq(labeledstatement1.toSTerm, labeledstatement2.toSTerm), scala.Some(origin))
    }
    object Seq2 extends scalaterms.TermLikeCompanion[Seq2] {
      override val fromSTerm: scalaterms.FromSTerm[Seq2] = new scalaterms.FromSTerm[Seq2] {
        override def unapply(term: STerm): Option[Seq2] = term match {
          case STerm.Cons("Seq", scala.Seq(SLabeledStatement.fromSTerm(labeledstatement1), SLabeledStatement.fromSTerm(labeledstatement2)), scala.Some(origin)) =>
            scala.Some(Seq2(labeledstatement1, labeledstatement2)(origin))
          case _ => None
        }
      }
    }
    case class LabeledIfThenElse4(expr1: SExpr, int2: SINT, labeledstatement3: SLabeledStatement, labeledstatement4: SLabeledStatement)(val origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledIfThenElse", scala.Seq(expr1.toSTerm, int2.toSTerm, labeledstatement3.toSTerm, labeledstatement4.toSTerm), scala.Some(origin))
    }
    object LabeledIfThenElse4 extends scalaterms.TermLikeCompanion[LabeledIfThenElse4] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledIfThenElse4] = new scalaterms.FromSTerm[LabeledIfThenElse4] {
        override def unapply(term: STerm): Option[LabeledIfThenElse4] = term match {
          case STerm.Cons("LabeledIfThenElse", scala.Seq(SExpr.fromSTerm(expr1), SINT.fromSTerm(int2), SLabeledStatement.fromSTerm(labeledstatement3), SLabeledStatement.fromSTerm(labeledstatement4)), scala.Some(origin)) =>
            scala.Some(LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4)(origin))
          case _ => None
        }
      }
    }
    case class LabeledWhile3(expr1: SExpr, int2: SINT, labeledstatement3: SLabeledStatement)(val origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledWhile", scala.Seq(expr1.toSTerm, int2.toSTerm, labeledstatement3.toSTerm), scala.Some(origin))
    }
    object LabeledWhile3 extends scalaterms.TermLikeCompanion[LabeledWhile3] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledWhile3] = new scalaterms.FromSTerm[LabeledWhile3] {
        override def unapply(term: STerm): Option[LabeledWhile3] = term match {
          case STerm.Cons("LabeledWhile", scala.Seq(SExpr.fromSTerm(expr1), SINT.fromSTerm(int2), SLabeledStatement.fromSTerm(labeledstatement3)), scala.Some(origin)) =>
            scala.Some(LabeledWhile3(expr1, int2, labeledstatement3)(origin))
          case _ => None
        }
      }
    }
  }
}