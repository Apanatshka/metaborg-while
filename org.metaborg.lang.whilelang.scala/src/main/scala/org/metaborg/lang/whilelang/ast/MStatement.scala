package org.metaborg.lang.whilelang.ast

object MStatement {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.lang.whilelang.ast.MCommon._
  import org.metaborg.lang.whilelang.ast.MExpr._
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

    case class Assign2(id1: SID, expr2: SExpr, origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Assign", scala.List(id1.toSTerm, expr2.toSTerm), origin)
    }
    object Assign2 extends scalaterms.TermLikeCompanion[Assign2] {
      override val fromSTerm: scalaterms.FromSTerm[Assign2] = new scalaterms.FromSTerm[Assign2] {
        override def unapply(term: STerm): Option[Assign2] = term match {
          case STerm.Cons("Assign", scala.List(SID.fromSTerm(id1), SExpr.fromSTerm(expr2)), o) => scala.Some(Assign2(id1, expr2, o))
          case _ => None
        }
      }
    }
    case class Skip0(origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Skip", scala.List(), origin)
    }
    object Skip0 extends scalaterms.TermLikeCompanion[Skip0] {
      override val fromSTerm: scalaterms.FromSTerm[Skip0] = new scalaterms.FromSTerm[Skip0] {
        override def unapply(term: STerm): Option[Skip0] = term match {
          case STerm.Cons("Skip", scala.List(), o) => scala.Some(Skip0(o))
          case _ => None
        }
      }
    }
    case class Seq2(statement1: SStatement, statement2: SStatement, origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("Seq", scala.List(statement1.toSTerm, statement2.toSTerm), origin)
    }
    object Seq2 extends scalaterms.TermLikeCompanion[Seq2] {
      override val fromSTerm: scalaterms.FromSTerm[Seq2] = new scalaterms.FromSTerm[Seq2] {
        override def unapply(term: STerm): Option[Seq2] = term match {
          case STerm.Cons("Seq", scala.List(SStatement.fromSTerm(statement1), SStatement.fromSTerm(statement2)), o) => scala.Some(Seq2(statement1, statement2, o))
          case _ => None
        }
      }
    }
    case class IfThenElse3(expr1: SExpr, statement2: SStatement, statement3: SStatement, origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("IfThenElse", scala.List(expr1.toSTerm, statement2.toSTerm, statement3.toSTerm), origin)
    }
    object IfThenElse3 extends scalaterms.TermLikeCompanion[IfThenElse3] {
      override val fromSTerm: scalaterms.FromSTerm[IfThenElse3] = new scalaterms.FromSTerm[IfThenElse3] {
        override def unapply(term: STerm): Option[IfThenElse3] = term match {
          case STerm.Cons("IfThenElse", scala.List(SExpr.fromSTerm(expr1), SStatement.fromSTerm(statement2), SStatement.fromSTerm(statement3)), o) => scala.Some(IfThenElse3(expr1, statement2, statement3, o))
          case _ => None
        }
      }
    }
    case class While2(expr1: SExpr, statement2: SStatement, origin: scalaterms.Origin) extends SStatement {
      override def toSTerm = STerm.Cons("While", scala.List(expr1.toSTerm, statement2.toSTerm), origin)
    }
    object While2 extends scalaterms.TermLikeCompanion[While2] {
      override val fromSTerm: scalaterms.FromSTerm[While2] = new scalaterms.FromSTerm[While2] {
        override def unapply(term: STerm): Option[While2] = term match {
          case STerm.Cons("While", scala.List(SExpr.fromSTerm(expr1), SStatement.fromSTerm(statement2)), o) => scala.Some(While2(expr1, statement2, o))
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

    case class LabeledAssign3(id1: SID, expr2: SExpr, int3: SINT, origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledAssign", scala.List(id1.toSTerm, expr2.toSTerm, int3.toSTerm), origin)
    }
    object LabeledAssign3 extends scalaterms.TermLikeCompanion[LabeledAssign3] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledAssign3] = new scalaterms.FromSTerm[LabeledAssign3] {
        override def unapply(term: STerm): Option[LabeledAssign3] = term match {
          case STerm.Cons("LabeledAssign", scala.List(SID.fromSTerm(id1), SExpr.fromSTerm(expr2), SINT.fromSTerm(int3)), o) => scala.Some(LabeledAssign3(id1, expr2, int3, o))
          case _ => None
        }
      }
    }
    case class LabeledSkip1(int1: SINT, origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledSkip", scala.List(int1.toSTerm), origin)
    }
    object LabeledSkip1 extends scalaterms.TermLikeCompanion[LabeledSkip1] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledSkip1] = new scalaterms.FromSTerm[LabeledSkip1] {
        override def unapply(term: STerm): Option[LabeledSkip1] = term match {
          case STerm.Cons("LabeledSkip", scala.List(SINT.fromSTerm(int1)), o) => scala.Some(LabeledSkip1(int1, o))
          case _ => None
        }
      }
    }
    case class Seq2(labeledstatement1: SLabeledStatement, labeledstatement2: SLabeledStatement, origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("Seq", scala.List(labeledstatement1.toSTerm, labeledstatement2.toSTerm), origin)
    }
    object Seq2 extends scalaterms.TermLikeCompanion[Seq2] {
      override val fromSTerm: scalaterms.FromSTerm[Seq2] = new scalaterms.FromSTerm[Seq2] {
        override def unapply(term: STerm): Option[Seq2] = term match {
          case STerm.Cons("Seq", scala.List(SLabeledStatement.fromSTerm(labeledstatement1), SLabeledStatement.fromSTerm(labeledstatement2)), o) => scala.Some(Seq2(labeledstatement1, labeledstatement2, o))
          case _ => None
        }
      }
    }
    case class LabeledIfThenElse4(expr1: SExpr, int2: SINT, labeledstatement3: SLabeledStatement, labeledstatement4: SLabeledStatement, origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledIfThenElse", scala.List(expr1.toSTerm, int2.toSTerm, labeledstatement3.toSTerm, labeledstatement4.toSTerm), origin)
    }
    object LabeledIfThenElse4 extends scalaterms.TermLikeCompanion[LabeledIfThenElse4] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledIfThenElse4] = new scalaterms.FromSTerm[LabeledIfThenElse4] {
        override def unapply(term: STerm): Option[LabeledIfThenElse4] = term match {
          case STerm.Cons("LabeledIfThenElse", scala.List(SExpr.fromSTerm(expr1), SINT.fromSTerm(int2), SLabeledStatement.fromSTerm(labeledstatement3), SLabeledStatement.fromSTerm(labeledstatement4)), o) => scala.Some(LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, o))
          case _ => None
        }
      }
    }
    case class LabeledWhile3(expr1: SExpr, int2: SINT, labeledstatement3: SLabeledStatement, origin: scalaterms.Origin) extends SLabeledStatement {
      override def toSTerm = STerm.Cons("LabeledWhile", scala.List(expr1.toSTerm, int2.toSTerm, labeledstatement3.toSTerm), origin)
    }
    object LabeledWhile3 extends scalaterms.TermLikeCompanion[LabeledWhile3] {
      override val fromSTerm: scalaterms.FromSTerm[LabeledWhile3] = new scalaterms.FromSTerm[LabeledWhile3] {
        override def unapply(term: STerm): Option[LabeledWhile3] = term match {
          case STerm.Cons("LabeledWhile", scala.List(SExpr.fromSTerm(expr1), SINT.fromSTerm(int2), SLabeledStatement.fromSTerm(labeledstatement3)), o) => scala.Some(LabeledWhile3(expr1, int2, labeledstatement3, o))
          case _ => None
        }
      }
    }
  }
}