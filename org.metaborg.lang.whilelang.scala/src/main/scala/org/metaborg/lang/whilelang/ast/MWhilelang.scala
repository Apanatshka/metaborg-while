package org.metaborg.lang.whilelang.ast

object MWhilelang {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.lang.whilelang.ast.MStatement._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SStart extends sdf.Constructor
  // Constructor definitions
  object SStart extends scalaterms.TermLikeCompanion[SStart] {
    override val fromSTerm: scalaterms.FromSTerm[SStart] = new scalaterms.FromSTerm[SStart] {
      override def unapply(term: STerm): Option[SStart] = term match {
        case While1.fromSTerm(start1) => scala.Some(start1)
        case Labeled1.fromSTerm(start1) => scala.Some(start1)
        case _ => scala.None
      }
    }

    case class While1(statement1: SStatement, origin: scalaterms.Origin) extends SStart {
      override def toSTerm = STerm.Cons("While", scala.List(statement1.toSTerm), origin)
    }
    object While1 extends scalaterms.TermLikeCompanion[While1] {
      override val fromSTerm: scalaterms.FromSTerm[While1] = new scalaterms.FromSTerm[While1] {
        override def unapply(term: STerm): Option[While1] = term match {
          case STerm.Cons("While", scala.List(SStatement.fromSTerm(statement1)), o) => scala.Some(While1(statement1, o))
          case _ => None
        }
      }
    }
    case class Labeled1(labeledstatement1: SLabeledStatement, origin: scalaterms.Origin) extends SStart {
      override def toSTerm = STerm.Cons("Labeled", scala.List(labeledstatement1.toSTerm), origin)
    }
    object Labeled1 extends scalaterms.TermLikeCompanion[Labeled1] {
      override val fromSTerm: scalaterms.FromSTerm[Labeled1] = new scalaterms.FromSTerm[Labeled1] {
        override def unapply(term: STerm): Option[Labeled1] = term match {
          case STerm.Cons("Labeled", scala.List(SLabeledStatement.fromSTerm(labeledstatement1)), o) => scala.Some(Labeled1(labeledstatement1, o))
          case _ => None
        }
      }
    }
  }
}