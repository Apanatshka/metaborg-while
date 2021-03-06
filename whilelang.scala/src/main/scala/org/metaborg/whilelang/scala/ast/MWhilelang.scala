package org.metaborg.whilelang.scala.ast

object MWhilelang {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import MStatement._
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

    case class While1(statement1: SStatement)(val origin: scalaterms.Origin) extends SStart {
      override def toSTerm = STerm.Cons("While", scala.Seq(statement1.toSTerm), scala.Some(origin))
    }
    object While1 extends scalaterms.TermLikeCompanion[While1] {
      override val fromSTerm: scalaterms.FromSTerm[While1] = new scalaterms.FromSTerm[While1] {
        override def unapply(term: STerm): Option[While1] = term match {
          case STerm.Cons("While", scala.Seq(SStatement.fromSTerm(statement1)), scala.Some(origin)) =>
            scala.Some(While1(statement1)(origin))
          case _ => None
        }
      }
    }
    case class Labeled1(labeledstatement1: SLabeledStatement)(val origin: scalaterms.Origin) extends SStart {
      override def toSTerm = STerm.Cons("Labeled", scala.Seq(labeledstatement1.toSTerm), scala.Some(origin))
    }
    object Labeled1 extends scalaterms.TermLikeCompanion[Labeled1] {
      override val fromSTerm: scalaterms.FromSTerm[Labeled1] = new scalaterms.FromSTerm[Labeled1] {
        override def unapply(term: STerm): Option[Labeled1] = term match {
          case STerm.Cons("Labeled", scala.Seq(SLabeledStatement.fromSTerm(labeledstatement1)), scala.Some(origin)) =>
            scala.Some(Labeled1(labeledstatement1)(origin))
          case _ => None
        }
      }
    }
  }
}