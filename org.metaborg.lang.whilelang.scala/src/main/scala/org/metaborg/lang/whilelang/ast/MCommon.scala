package org.metaborg.lang.whilelang.ast

object MCommon {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports

  // Lexical definitions
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SID(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SINT(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SSTRING(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SStringChar(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SBackSlashChar(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SCommentChar(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SInsideComment(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SNewLineEOF(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Define implicit conversions (e.g. in the package object) to another representation you prefer
  case class SEOF(string: java.lang.String, origin: scalaterms.Origin) extends sdf.Lexical {
    override def toSTerm: STerm.String = STerm.String(string, origin)
  }
  // Lexical extractors
  object SID extends scalaterms.TermLikeCompanion[SID] {
    override val fromSTerm = new scalaterms.FromSTerm[SID] {
      override def unapply(term: STerm): Option[SID] = term match {
        case STerm.String(string, origin) => scala.Some(SID(string, origin))
        case _ => scala.None
      }
    }
  }
  object SINT extends scalaterms.TermLikeCompanion[SINT] {
    override val fromSTerm = new scalaterms.FromSTerm[SINT] {
      override def unapply(term: STerm): Option[SINT] = term match {
        case STerm.String(string, origin) => scala.Some(SINT(string, origin))
        case _ => scala.None
      }
    }
  }
  object SSTRING extends scalaterms.TermLikeCompanion[SSTRING] {
    override val fromSTerm = new scalaterms.FromSTerm[SSTRING] {
      override def unapply(term: STerm): Option[SSTRING] = term match {
        case STerm.String(string, origin) => scala.Some(SSTRING(string, origin))
        case _ => scala.None
      }
    }
  }
  object SStringChar extends scalaterms.TermLikeCompanion[SStringChar] {
    override val fromSTerm = new scalaterms.FromSTerm[SStringChar] {
      override def unapply(term: STerm): Option[SStringChar] = term match {
        case STerm.String(string, origin) => scala.Some(SStringChar(string, origin))
        case _ => scala.None
      }
    }
  }
  object SBackSlashChar extends scalaterms.TermLikeCompanion[SBackSlashChar] {
    override val fromSTerm = new scalaterms.FromSTerm[SBackSlashChar] {
      override def unapply(term: STerm): Option[SBackSlashChar] = term match {
        case STerm.String(string, origin) => scala.Some(SBackSlashChar(string, origin))
        case _ => scala.None
      }
    }
  }
  object SCommentChar extends scalaterms.TermLikeCompanion[SCommentChar] {
    override val fromSTerm = new scalaterms.FromSTerm[SCommentChar] {
      override def unapply(term: STerm): Option[SCommentChar] = term match {
        case STerm.String(string, origin) => scala.Some(SCommentChar(string, origin))
        case _ => scala.None
      }
    }
  }
  object SInsideComment extends scalaterms.TermLikeCompanion[SInsideComment] {
    override val fromSTerm = new scalaterms.FromSTerm[SInsideComment] {
      override def unapply(term: STerm): Option[SInsideComment] = term match {
        case STerm.String(string, origin) => scala.Some(SInsideComment(string, origin))
        case _ => scala.None
      }
    }
  }
  object SNewLineEOF extends scalaterms.TermLikeCompanion[SNewLineEOF] {
    override val fromSTerm = new scalaterms.FromSTerm[SNewLineEOF] {
      override def unapply(term: STerm): Option[SNewLineEOF] = term match {
        case STerm.String(string, origin) => scala.Some(SNewLineEOF(string, origin))
        case _ => scala.None
      }
    }
  }
  object SEOF extends scalaterms.TermLikeCompanion[SEOF] {
    override val fromSTerm = new scalaterms.FromSTerm[SEOF] {
      override def unapply(term: STerm): Option[SEOF] = term match {
        case STerm.String(string, origin) => scala.Some(SEOF(string, origin))
        case _ => scala.None
      }
    }
  }
  // Sort definitions

  // Constructor definitions

}