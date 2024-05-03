package scala.tools.refactoring.util

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.OffsetPosition

/**
 * Represents source code with a movable marker.
 *
 * @see [[SourceWithMarker.Movement]]
 * @see [[SourceWithMarker.Movements]]
 */
final case class SourceWithMarker(source: IndexedSeq[Char] = IndexedSeq(), marker: Int = 0) {
  import SourceWithMarker._

  def moveMarker(movement: SimpleMovement): SourceWithMarker = {
    applyMovement(movement).getOrElse(this)
  }

  def moveMarkerBack(movement: Movement): SourceWithMarker = {
    moveMarker(movement.backward)
  }

  def applyMovement(movement: SimpleMovement): Option[SourceWithMarker] = {
    movement(this).map(m => copy(marker = m)).orElse(None)
  }

  def withMarkerAt(pos: Int): SourceWithMarker = {
    copy(marker = pos)
  }

  def withMarkerAtLastChar: SourceWithMarker = {
    copy(marker = source.size - 1)
  }

  def current: Char = source(marker)

  def currentOption: Option[Char] = {
    if (isDepleted) None
    else Some(source(marker))
  }

  def isInRange(i: Int): Boolean = {
    i >= 0 && i < source.length
  }

  def isDepleted: Boolean = {
    !isInRange(marker)
  }

  def step(forward: Boolean): SourceWithMarker = {
    val delta = if (forward) 1 else -1
    copy(marker = marker + delta)
  }

  def stepForward: SourceWithMarker = step(true)

  def length = source.length

  override def toString = {
    def mkFlatString(chars: Seq[Char]): String = {
      chars.mkString("").replace("\r\n", "\\r\\n").replace("\n", "\\n")
    }

    val lrChars = 10
    val nChars = lrChars*2 + 1

    def leftDots = if (marker - lrChars > 0) "..." else ""
    def rightDots = if (marker + lrChars < source.length - 1) "..." else ""

    if (marker < 0) {
      "<>" + mkFlatString(source.take(nChars)) + rightDots
    } else if (marker >= source.length) {
      leftDots + mkFlatString(source.takeRight(nChars)) + "<>"
    } else {
      val marked = current
      val lStr = leftDots + mkFlatString(source.slice(marker - lrChars, marker))
      val rStr = mkFlatString(source.slice(marker + 1, marker + 1 + lrChars)) + rightDots
      s"$lStr<$marked>$rStr"
    }
  }

  override def equals(obj: Any) = obj match {
    case SourceWithMarker(otherSource, otherMarker) =>
      source.toSeq == otherSource.toSeq && marker == otherMarker
    case _ => false
  }

  override def hashCode = {
    (source.toSeq, marker).hashCode
  }
}

object SourceWithMarker {

  def beforeStartOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.start - 1)
  }

  def atStartOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.start)
  }

  def afterEndOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.end + 1)
  }

  def atEndOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.end + 1)
  }

  def atPoint(pos: OffsetPosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.point)
  }

  /**
   * A [[Movement]] that can be applied in one direction.
   *
   * The difference between a [[SimpleMovement]] and a [[Movement]] is that [[SimpleMovement]]
   * is missing the `backward` method. Apart from that, the documentation for [[Movement]] applies.
   * Note that ''movements'' can be combined with ''simple movements'', so there is no need to
   * implement a full movement unless you need it.
   */
  trait SimpleMovement { self =>
    final def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
      compute(sourceWithMarker).headOption
    }

    /**
     * The actual implementation of the movement
     *
     * Returns a possibly empty sequence of markers, that this movement could lead to,
     * starting at the given argument. For example, the movement `'a'.atLeastOnce` applied
     * to the beginning of "aaa", would return `Seq(2, 1)`. Note that elements at the start
     * of the returned sequence are favored over elements coming later, so greedy movements
     * should always return the biggest jumps at the beginning.
     */
    def compute(sourceWithMarker: SourceWithMarker): Seq[Int]

    final def ~(other: SimpleMovement): SimpleMovement = SimpleMovementHelpers.sequence(this, other) _
    final def |(other: SimpleMovement): SimpleMovement = SimpleMovementHelpers.or(this, other) _
    final def ||(other: SimpleMovement): SimpleMovement = SimpleMovementHelpers.or(this, other) _
    def zeroOrMore: SimpleMovement = SimpleMovementHelpers.repeat(this) _
    def atLeastOnce: SimpleMovement = this ~ this.zeroOrMore
    def nTimes(n: Int): SimpleMovement = SimpleMovementHelpers.nTimes(this, n) _
    def butNot(other: SimpleMovement): SimpleMovement = SimpleMovementHelpers.butNot(this, other) _
    def optional: SimpleMovement = SimpleMovementHelpers.optional(this) _
    def atMostNtimes(n: Int): SimpleMovement = optional.nTimes(n)
    def refineWith(refinement: (SourceWithMarker, Seq[Int]) => Seq[Int]): SimpleMovement = SimpleMovementHelpers.refineWith(this, refinement) _
  }

  object SimpleMovement {
    implicit class WrapSimpleMovementImpl(impl: SourceWithMarker => Seq[Int]) extends SimpleMovement {
      override def compute(sourceWithMarker: SourceWithMarker) = impl(sourceWithMarker)
    }

    def apply(impl: SourceWithMarker => Seq[Int]): SimpleMovement = impl

    def startingWith(c: Char)(impl: SourceWithMarker => Seq[Int]) = SimpleMovement { sourceWithMarker =>
      if (sourceWithMarker.current == c) impl(sourceWithMarker)
      else Nil
    }

    def ifNotDepleted(impl: SourceWithMarker => Seq[Int]): SimpleMovement = {
      SimpleMovement(s => MovementHelpers.doIfNotDepleted(s)(impl(s)))
    }
  }

  implicit class SimpleMovementOps(val underlying: SimpleMovement) extends AnyVal {
    def consumes(str: IndexedSeq[Char]): Boolean = {
      val srcWithMarker = SourceWithMarker(str).moveMarker(underlying)
      srcWithMarker.marker > -1 && srcWithMarker.isDepleted
    }
  }

  private object SimpleMovementHelpers {
    def sequence[MovementT <: SimpleMovement](mvnt1: MovementT, mvnt2: MovementT)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      mvnt1.compute(sourceWithMarker).flatMap { marker =>
        val sourceAfterMvnt1 = sourceWithMarker.withMarkerAt(marker)
        mvnt2.compute(sourceAfterMvnt1)
      }.distinct
    }

    def or[MovementT <: SimpleMovement](mvnt1: MovementT, mvnt2: MovementT)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      val res1 = mvnt1.compute(sourceWithMarker)
      val res2 = mvnt2.compute(sourceWithMarker)

      (res1 ++ res2).sortBy { newMarker =>
        -math.abs((sourceWithMarker.marker - newMarker))
      }
    }

    def repeat[MovementT <: SimpleMovement](mvnt: MovementT)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      @tailrec
      def go(lastResults: Seq[Int] = Seq(sourceWithMarker.marker), acc: Seq[Int] = Seq()): Seq[Int] = {
        val newResults = lastResults.flatMap { marker =>
          mvnt.compute(sourceWithMarker.withMarkerAt(marker)).filterNot(lastResults.contains(_))
        }

        if (newResults.isEmpty) lastResults ++ acc
        else go(newResults, lastResults ++ acc)
      }

      go()
    }

    def nTimes[MovementT <: SimpleMovement](mvnt: MovementT, n: Int)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      if (n == 0) {
        Seq(sourceWithMarker.marker)
      } else if (n == 1) {
        mvnt.compute(sourceWithMarker)
      } else if (n > 1) {
        val res = mvnt.compute(sourceWithMarker)

        if (res.isEmpty) {
          Nil
        } else {
          res.flatMap { marker =>
            nTimes(mvnt, n - 1)(sourceWithMarker.withMarkerAt(marker))
          }.distinct
        }
      } else {
        throw new IllegalArgumentException(s"$n")
      }
    }

    def butNot[MovementT <: SimpleMovement](mvnt: MovementT, notMvnt: MovementT, forward: Boolean = true)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      def isBehind(m1: Int)(m2: Int) = {
        if (forward) m1 < m2
        else m1 > m2
      }

      mvnt.compute(sourceWithMarker) match {
        case Seq() => Nil

        case res =>
          val nRes = notMvnt.compute(sourceWithMarker)
          if (nRes.isEmpty) res
          else res.filter(isBehind(nRes.head))
      }
    }

    def optional[MovementT <: SimpleMovement](mvnt: MovementT)(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      mvnt.compute(sourceWithMarker) match {
        case Seq() => Seq(sourceWithMarker.marker)
        case res => res
      }
    }

    def refineWith[MovementT <: SimpleMovement](mvnt: MovementT, refinement: (SourceWithMarker, Seq[Int]) => Seq[Int])(sourceWithMarker: SourceWithMarker): Seq[Int] = {
      refinement(sourceWithMarker, mvnt.compute(sourceWithMarker))
    }
  }

  /**
   * A context dependent, directional movement that can be applied to a [[SourceWithMarker]]
   *
   * ==Overview==
   * Movements can be combined similar to parser combinators and optionally applied backwards.
   * They are meant to be used to perform minor tweaks in already parsed source code that might be necessary
   * due to compiler bugs or compiler API limitations.
   *
   * ==Why not use parser combinators?==
   * <ul>
   *  <li>We want to conveniently move forward <b>and</b> backward in the source code</li>
   *  <li>The code we are looking at is already parsed; we only want to move to specific points</li>
   * </ul>
   *
   * ==Examples==
   * {{{
   * scala> import scala.tools.refactoring.util.SourceWithMarker
   * scala> import scala.tools.refactoring.util.SourceWithMarker._
   * scala> import scala.tools.refactoring.util.SourceWithMarker.Movements._
   *
   * scala> val src = SourceWithMarker("private val /*---*/ x = 4".toCharArray)
   * src: scala.tools.refactoring.util.SourceWithMarker = <p>riv...
   * scala> val movement = ("private" | "protected") ~ commentsAndSpaces ~ "val" ~ commentsAndSpaces
   * scala> val srcAtx = src.moveMarker(movement)
   * srcAtx: scala.tools.refactoring.util.SourceWithMarker = ... <x> = ...
   * scala> val moveBackToVal = ("al" ~ commentsAndSpaces ~ "x").backward
   * scala> val srcAtVal = srcAtx.moveMarker(moveBackToVal)
   * srcAtVal:  scala.tools.refactoring.util.SourceWithMarker = ...te <v>al ...
   * }}}
   *
   * @see [[Movements]]
   */
  trait Movement extends SimpleMovement { self =>
    def backward: Movement

    final def ~(other: Movement) = Movement { (sourceWithMarker, forward) =>
      if (forward) {
        SimpleMovementHelpers.sequence(self, other)(sourceWithMarker)
      } else {
        SimpleMovementHelpers.sequence(other.backward, self.backward)(sourceWithMarker)
      }
    }

    final def |(other: Movement) = Movement { (sourceWithMarker, forward) =>
      if (forward) SimpleMovementHelpers.or(this, other)(sourceWithMarker)
      else SimpleMovementHelpers.or(this.backward, other.backward)(sourceWithMarker)
    }

    final def ||(other: Movement): Movement = this | other

    final override def zeroOrMore = Movement { (sourceWithMarker, forward) =>
      val mvnt = if (forward) self else self.backward
      SimpleMovementHelpers.repeat(mvnt)(sourceWithMarker)
    }

    final override def atLeastOnce: Movement = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      val mvnt = if (forward) self else self.backward

      mvnt.compute(sourceWithMarker) match {
        case Seq() => Nil
        case res =>
          res.flatMap { marker =>
            SimpleMovementHelpers.repeat(mvnt)(sourceWithMarker.withMarkerAt(marker))
          }.distinct
      }
    }

    final override def nTimes(n: Int) = Movement { (sourceWithMarker, forward) =>
      val mvnt = if  (forward) self else self.backward
      SimpleMovementHelpers.nTimes(mvnt, n)(sourceWithMarker)
    }

    final def butNot(mvnt: Movement) = Movement { (sourceWithMarker, forward) =>
      val (actualSelf, actualMvnt) = {
        if (forward) (self, mvnt)
        else (self.backward, mvnt.backward)
      }

      SimpleMovementHelpers.butNot(actualSelf, actualMvnt, forward)(sourceWithMarker)
    }

    final override def optional = Movement { (sourceWithMarker, forward) =>
      SimpleMovementHelpers.optional(if (forward) self else self.backward)(sourceWithMarker)
    }

    final override def atMostNtimes(n: Int) = optional.nTimes(n)

    final override def refineWith(refinement: (SourceWithMarker, Seq[Int]) => Seq[Int]): Movement = Movement { (sourceWithMarker, forward) =>
      val mvnt = {
        if (forward) self
        else self.backward
      }

      SimpleMovementHelpers.refineWith(mvnt, refinement)(sourceWithMarker)
    }
  }

  object Movement {
    /**
     * Factory method for constructing movements
     *
     * Constructs a movement from a function of the form `(sourceWithMarker, forward) => markers`
     * where `sourceWithMarker` represents a position in some piece of source code, `forward` tells
     * us in which direction to move, and the return value `markers` is the possibly empty
     * sequence of markers this movement might lead to.
     */
    def apply(impl: (SourceWithMarker, Boolean) => Seq[Int]): Movement = {
      class MovementImpl(forward: Boolean) extends Movement {
        override def compute(sourceWithMarker: SourceWithMarker) = impl(sourceWithMarker, forward)
        override def backward = new MovementImpl(!forward)
      }

      return new MovementImpl(true)
    }

    def ifNotDepleted(impl: (SourceWithMarker, Boolean) => Seq[Int]): Movement = {
      Movement((s, f) => MovementHelpers.doIfNotDepleted(s)(impl(s, f)))
    }

    def coveredStringStartingAtEndOf(pos: RangePosition, mvnt: SimpleMovement): String = {
      coveredString(pos.end, pos.source.content, mvnt)
    }

    def coveredString(pos: Int, src: IndexedSeq[Char], mvnt: SimpleMovement): String = {
      coveredString(SourceWithMarker(src, pos), mvnt)
    }

    def coveredString(sourceWithMarker: SourceWithMarker, mvnt: SimpleMovement): String = {
      val srcEnd = sourceWithMarker.moveMarker(mvnt)
      val (start, end) = (sourceWithMarker.marker, srcEnd.marker)
      val (actualStart, actualEnd) = {
        if (start <= end) (start, end)
        else (end + 1, start + 1)
      }
      sourceWithMarker.source.slice(actualStart, actualEnd).mkString("")
    }
  }

  /**
   * Various movements related to Scala code
   *
   * Take a look at the
   * [[http://www.scala-lang.org/files/archive/spec/2.11/01-lexical-syntax.html  Scala Language Specification]]
   * if you wonder about terms like ''plainid'', ''idrest'' or ''varid''.
   */
  object Movements {
    import MovementHelpers._

    /**
     * A specialized implementation that matches single characters
     *
     * Note the we represent characters as ints to avoid needless boxing ([[Function1]] is not specialized for [[Char]]).
     */
    class SingleCharMovement(private val acceptChar: Int => Boolean, private val forward: Boolean = true) extends Movement {
      final override def compute(sourceWithMarker: SourceWithMarker): Seq[Int] = {
        if (sourceWithMarker.isDepleted || !acceptChar(sourceWithMarker.current.toInt)) Nil
        else Seq(nextMarker(sourceWithMarker.marker, forward))
      }

      final def |(other: SingleCharMovement): SingleCharMovement = {
        new SingleCharMovement(c => acceptChar(c) || other.acceptChar(c), forward)
      }

      final override def backward: SingleCharMovement = new SingleCharMovement(acceptChar, !forward)

      final def butNot(mvnt: SingleCharMovement): SingleCharMovement = {
        new SingleCharMovement(c => acceptChar(c) && !mvnt.acceptChar(c))
      }
    }

    private[Movements] implicit class CharacterOps(val underlying: Int) extends AnyVal {
      def getType = Character.getType(underlying)
      def isUpper = Character.isUpperCase(underlying)
      def isLower = Character.isLowerCase(underlying)
      def isTitleCase = Character.isTitleCase(underlying)
      def isDigit = Character.isDigit(underlying)
      def isControl = Character.isISOControl(underlying)
    }

    val any = new SingleCharMovement(_ => true)

    val none = new SingleCharMovement(_ => false)

    def character(c: Char) = new SingleCharMovement(_ == c)

    def string(str: String) = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      def strAt(i: Int) = {
        if (forward) str.charAt(i)
        else str.charAt(str.length - 1 - i)
      }

      @tailrec
      def go(m: Int, i: Int = 0): Option[Int] = {
        if (i >= str.length) {
          Some(m)
        } else if (wouldBeDepleted(m, sourceWithMarker)) {
          None
        } else {
          if (strAt(i) == sourceWithMarker.source(m)) go(nextMarker(m, forward), i + 1)
          else None
        }
      }

      go(sourceWithMarker.marker).toSeq
    }

    val comment = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      @tailrec
      def go(m: Int, inSingleLineComment: Boolean = false, inMultilineComment: Int = 0, slashSeen: Boolean = false, starSeen: Boolean = false): Option[Int] = {
        if (wouldBeDepleted(m, sourceWithMarker)) {
          if (inSingleLineComment && forward) Some(m) else None
        } else {
          val c = sourceWithMarker.source(m)
          val nm = nextMarker(m, forward)

          if (inSingleLineComment) {
            if (c == '/') if(slashSeen && !forward) Some(nm) else go(nm, inSingleLineComment = true, slashSeen = true)
            else if (c == '\n') if(forward) Some(m) else None
            else go(nm, inSingleLineComment = true)
          } else if (inMultilineComment > 0) {
            if (c == '/') {
              if (starSeen) {
                if (inMultilineComment == 1) Some(nm)
                else go(nm, inMultilineComment = inMultilineComment - 1)
              } else {
                go(nm, inMultilineComment = inMultilineComment, slashSeen = true)
              }
            } else if (c == '*') {
              if (slashSeen) go(nm, inMultilineComment = inMultilineComment + 1)
              else go(nm, inMultilineComment = inMultilineComment, starSeen = true)
            } else {
              go(nm, inMultilineComment = inMultilineComment)
            }
          } else {
            if (c == '/') {
              if (slashSeen && forward) go(nm, inSingleLineComment = true)
              else go(nm, slashSeen = true)
            } else if(c == '*' && slashSeen) {
              go(nm, inMultilineComment = 1)
            } else {
              None
            }
          }
        }
      }

      if(!forward) go(sourceWithMarker.marker, inSingleLineComment = true).orElse(go(sourceWithMarker.marker, inSingleLineComment = false)).toSeq
      else go(sourceWithMarker.marker).toSeq
    }

    def inBrackets(open: Char, close: Char) = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      val br1 = if (forward) open else close

      if (sourceWithMarker.current != br1) {
        Nil
      } else {
        val toNextBr = {
          val toNextBr = until(character(open) | close, skipping = comment)

          if (forward) toNextBr
          else toNextBr.backward
        }

        @tailrec
        def go(sourceWithMarker: SourceWithMarker = sourceWithMarker.step(forward), level: Int = 1): Option[Int] = {
          sourceWithMarker.applyMovement(toNextBr) match {
            case Some(srcAtBr) =>
              val newLevel = {
                if (srcAtBr.current == br1) level + 1
                else level - 1
              }

              if (newLevel == 0) Some(nextMarker(srcAtBr.marker, forward))
              else go(srcAtBr.step(forward), newLevel)

            case None => None
          }

        }

        go().toSeq
      }
    }

    /**
     * Advances the marker until the given movement can be applied, optionally skipping parts of the input.
     *
     * Note that the movement specified by ''skipping'' is applied after ''mvnt''. This matters if
     * there is input that might be matched by both movements. Here is an example:
     * {{{
     * val src = SimpleMovement("0123456789".toCharArray)
     * src.moveMarker(until('5', skipping = digit))
     * }}}
     * will move the marker to ''5'', because ''5'' is matched before ''digit''. However doing
     * {{{
     * src.moveMarker(until('5', skipping = digit.zeroOrMore))
     * }}}
     * will leave the marker at ''0'', since ''digit.zeroOrMore'' will consume the entire string,
     * after matching ''5'' against ''0'' has failed.
     */
    def until(mvnt: Movement, skipping: Movement = none) = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      val (actualMvnt, actualSkipping) = {
        if (forward) (mvnt, skipping)
        else (mvnt.backward, skipping.backward)
      }

      @tailrec
      def go(sourceWithMarker: SourceWithMarker = sourceWithMarker): Option[Int] = {
        actualMvnt(sourceWithMarker) match {
          case Some(_) => Some(sourceWithMarker.marker)
          case _ =>
            if (sourceWithMarker.isDepleted) {
              None
            } else {
              val newSourceWithMarker = {
                val markerAfterSkipping = actualSkipping(sourceWithMarker).flatMap { markerAfterSkipping =>
                  if (markerAfterSkipping == sourceWithMarker.marker) None
                  else Some(markerAfterSkipping)
                }

                val newMarker = markerAfterSkipping.getOrElse(nextMarker(sourceWithMarker.marker, forward))
                sourceWithMarker.withMarkerAt(newMarker)
              }

              go(newSourceWithMarker)
            }
        }
      }

      go().toSeq
    }

    def charOfClass(inClass: Int => Boolean) = new SingleCharMovement(inClass)

    val space = charOfClass { c =>
      c == '\u0020' || c == '\u0009' || c == '\u000D' || c == '\u000A'
    }

    val letter = charOfClass { c =>
      c.isLower || c.isUpper || c.isTitleCase || c == '\u0024' || c == '$' || c == '\u005F' || c == '_' || {
        val ct = c.getType
        ct == Character.OTHER_LETTER || ct == Character.LETTER_NUMBER
      }
    }

    val digit = charOfClass(c => c.isDigit)

    val bracket = charOfClass { c =>
      c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
    }

    val delimiter = charOfClass { c =>
      c == '`' || c == '\'' || c == '"' || c == '.' || c == ';' || c == ','
    }

    val opChar = charOfClass { c =>
      (c >= '\u0020' && c <= '\u007F') || {
        val ct = c.getType
        ct == Character.MATH_SYMBOL || ct == Character.OTHER_SYMBOL
      }
    }.butNot(letter | digit | space | bracket | delimiter)

    val octalDigit = charOfClass { c =>
      c.isDigit && c != '8' && c != '9'
    }

    val characterLiteral = {
      val charEscape = character('b') | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\'
      val octEscape = octalDigit ~ octalDigit.atMostNtimes(2)

      '\'' ~ ((any.butNot('\\') | ('\\' ~ (charEscape | octEscape)))) ~ '\''
    }

    val stringLiteral = {
      val simpleLiteral = '"' ~ (('\\' ~ '"') | charOfClass(c => !c.isControl && c != '"')).zeroOrMore ~ '"'
      val multiLiteral = "\"\"\"" ~ ('"'.atMostNtimes(2) ~ any.butNot('"')).zeroOrMore ~ "\"\"\""

      multiLiteral | simpleLiteral
    }

    val op = opChar.atLeastOnce.refineWith { (sourceWithMarker, positions) =>
      // This refinement takes care of comments immediately after or before
      // operators, like `1 +/*<-do no evil*/2`:

      val goingBackward = positions.exists(_ < sourceWithMarker.marker)

      if (!goingBackward) {
        val positionsIncludingCommentStart = positions.filter { pos =>
          val covered = sourceWithMarker.source.slice(sourceWithMarker.marker, pos).mkString("")

          if (covered.size < 2) {
            false
          } else {
            covered.contains("//") || covered.contains("/*")
          }
        }.toSet

        positions.filterNot { pos =>
          positionsIncludingCommentStart.contains(pos) || positionsIncludingCommentStart.contains(pos + 1)
        }
      } else {
        val positionIncludesCommentStart = positions.exists { pos =>
          val coveredChars = sourceWithMarker.source.slice(pos + 1, sourceWithMarker.marker + 1)

          if (coveredChars.size < 2) {
            false
          } else {
            val c0 = coveredChars(0)
            val c1 = coveredChars(1)

            c0 == '/' && (c1 == '*' || c1 == '/')
          }
        }

        if (positionIncludesCommentStart) Seq()
        else positions
      }
    }

    private val upper = charOfClass(c => c.isUpper || c == '_')

    val idrest = (letter | digit).zeroOrMore ~ ('_' ~ op).zeroOrMore

    val varid = charOfClass(_.isLower) ~ idrest

    val plainid = (upper ~ idrest) | varid | op

    val symbolLiteral = '\'' ~ plainid

    val literalIdentifier = '`' ~ any.butNot('`').atLeastOnce ~ '`'

    val reservedName = string("abstract") | "case" | "catch" | "class" | "def" |
      "do" | "else" | "extends" | "false" | "final" |
      "finally" | "for" | "forSome" | "if" | "implicit" |
      "import" | "lazy" | "macro" | "match" | "new" |
      "null" | "object" | "override" | "package" | "private" |
      "protected" | "return" | "sealed" | "super" | "this" |
      "throw" | "trait" | "try" | "true" | "type" |
      "val" | "var" | "while" | "with" | "yield" |
      "_" | ":" | "=" | "=>" | "<-" | "<:" | "<%" | ">:" | "#" | "@" |
      "\u21D2" | "\u2190"

    val id = (plainid | literalIdentifier).butNot(reservedName | comment)

    val spaces: Movement = space.zeroOrMore
    val comments: Movement = comment.zeroOrMore
    val commentsAndSpaces: Movement = (comments ~ spaces).zeroOrMore
    val bracketsWithContents = inBrackets('[', ']')
    val curlyBracesWithContents = inBrackets('{', '}')

    implicit def charToMovement(c: Char): SingleCharMovement = character(c)
    implicit def stringToMovement(str: String): Movement  = string(str)
  }

  object MovementHelpers {
    def nextMarker(currentMarker: Int, forward: Boolean): Int = {
      if (forward) currentMarker + 1
      else currentMarker - 1
    }

    def wouldBeDepleted(potentialMarker: Int, sourceWithMarker: SourceWithMarker): Boolean = {
      !sourceWithMarker.isInRange(potentialMarker)
    }

    def doIfNotDepleted(sourceWithMarker: SourceWithMarker)(op: => Seq[Int]): Seq[Int] = {
      if (sourceWithMarker.isDepleted) Nil
      else op
    }

    def coveredChars(sourceWithMarker: SourceWithMarker, pos: Int): IndexedSeq[Char] = {
      val (from, until) = {
        if (sourceWithMarker.marker <= pos) (sourceWithMarker.marker, pos)
        else (pos + 1, sourceWithMarker.marker + 1)
      }

      sourceWithMarker.source.slice(from, until)
    }
  }
}
