//Grammar
//e ::= c | [e1, e2, ..., en] | {str1: e1, ..., strn:en} | .complexPat | .
//complexPat ::= simplePat | simplePat . complexPat
//simplePat ::= [n] | id | [str]

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait Token extends Positional
class ConstantToken extends Token
case class NumberToken (num :String) extends ConstantToken
case class BooleanToken (bool: String) extends ConstantToken
case class StringToken (str : String) extends ConstantToken
case object NullToken  extends ConstantToken

case object BigBraceStart extends Token // [
case object BigBraceEnd extends Token // ]
case object CurlyBraceStart extends Token // {
case object CurlyBraceEnd extends Token // }
case object PatternDot extends Token //.
case object KeyValueSeparator extends Token //:
case object CommaSeparator extends Token

case class IdentifierToken (id: String) extends Token

trait ParsingError
case class TokenizerError(msg: String) extends ParsingError

object Tokenizer extends RegexParsers {
  
  def bigBraceStart = "[" ^^ {_ => BigBraceStart}
  def bigBraceEnd = "]" ^^ {_ => BigBraceEnd}
  def curlyBraceStart = "{" ^^ {_ => CurlyBraceStart}
  def curlyBraceEnd = "}" ^^ {_ => CurlyBraceEnd}
  def patternDot = "." ^^ {_ => PatternDot}
  def keyValueSeparator = ":" ^^ {_ => KeyValueSeparator}
  def commaSeparator = "," ^^ {_ => CommaSeparator}
  
  def identifier: Parser[IdentifierToken] = {
    //TODO: null is recognized as identifier
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IdentifierToken(str) }
  }


  def stringToken : Parser[StringToken] = {
    """"[^"]*"""".r ^^ { str =>
      StringToken (str.substring (1, str.length - 1))
    }
  }
  
  def numberToken : Parser[NumberToken] = {
    """\d+""".r ^^ { num => 
      NumberToken (num)
    }
  }
  
  def booleanToken : Parser[BooleanToken] = {
    """(false|true)""".r ^^ { bool =>
      BooleanToken (bool)
    }
  }
  
  def nullToken = "null" ^^ (_ => NullToken)
  
  def tokens:Parser[List[Token]] = {
    phrase (rep1 (bigBraceStart | bigBraceEnd | curlyBraceStart |
                  curlyBraceEnd | patternDot | keyValueSeparator |
                  identifier | stringToken | numberToken | booleanToken |
                  nullToken | commaSeparator))
  }
  
  def apply(code: String): Either [TokenizerError, List[Token]] = {
    parse (tokens, code) match {
      case NoSuccess(msg, next) => Left(TokenizerError (msg))
      case Success(result, next) => Right (result)
    }
  }
}

class TokenReader (tokens: List[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

trait ASTNode
trait Expression extends ASTNode
trait Pattern extends ASTNode
trait Constant extends Expression

case class NumberConstant (num : String) extends Constant
case class BoolConstant (bool : String) extends Constant
case class StringConstant (str : String) extends Constant
case object NullConstant extends Constant

case class KeyValuePair (key: StringConstant, value: Expression) extends ASTNode
case class Identifier (id: String) extends ASTNode

case class ArrayExpression (expressions : List [Expression]) extends Expression
case class JsonObjectExpression (map : List[KeyValuePair]) extends Expression
case class PatternExpression (pattern: Pattern) extends Expression
case class EmptyPatternExpression() extends Expression

case class ArrayIndexPattern (index : NumberConstant) extends Pattern
case class FieldPattern (field : Identifier) extends Pattern
case class StringPattern (str : StringConstant) extends Pattern
case class ContinuousPatterns (pattern1 : Pattern, pattern2: Pattern) extends Pattern
case class DotPattern (pattern: Pattern) extends Pattern

case class BigBraceStartASTNode () extends ASTNode // [
case class BigBraceEndASTNode () extends ASTNode // ]
case class CurlyBraceStartASTNode () extends ASTNode // {
case class CurlyBraceEndASTNode () extends ASTNode // }
case class PatternDotASTNode () extends ASTNode //.
case class KeyValueSeparatorASTNode () extends ASTNode //:
case class CommaSeparatorASTNode () extends ASTNode //:

case class DSLParsingError(msg: String) extends ParsingError

object DSLParser extends Parsers {
  override type Elem = Token
  
  def stringConstant : Parser[StringConstant] = {
    accept ("string constant", {case StringToken(n) => StringConstant (n)})
  }
  
  def identifier : Parser[Identifier] = {
    accept ("identifier", {case IdentifierToken(id) => Identifier (id)})
  }
  
  def numberConstant : Parser[NumberConstant] = {
    accept ("number", {case n @ NumberToken (num) => NumberConstant (num)})
  }
  
  def commaSeparatorASTNode : Parser[CommaSeparatorASTNode] = {
    accept (",", {case s @ CommaSeparator => CommaSeparatorASTNode ()})
  }
  
  def bigBraceStartASTNode : Parser[BigBraceStartASTNode] = {
    accept ("[", {case BigBraceStart => BigBraceStartASTNode ()})
  }
  
  def bigBraceEndASTNode : Parser[BigBraceEndASTNode] = {
    accept ("]", {case BigBraceEnd => BigBraceEndASTNode ()})
  }
  
  def curlyBraceStartASTNode : Parser[CurlyBraceStartASTNode] = {
    accept ("{", {case CurlyBraceStart => CurlyBraceStartASTNode ()})
  }
  
  def curlyBraceEndASTNode : Parser[CurlyBraceEndASTNode] = {
    accept ("}", {case CurlyBraceEnd => CurlyBraceEndASTNode ()})
  }
  
  def patternDotASTNode : Parser[PatternDotASTNode] = {
    accept (".", {case PatternDot => PatternDotASTNode ()})
  }
  
  def keyValueSeparatorASTNode : Parser[KeyValueSeparatorASTNode] = {
    accept (":", {case KeyValueSeparator => KeyValueSeparatorASTNode ()})
  }
  
  def fieldPattern : Parser[FieldPattern] = {
    (identifier) ^^ {case (i @ Identifier(id)) => FieldPattern (i)}
  }
  
  def stringPattern : Parser[StringPattern] = {
    (bigBraceStartASTNode ~ stringConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ str ~ ebrace => StringPattern (str)
    }
  }
  
  def arrayIndexPattern : Parser[ArrayIndexPattern] = {
    (bigBraceStartASTNode ~ numberConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ n ~ ebrace => ArrayIndexPattern (n)
    }
  }
  
  def keyValuePair : Parser[KeyValuePair] = {
    (stringConstant ~ keyValueSeparatorASTNode ~ expression) ^^ {case str ~ sep ~ exp => KeyValuePair (str, exp)}
  }
  
  def arrayExpression : Parser[ArrayExpression] = {
    (bigBraceStartASTNode ~ rep1sep(expression, commaSeparatorASTNode) ~ bigBraceEndASTNode) ^^ {case sbrace ~ exps ~ ebrace => ArrayExpression (exps)}
  }
  
  def jsonObjectExpression : Parser[JsonObjectExpression] = {
    (curlyBraceStartASTNode ~ rep1sep(keyValuePair, commaSeparatorASTNode) ~ curlyBraceEndASTNode) ^^ {case sbrace ~ kvpairs ~ ebrace => JsonObjectExpression (kvpairs)}
  }
  
  def patternExpression : Parser[PatternExpression] = {
    (patternDotASTNode ~ complexPattern) ^^ {case dot ~ pat => PatternExpression (pat)}
  }
  
  def emptyPatternExpression : Parser[EmptyPatternExpression] = {
    (patternDotASTNode) ^^ {case dot => EmptyPatternExpression ()}
  }
  
  //~ def dotPattern : Parser [DotPattern] = {
    //~ (patternDotASTNode ~ pattern) ^^ {case dot ~ pat => DotPattern (pat)}
  //~ }
  
  def continuousPatterns : Parser[ContinuousPatterns] = {
    (simplePattern ~ patternDotASTNode ~ complexPattern) ^^ { case pat1 ~ dot ~ pat2 => ContinuousPatterns (pat1, pat2)}
  }
  
  def expression : Parser[Expression] = {
    //TODO:Add Boolean and Null Constant
    stringConstant | numberConstant | arrayExpression | jsonObjectExpression |
    patternExpression | emptyPatternExpression
  }
  
  def simplePattern : Parser[Pattern] = {
    arrayIndexPattern | stringPattern | fieldPattern
  }
  
  def complexPattern : Parser[Pattern] = {
    continuousPatterns | simplePattern
  }
  
  def result : Parser[Expression] = {
    phrase (expression)
  }
  
  def apply(tokens: List[Token]): Either [DSLParsingError, Expression] = {
    val reader = new TokenReader (tokens)
    result (reader) match {
      case NoSuccess(msg, next) => Left(DSLParsingError (msg))
      case Success(result, next) => Right (result)
    }
  }
}

object HelloWorld {
    def example() = {
      //val q = """{"x":1, "y":22}"""
      val q = """[1,2,3,4,5]"""
      //val q = """.[0]"""
      //val q = """."""
      //val q = """.[0].[1].[2].id.["xx"]"""
      System.out.println (q)
        val l = Tokenizer (q)
        System.out.println (l)
        System.out.println (l match {
          case Right (t) => DSLParser (t)
          case Left (t) => t
        })
    }
}
