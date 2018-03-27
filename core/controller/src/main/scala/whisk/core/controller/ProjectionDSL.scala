//Grammar
//e ::= c | [e1, e2, ..., en] | {str1: e1, ..., strn:en} | .complexPat | .
//complexPat ::= simplePat | simplePat . complexPat
//simplePat ::= [n] | id | [str]

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import scala.util.parsing.input.{NoPosition, Position, Reader}
import spray.json._
import DefaultJsonProtocol._ 

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

case class NumberConstant (num : Int) extends Constant
case class BoolConstant (bool : Boolean) extends Constant
case class StringConstant (str : String) extends Constant
case object NullConstant extends Constant

case class KeyValuePair (key: String, value: Expression) extends ASTNode
case class Identifier (id: String) extends Pattern

case class ArrayExpression (expressions : List [Expression]) extends Expression
case class JsonObjectExpression (map : List[KeyValuePair]) extends Expression
case class PatternExpression (pattern: Pattern) extends Expression
case class EmptyPatternExpression() extends Expression

case class ArrayIndexPattern (index : Int) extends Pattern
case class FieldPattern (field : String) extends Pattern
case class StringPattern (str : String) extends Pattern
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
    accept ("number", {case n @ NumberToken (num) => NumberConstant (num.toInt)})
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
    (identifier) ^^ {case (i @ Identifier(id)) => FieldPattern (id)}
  }
  
  def stringPattern : Parser[StringPattern] = {
    (bigBraceStartASTNode ~ stringConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ StringConstant(str) ~ ebrace => StringPattern (str)
    }
  }
  
  def arrayIndexPattern : Parser[ArrayIndexPattern] = {
    (bigBraceStartASTNode ~ numberConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ NumberConstant(n) ~ ebrace => ArrayIndexPattern (n.toInt)
    }
  }
  
  def keyValuePair : Parser[KeyValuePair] = {
    (stringConstant ~ keyValueSeparatorASTNode ~ expression) ^^ {case StringConstant(str) ~ sep ~ exp => KeyValuePair (str, exp)}
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

class DSLInterpreterException (private val msg: String, private val cause: Throwable = None.orNull) extends Exception (msg, cause)

object DSLInterpreter {
  def apply (astNode : ASTNode, jsValue : JsValue) : JsValue = {
    astNode match {
      case exp : Expression => interpretExpression (exp, jsValue)
      //case pat : Pattern => interpretPattern (pat, jsObject)
      case _ => throw new IllegalArgumentException ("Not handling this ASTNode in interpreter $astNode")
    }
  }
  
  def interpretExpression (expr : Expression, jsObject: JsValue) : JsValue = {
    expr match {
      case NumberConstant (num) => JsNumber (num)
      case StringConstant (str) => JsString (str)
      case BoolConstant (bool) => JsBoolean (bool)
      case NullConstant => JsNull
      case ArrayExpression (exprs) => JsArray (exprs.map (expr => interpretExpression(expr, jsObject)))
      case JsonObjectExpression (keyVals) => {
        val map : Map [String, JsValue] = keyVals.map {case KeyValuePair (key, expr) => (key, interpretExpression (expr, jsObject))}.toMap
        JsObject (map)
      }
      case PatternExpression (pattern) => interpretPattern (pattern, jsObject)
      case EmptyPatternExpression () => jsObject
      case _ => throw new IllegalArgumentException ("Not handling this expression") 
    }
  }
  
  private def jsValueToJsObject (jsValue: JsValue) : JsObject = {
    try {
      jsValue.asInstanceOf[JsObject]
    }
    catch {
      case e : Exception => throw new DSLInterpreterException (s"Cannot convert $jsValue to JsObject")
    }
  }
  
  def interpretPattern (pat: Pattern, jsValue: JsValue) : JsValue = {
    pat match {
      //~ case Identifier (id) => {
        //~ val JsObject (mapJsObject : Map[String, JsValue]) = jsValueToJsObject (jsValue)
        //~ if (!(mapJsObject contains id)) {
          //~ throw new DSLInterpreterException (s"$id not found in JsObject $jsValue")
        //~ }
        //~ mapJsObject (id)
      //~ }
      case ArrayIndexPattern (index) => {
        try {
          val jsArray = jsValue.asInstanceOf [JsArray]
          val JsArray (elements) = jsArray
          elements (index)
        }
        catch {
          case ex : java.lang.ClassCastException => throw new DSLInterpreterException (s"$jsValue is not Array. Error interpreting ArrayIndexPattern($index)")
        }
      }
      
      case FieldPattern (field) => {
        val JsObject (mapJsObject : Map[String, JsValue]) = jsValueToJsObject (jsValue)
        try {
          mapJsObject (field)
        }
        catch {
          case e: Exception => throw new DSLInterpreterException (s"Field $field not present in JsObject $mapJsObject")
        }
      }
      
      case StringPattern (str) => {
        val JsObject (mapJsObject : Map[String, JsValue]) = jsValueToJsObject (jsValue)
        try {
          mapJsObject (str)
        }
        catch {
          case e: Exception => throw new DSLInterpreterException (s"StringPattern $str not present in JsObject $mapJsObject")
        }
      }
      case ContinuousPatterns (pattern1, pattern2) => interpretPattern (pattern2, interpretPattern (pattern1, jsValue))
      case DotPattern (pattern) => interpretPattern (pattern, jsValue)
      case _ => throw new IllegalArgumentException ("Not handling this pattern") 
    }
  }
}
