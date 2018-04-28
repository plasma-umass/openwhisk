//Grammar
//e ::= c | [e1, e2, ..., en] | {str1: e1, ..., strn:en} | .complexPat | . | e1 == e2 | e1 != e2 | if e1 then e2 else e3
//complexPat ::= simplePat | simplePat . complexPat
//simplePat ::= [n] | id | [str]

package whisk.core.controller

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import scala.util.parsing.input.{Position, Reader}
import spray.json._

sealed trait Token extends Positional
class ConstantToken extends Token
class ConditionalOperatorToken extends Token
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
case object If extends Token
case object Then extends Token
case object Else extends Token
case object IsEqualToken extends ConditionalOperatorToken
case object IsNotEqualToken extends ConditionalOperatorToken

case class IdentifierToken (id: String) extends Token

trait ParsingError
case class TokenizerError(msg: String, private val cause: Throwable = None.orNull) extends Exception ("Tokenization Failed: " + msg, cause)

object Tokenizer extends RegexParsers {
  
  def bigBraceStart = "[" ^^ {_ => BigBraceStart}
  def bigBraceEnd = "]" ^^ {_ => BigBraceEnd}
  def curlyBraceStart = "{" ^^ {_ => CurlyBraceStart}
  def curlyBraceEnd = "}" ^^ {_ => CurlyBraceEnd}
  def patternDot = "." ^^ {_ => PatternDot}
  def keyValueSeparator = ":" ^^ {_ => KeyValueSeparator}
  def commaSeparator = "," ^^ {_ => CommaSeparator}
  def ifKeyword = "if" ^^ {_ => If}
  def elseKeyword = "else" ^^ {_ => Else}
  def thenKeyword = "then" ^^ {_ => Then}
  def isEqualToken = "==" ^^ {_ => IsEqualToken}
  def isNotEqualToken = "!=" ^^ {_ => IsNotEqualToken}
  
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
    phrase (rep1 (bigBraceStart   | bigBraceEnd       | 
                  curlyBraceStart | curlyBraceEnd     |
                  patternDot      | keyValueSeparator |
                  stringToken     | numberToken       | 
                  booleanToken    | nullToken         |
                  commaSeparator  | ifKeyword         | 
                  elseKeyword     | thenKeyword       | 
                  isEqualToken    | isNotEqualToken   | 
                  booleanToken    | nullToken         |
                  identifier))
  }
  
  def apply(code: String): Either [TokenizerError, List[Token]] = {
    System.out.println ("Doing Tokenization of code " + code)
    parse (tokens, code) match {
      case NoSuccess(msg, next) => Left(TokenizerError (msg))
      case Success(result, next) => Right (result)
    }
  }
}

case class TokenPosition (val col:Int) extends Position {
  override def column = col
  override def line = 0
  override def lineContents = ""
}

class TokenReader (tokens: List[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = new TokenPosition(3-tokens.length)
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
  override def drop (n: Int) = new TokenReader(tokens.drop(n))
}

object Operator extends Enumeration {
  type Operator = Value
  val IsEqual, IsNotEqual = Value
}

trait ASTNode
trait Expression extends ASTNode
trait Pattern extends ASTNode
trait Constant extends Expression
class ConditionalOperator (op: Operator.Value) extends ASTNode {
  def getOperator = op
}

case class NumberConstant (num : Int) extends Constant
case class BoolConstant (bool : Boolean) extends Constant
case class StringConstant (str : String) extends Constant
case class NullConstant () extends Constant

case class KeyValuePair (key: String, value: Expression) extends ASTNode
case class Identifier (id: String) extends Pattern

case class ConditionalExpression (exp1 : Expression, op: ConditionalOperator, exp2 : Expression) extends Expression
case class ArrayExpression (expressions : List [Expression]) extends Expression
case class JsonObjectExpression (map : List[KeyValuePair]) extends Expression
case class PatternExpression (pattern: Pattern) extends Expression
case class EmptyPatternExpression() extends Expression
case class IsEqualExpression(exp1 : Expression, exp2 : Expression) extends Expression
case class IsNotEqualExpression(exp1 : Expression, exp2 : Expression) extends Expression
case class IfThenElseExpression(cond : Expression, thenExp : Expression, elseExp : Expression) extends Expression

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
case class IfASTNode() extends ASTNode
case class ThenASTNode() extends ASTNode
case class ElseASTNode() extends ASTNode
case class IsEqualOperator() extends ConditionalOperator (Operator.IsEqual)
case class IsNotEqualOperator() extends ConditionalOperator (Operator.IsNotEqual)

case class DSLParsingError(msg: String, private val cause: Throwable = None.orNull) extends Exception ("Parsing Failed: " + msg, cause)

object DSLParser extends PackratParsers {
  override type Elem = Token
  
  lazy val stringConstant : PackratParser[StringConstant] = {
    accept ("string constant", {case StringToken(n) => StringConstant (n)})
  }
  
  lazy val identifier : PackratParser[Identifier] = {
    accept ("identifier", {case IdentifierToken(id) => Identifier (id)})
  }
  
  lazy val numberConstant : PackratParser[NumberConstant] = {
    accept ("number", {case n @ NumberToken (num) => NumberConstant (num.toInt)})
  }
  
  lazy val boolConstant : PackratParser[BoolConstant] = {
    accept ("boolean", {case b @ BooleanToken (bool) => if (bool == "true") BoolConstant(true) else BoolConstant(false)})
  }
  
  lazy val nullConstant : PackratParser[NullConstant] = {
    accept ("null", {case NullToken => NullConstant ()})
  }
  
  lazy val commaSeparatorASTNode : PackratParser[CommaSeparatorASTNode] = {
    accept (",", {case s @ CommaSeparator => CommaSeparatorASTNode ()})
  }
  
  lazy val bigBraceStartASTNode : PackratParser[BigBraceStartASTNode] = {
    accept ("[", {case BigBraceStart => BigBraceStartASTNode ()})
  }
  
  lazy val bigBraceEndASTNode : PackratParser[BigBraceEndASTNode] = {
    accept ("]", {case BigBraceEnd => BigBraceEndASTNode ()})
  }
  
  lazy val curlyBraceStartASTNode : PackratParser[CurlyBraceStartASTNode] = {
    accept ("{", {case CurlyBraceStart => CurlyBraceStartASTNode ()})
  }
  
  lazy val curlyBraceEndASTNode : PackratParser[CurlyBraceEndASTNode] = {
    accept ("}", {case CurlyBraceEnd => CurlyBraceEndASTNode ()})
  }
  
  lazy val patternDotASTNode : PackratParser[PatternDotASTNode] = {
    accept (".", {case PatternDot => PatternDotASTNode ()})
  }
  
  lazy val keyValueSeparatorASTNode : PackratParser[KeyValueSeparatorASTNode] = {
    accept (":", {case KeyValueSeparator => KeyValueSeparatorASTNode ()})
  }
  
  lazy val ifASTNode : PackratParser[IfASTNode] = {
    accept ("if", {case If => IfASTNode()})
  }
  
  lazy val elseASTNode : PackratParser[ElseASTNode] = {
    accept ("else", {case Else => ElseASTNode()})
  }
  
  lazy val thenASTNode : PackratParser[ThenASTNode] = {
    accept ("then", {case Then => ThenASTNode()})
  }
  
  lazy val isEqualOperator : PackratParser[IsEqualOperator] = {
    accept ("==", {case IsEqualToken => IsEqualOperator()})
  }
  
  lazy val isNotEqualOperator : PackratParser[IsNotEqualOperator] = {
    accept ("!=", {case IsNotEqualToken => IsNotEqualOperator()})
  }
  
  lazy val conditionalOperator : PackratParser[ConditionalOperator] = {
    isEqualOperator | isNotEqualOperator
  }

  lazy val conditionalExpression : PackratParser[ConditionalExpression] = {
    (expression ~ conditionalOperator ~ expression) ^^ {
      case exp1 ~ op ~ exp2 => ConditionalExpression (exp1, op, exp2)
    }
  }
  
  lazy val fieldPattern : PackratParser[FieldPattern] = {
    (identifier) ^^ {case (i @ Identifier(id)) => FieldPattern (id)}
  }
  
  lazy val stringPattern : PackratParser[StringPattern] = {
    (bigBraceStartASTNode ~ stringConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ StringConstant(str) ~ ebrace => StringPattern (str)
    }
  }
  
  lazy val arrayIndexPattern : PackratParser[ArrayIndexPattern] = {
    (bigBraceStartASTNode ~ numberConstant ~ bigBraceEndASTNode) ^^ {
      case sbrace ~ NumberConstant(n) ~ ebrace => ArrayIndexPattern (n.toInt)
    }
  }
  
  lazy val keyValuePair : PackratParser[KeyValuePair] = {
    (stringConstant ~ keyValueSeparatorASTNode ~ expression) ^^ {case StringConstant(str) ~ sep ~ exp => KeyValuePair (str, exp)}
  }
  
  lazy val arrayExpression : PackratParser[ArrayExpression] = {
    (bigBraceStartASTNode ~ rep1sep(expression, commaSeparatorASTNode) ~ bigBraceEndASTNode) ^^ {case sbrace ~ exps ~ ebrace => ArrayExpression (exps)}
  }
  
  lazy val jsonObjectExpression : PackratParser[JsonObjectExpression] = {
    (curlyBraceStartASTNode ~ rep1sep(keyValuePair, commaSeparatorASTNode) ~ curlyBraceEndASTNode) ^^ {case sbrace ~ kvpairs ~ ebrace => JsonObjectExpression (kvpairs)}
  }
  
  lazy val patternExpression : PackratParser[PatternExpression] = {
    (patternDotASTNode ~ complexPattern) ^^ {case dot ~ pat => PatternExpression (pat)}
  }
  
  lazy val emptyPatternExpression : PackratParser[EmptyPatternExpression] = {
    (patternDotASTNode) ^^ {case dot => EmptyPatternExpression ()}
  }
  
  //~ def dotPattern : Parser [DotPattern] = {
    //~ (patternDotASTNode ~ pattern) ^^ {case dot ~ pat => DotPattern (pat)}
  //~ }
  
  lazy val continuousPatterns : PackratParser[ContinuousPatterns] = {
    (log(simplePattern)("simplePatternInContinuousPattern") ~ log(patternDotASTNode)("patternDot") ~ log(complexPattern)("complexPattern")) ^^ { case pat1 ~ dot ~ pat2 => ContinuousPatterns (pat1, pat2)}
  }
  
  lazy val ifThenElseExpression : PackratParser[IfThenElseExpression] = {
    (ifASTNode ~ expression ~ thenASTNode ~ expression ~ elseASTNode ~ expression) ^^ {
      case _ ~ condExp ~ _ ~ exp1 ~ _ ~ exp2 => IfThenElseExpression(condExp, exp1, exp2)
    }
  }
  
  lazy val expression : PackratParser[Expression] = {
    ifThenElseExpression |
    log(conditionalExpression) ("conditionalExpression")|
    log (patternExpression) ("patternExpression")|
    stringConstant |
    boolConstant | nullConstant |
    log (numberConstant) ("numberConstant") |
    arrayExpression|
    jsonObjectExpression |
    emptyPatternExpression
  }
  
  lazy val simplePattern : PackratParser[Pattern] = {
    log(arrayIndexPattern)("arrayIndexPattern") | stringPattern | fieldPattern
  }
  
  lazy val complexPattern : PackratParser[Pattern] = {
    log(continuousPatterns) ("continuousPatterns") | log(simplePattern)("simplePattern")
    
  }
  
  lazy val result : PackratParser[Expression] = {
    phrase (expression)
  }
  
  def apply(tokens: List[Token]): Either [DSLParsingError, Expression] = {
    val reader = new TokenReader (tokens)
    result (new PackratReader(reader)) match {
      case NoSuccess(msg, next) => Left(DSLParsingError (msg + " " + next))
      case Success(result, next) => Right (result)
    }
  }
}

class DSLInterpreterException (private val msg: String, private val cause: Throwable = None.orNull) extends Exception (msg, cause)

sealed class ProjectionDSL () {
  def apply (code : String, jsValue : JsValue) : JsValue = {
    val lexerResult = Tokenizer (code)
    val tokens = lexerResult match {
      case Right(t) => t
      case Left(m) => throw m
    }
    
    System.out.println (s"Tokens are $tokens")
    val astNode = DSLParser (tokens) match {
      case Right(t) => t
      case Left(m) => throw m
    } 
    
    System.out.println (s"ASTNode after parsing is $astNode")
    
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
      case NullConstant () => JsNull
      case ArrayExpression (exprs) => JsArray ((exprs.map (expr => interpretExpression(expr, jsObject))).toVector)
      case JsonObjectExpression (keyVals) => {
        val map : Map [String, JsValue] = keyVals.map {case KeyValuePair (key, expr) => (key, interpretExpression (expr, jsObject))}.toMap
        JsObject (map)
      }
      case ConditionalExpression (e1, op, e2) => {
        val ret1 = interpretExpression (e1, jsObject)
        val ret2 = interpretExpression (e2, jsObject)
        op.getOperator match {
          case Operator.IsEqual => if (ret1 == ret2) JsTrue else JsFalse 
          case Operator.IsNotEqual => if (ret1 != ret2) JsTrue else JsFalse
        }        
      }
      case IfThenElseExpression(condExp, exp1, exp2) => {
        if (interpretExpression (condExp, jsObject) == JsTrue) {
          interpretExpression(exp1, jsObject)
        }
        else {
          interpretExpression(exp2, jsObject)
        }
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
