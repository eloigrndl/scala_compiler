/*
package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }

  def opPos(string: String): Syntax[(String, Position)] = accept(OperatorKind(string)) { 
    case op@OperatorToken(v) => (v, op.position)
  }

  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
    case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
    functionDefinition | abstractClassDefinition | caseClassDefinition
   
  lazy val functionDefinition: Syntax[ClassOrFunDef] = 
    (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ "=" ~ "{" ~ expr ~ "}").map {
      case kw ~ id ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ _ ~ body ~ _ => FunDef(id, params, retType, body).setPos(kw) 
    }
  
  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifier).map {
      case kw ~ _ ~ id => AbstractClassDef(id).setPos(kw)
    }
  
  lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map {
      // Raoul : TODO!!! What is the fields? I believe params is a list of ParamDef, not a list of TypeTree
      case kw ~ _ ~ id ~ _ ~ params ~ _ ~ _ ~ parent => CaseClassDef(id, params.map(_.tt), parent).setPos(kw)
    }

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] = 
    (identifierPos ~ ":" ~ typeTree).map {
      case (id,pos) ~ _ ~ tt => ParamDef(id, tt).setPos(pos)
    }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] = (identifierPos ~ opt("." ~ identifier)).map {
    case (id, pos) ~ None => TypeTree(ClassType(QualifiedName(None, id))).setPos(pos)
    case (mod, pos) ~ Some(_ ~ id) => TypeTree(ClassType(QualifiedName(Some(mod), id))).setPos(pos)
  }
  
  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive { 
    valOrSemiCol
    // identifierPos | literal | binaryOp | unaryOp | identifierType ~ '(' ~ params ~ ')' | seqOp | assignOp | iteOp | matchOp | errorOp | ??? // TODO
  }
  //                                            [ Id . ]? Id ( Args ) |               val ParamDef = Expr ; Expr ???          | ( Expr )

  
  lazy val assignOp : Syntax[Expr] = (kw("val") ~ parameter ~ "=" ~ ifOrMatch ~ ";" ~ expr).map {
    case kw ~ param ~ _ ~ value ~ _ ~ next => Let(param, value, next).setPos(kw)
    /* Note: Although Amy spec and the syntax of Let hint at always having a follow up to an assignment, should we wish to allow a last-line assignment, add opt() around (; ~ expr) and use the two commented lines below
    case kw ~ param ~ _ ~ value ~ None => Let(param, value, UnitLiteral()).setPos(kw)
    case kw ~ param ~ _ ~ value ~ Some(_ ~ next) => Let(param, value, next).setPos(kw)
    */
  }

  lazy val valOrSemiCol: Syntax[Expr] = assignOp | semiCol

  //lazy val valE: Syntax[Expr] = kw("val") ~ identifier ~ "="

  lazy val semiCol: Syntax[Expr] = (ifOrMatch ~ opt(";" ~ expr)).map {
    case e1 ~ None => e1.setPos(e1)
    case e1 ~ Some(_ ~ e2) => Sequence(e1, e2).setPos(e1)
  }
  
  lazy val matchForm: Syntax[Seq[MatchCase]] = (kw("match") ~ "{" ~ many1(matchCase) ~ "}").map{
    case kw ~ _ ~ cases ~ _ => cases
  }

  lazy val ifOrMatch: Syntax[Expr] = ((iteOp | binaryOp) ~ opt(many1(matchForm))).map {
    case b ~ None => b.setPos(b)
    case b ~ Some(matches) => {
      recursive(Match(b, matches.head.toList), matches.tail.toList).setPos(b)
    }
  }

  def recursive(result : Match, matches: List[Seq[MatchCase]]) : Match = {
    matches match {
      case Nil => result
      case _ => recursive(Match(result, matches.head.toList), matches.tail)
    }
  }
    
  

  // (expr) verify
  lazy val iteOp: Syntax[Expr] = (kw("if") ~ "(" ~ expr ~ ")" ~ "{" ~ expr ~ "}" ~ kw("else") ~ "{" ~ expr ~ "}").map {
    case kw ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ _ ~ _ ~ elze ~ _ => Ite(cond, thenn, elze).setPos(kw)
  }

  lazy val matchCase: Syntax[MatchCase] = (kw("case") ~ pattern ~ "=>" ~ expr).map{
    case kw ~ pattern ~ _ ~ expr => MatchCase(pattern, expr)
  }  
  
  //lazy val caseC: Syntax[Expr] = (case x =>)


  

  lazy val unaryOp: Syntax[Expr] = (opt((opPos("!") | opPos("-"))) ~ simpleExpr).map {
    case Some(("!", pos)) ~ e => Not(e).setPos(pos)
    case Some(("-", pos)) ~ e => Neg(e).setPos(pos)
    case None ~ e => e.setPos(e)
  }

  lazy val binaryOp: Syntax[Expr] =
    operators(unaryOp)(
      // Defines the different operators, by decreasing priority.
      //Unary operators are need to define
      op("*") | op("/") | op("%") is LeftAssociative,
      op("+") | op("-") | op("++") is LeftAssociative,
      op("<")| op("<=") is LeftAssociative,
      op("==") is LeftAssociative,
      op("&&") is LeftAssociative,
      op("||") is LeftAssociative,
    ) {
      case (e1, "+", e2)  => Plus(e1, e2).setPos(e1)
      case (e1, "-", e2)  => Minus(e1, e2).setPos(e1)
      case (e1, "*", e2)  => Times(e1, e2).setPos(e1)
      case (e1, "/", e2)  => Div(e1, e2).setPos(e1)
      case (e1, "%", e2)  => Mod(e1, e2).setPos(e1)
      case (e1, "<", e2)  => LessThan(e1, e2).setPos(e1)
      case (e1, "<=", e2) => LessEquals(e1, e2).setPos(e1)
      case (e1, "&&", e2) => And(e1, e2).setPos(e1)
      case (e1, "||", e2) => Or(e1, e2).setPos(e1)
      case (e1, "==", e2) => Equals(e1, e2).setPos(e1)
      case (e1, "++", e2) => Concat(e1, e2).setPos(e1)
    }

  lazy val errorOp: Syntax[Expr] = (kw("error") ~ "(" ~ expr ~ ")").map {
    case kw ~ _ ~ msg ~ _ => Error(msg).setPos(kw)
  }
  
  // A literal expression. //TODO: add positions
  lazy val literal: Syntax[Literal[_]] = accept(LiteralKind){
    case boolean@BoolLitToken(value) => BooleanLiteral(value).setPos(boolean)
    case integer@IntLitToken(value) => IntLiteral(value).setPos(integer)
    case string@StringLitToken(value) => StringLiteral(value).setPos(string)
    //case "(" ~ ")" => UnitLiteral() //FIXME: not sure about this
  }

  lazy val unitLitOrParanth = ("(" ~ opt(expr) ~ ")").map {
    case _ ~ None ~ _ => UnitLiteral()
    case _ ~ Some(e) ~ _ => e
  }
  
  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | caseClassPattern | unitPattern
  }

  lazy val literalPattern: Syntax[Pattern] = literal.map{
    l => LiteralPattern(l)
  }
  
  lazy val wildPattern: Syntax[Pattern] = kw("_").map{
    case _ => WildcardPattern()
  }  

  lazy val unitPattern: Syntax[Pattern] = ("(" ~ ")").map {
    case parenthesis ~_ => LiteralPattern(UnitLiteral()).setPos(parenthesis);
  }

  //TODO: need to require stuff inside paranthesis?
  lazy val caseClassPattern: Syntax[Pattern] = 
    (identifierPos ~ opt("." ~ identifier) ~ opt("(" ~ repsep(pattern, ",") ~ ")")).map{
      // case abc 
      case (id, pos) ~ None ~ None                            => IdPattern(id).setPos(pos)
      // case List(head,tail)
      case (id, pos) ~ None ~ Some(_ ~ patterns ~ _)          => CaseClassPattern(QualifiedName(None, id), patterns.toList).setPos(pos)
      // case Std.Lib
      case (mod, pos) ~ Some(_ ~ id) ~ None                   => CaseClassPattern(QualifiedName(Some(mod), id), Nil).setPos(pos)
      // case Std.List(head,tail)
      case (mod, pos) ~ Some(_ ~ id) ~ Some(_ ~ patterns ~ _) => CaseClassPattern(QualifiedName(Some(mod), id), patterns.toList).setPos(pos)
    }
    
 
  
  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | unitLitOrParanth | errorOp
  
  //Almost same as Case Class Pattern //TODO: call with no args
  lazy val variableOrCall: Syntax[Expr] = (identifierPos ~ opt("." ~ identifier) ~ opt("(" ~ repsep(expr, ",") ~ ")")).map {
      // case abc 
      case (id, pos) ~ None ~ None                            => Variable(id).setPos(pos)
      // case add(a,b)
      case (id, pos) ~ None ~ Some(_ ~ params ~ _)            => Call(QualifiedName(None, id), params.toList).setPos(pos)
      // case add() 
      case (mod, pos) ~ Some(_ ~ id) ~ None                   => Call(QualifiedName(Some(mod), id), Nil).setPos(pos)
      // case Std.List(head,tail)
      case (mod, pos) ~ Some(_ ~ id) ~ Some(_ ~ params ~ _)   => Call(QualifiedName(Some(mod), id), params.toList).setPos(pos)
    
  }
  
  
  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => {
        Console.err.println("Print from parser")
        result
      }
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
*/