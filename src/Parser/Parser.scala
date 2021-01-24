package Parser

import Ast._
import Lexer.{Lexer, Token}

import scala.language.postfixOps

object Parser {
  var lexer = new Lexer()

  def parseName():Option[String] ={
    lexer.NextTokenIs(Token.NAME) match {
      case Some((_, name)) => Some(name)
      case _ =>
        None
    }
  }

  def parseString():Option[String] ={
    lexer.LookAhead() match {
      case Some(Token.DUOQUOTE) =>
        lexer.NextTokenIs(Token.DUOQUOTE)
        lexer.LookAheadAndSkip(Token.IGNORED)
        Some("")
      case Some(Token.QUOTE)=>
        lexer.NextTokenIs(Token.QUOTE)
        lexer.scanBeforeToken("\"") match {
          case Some(str) =>
            lexer.NextTokenIs(Token.QUOTE)
            lexer.LookAheadAndSkip(Token.IGNORED)
            Some(str)
          case None => None
        }
      case _ => printf("parseString(): not a string.")
        None
    }
  }

  def parseVariable():Option[Variable]={
    val lineNum = lexer.getLineNum()
    lexer.NextTokenIs(Token.VAR_PREFIX) match {
      case Some(_) =>
        parseName() match{
          case Some(name) => lexer.LookAheadAndSkip(Token.IGNORED)
            Some(Variable(lineNum,name))
          case _ =>
            printf("parseVariable(): expect a variable name at line %n", lineNum)
            None
        }
      case None =>
        printf("parseVariable(): expect a \"$\" at line %n", lineNum)
        None
    }
  }

  def parseAssignment(): Option[Assignment] = {
    val lineNum = lexer.getLineNum()
    parseVariable() match{
      case Some(variable) => lexer.LookAheadAndSkip(Token.IGNORED)
        lexer.NextTokenIs(Token.EQUAL) match {
          case Some(_) => lexer.LookAheadAndSkip(Token.IGNORED)
            parseString() match{
              case Some(str) => lexer.LookAheadAndSkip(Token.IGNORED)
                Some(Assignment(lineNum,variable,str))
              case _ => None
            }
          case None => None
        }
      case None => None
    }
  }

  def parsePrint(): Option[Print] = {
    val lineNum = lexer.getLineNum()
    lexer.NextTokenIs(Token.PRINT) match {
      case Some(_) =>
        lexer.NextTokenIs(Token.LEFT_PAREN) match {
          case Some(_) => lexer.LookAheadAndSkip(Token.IGNORED)
            parseVariable() match {
              case Some(variable) => lexer.LookAheadAndSkip(Token.IGNORED)
                lexer.NextTokenIs(Token.RIGHT_PAREN) match {
                  case Some(_) => lexer.LookAheadAndSkip(Token.IGNORED)
                    Some(Print(lineNum, variable))
                  case None => None
                }
              case None => None
            }
          case None => None
        }
      case None => None
    }
  }

  def parseStatement():Option[Statement] = {
    lexer.LookAhead() match{
      case Some(Token.PRINT) => parsePrint()
      case Some(Token.VAR_PREFIX) => parseAssignment()
      case _ => printf("parseStatement(): unknown Ast.Statement.")
        None
    }
  }

  def parseStatements():Option[List[Statement]]= {
    def parseStatementsAux(statements:List[Statement]): Option[List[Statement]] = {
      lexer.LookAhead() match {
        case Some(Token.EOF) => Some(statements)
        case Some(_) => parseStatement() match {
          case Some(stmt) => parseStatementsAux(statements::: List(stmt))
          case None => None
        }
        case None => None
      }
    }
    parseStatementsAux(Nil)
  }

  def parseSourceCode():Option[SourceCode]= {
    val lineNum = lexer.getLineNum()
    parseStatements() match{
      case Some(stmts) => Some(SourceCode(lineNum,stmts))
      case None => None
    }
  }

  def parse(code:String):Option[SourceCode] = {
    lexer.sourceCode = code
    parseSourceCode() match {
      case Some(sc) => lexer.NextTokenIs(Token.EOF)
        Some(sc)
      case None => None
    }
  }

}
