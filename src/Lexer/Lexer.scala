package Lexer
import Lexer.Token.Token

import java.lang.Character.isLetter
import scala.util.matching.Regex

class Lexer {
  var sourceCode = ""
  private var nextToken:String = ""
  private var lineNum : Int = 1
  private var nextTokenLineNum : Int = 0
  private var nextTokenType:Token = Token.EOF

  private val regexp: Regex = raw"^([_\d\w]+)".r

  def getLineNum():Int = lineNum

  def skipSourceCode(i: Int) = {
    sourceCode = sourceCode.substring(i)
  }

  def scanName():(String,Option[Token]) = {
    regexp.findFirstIn(sourceCode) match{
      case Some(name) => skipSourceCode(name.length())
        if (name =="print") { (name, Some(Token.PRINT)) }
        else { (name, None)}
      case _ => ("", None)
    }
  }

  def nextSourceCodeIs(str: String): Boolean = sourceCode.startsWith(str)

  def isIgnored(): Boolean = {
    def isIgnoredAux(b:Boolean, i: Int): Boolean ={
      if (i>= sourceCode.length) {
        skipSourceCode(i)
        b
      }else{
        if (nextSourceCodeIs("\r\n") || nextSourceCodeIs("\n\r")) {
          lineNum+=1
          isIgnoredAux(true,i+2)
        }else{
          sourceCode(i) match {
            case '\n' | '\r' =>
              lineNum+=1
              isIgnoredAux(true, i+1)
            case '\t' | '\f' | ' '=>
              isIgnoredAux(true, i+1)
            case _ => skipSourceCode(i)
              b
          }
        }
      }
    }
    isIgnoredAux(false,0)
  }

  def MatchToken():Option[(Int, Token.Value, String)] = {
    if (sourceCode.length ==0) {
      Some(lineNum, Token.EOF, "EOF")
    } else {
      sourceCode(0) match {
        case '$' => skipSourceCode(1)
          Some(lineNum, Token.VAR_PREFIX, "$")
        case '(' => skipSourceCode(1)
          Some(lineNum, Token.LEFT_PAREN, "(")
        case ')' => skipSourceCode(1)
          Some(lineNum, Token.RIGHT_PAREN, ")")
        case '=' => skipSourceCode(1)
          Some(lineNum, Token.EQUAL, "=")
        case '\"' =>
          if (nextSourceCodeIs("\"\"")) {
            skipSourceCode(2)
            Some(lineNum, Token.DUOQUOTE, "\"\"")
          } else {
            skipSourceCode(1)
            Some(lineNum, Token.QUOTE, "\"")
          }
        case s@_ => if (s == '_' || isLetter(s)) {
          scanName() match {
            case (token, Some(tokenType)) => Some(lineNum, tokenType, token)
            case (token, None) => Some(lineNum, Token.NAME, token)
          }
        } else if (isIgnored()) {
          Some(lineNum, Token.IGNORED, s.toString())
        } else {
          printf("MatchToken(): unexpected symbol near '%c' at line %n.", s.toString(), lineNum)
          None
        }
      }
    }
  }

  def scanBeforeToken(token:String):Option[String]={
    def scanBeforeTokenAux (token:String, i:Int):Option[String] = {
      if (i>= sourceCode.length) {
        printf("scanBeforeToken(): expect a(n) %s.", token)
        None
      }else if(sourceCode.startsWith(token,i)){
        val str = sourceCode.substring(0,i)
        skipSourceCode(i)
        Some(str)
      }else{
        scanBeforeTokenAux(token, i+1)
      }
    }
    scanBeforeTokenAux(token,0)
  }


  def GetNextToken():Option[(Int, Token, String)]={
    if (nextTokenLineNum >0) {
      lineNum=nextTokenLineNum
      nextTokenLineNum=0
      Some(lineNum, nextTokenType, nextToken)
    }else{
      MatchToken()
    }
  }

  def NextTokenIs(token: Token): Option[(Int, String)]= {
    GetNextToken() match {
      case Some((nowLineNum, nowTokenType, nowToken)) if (nowTokenType == token) => Some(nowLineNum,nowToken)
      case Some((_, nowTokenType, nowToken)) =>
        printf("NextTokenIs(): syntax error near '%s', expected token: {%s} but got {%s}.", nowToken, token, nowTokenType)
        None
      case None => None
    }
  }

  def LookAhead():Option[Token] = {
    if (nextTokenLineNum >0) {
      Some(nextTokenType)
    }else{
      val nowLineNum = lineNum
      GetNextToken() match {
        case Some((lineNum,tokenType, token)) =>
          this.lineNum = nowLineNum
          this.nextTokenType = tokenType
          this.nextToken = token
          this.nextTokenLineNum=lineNum
          Some(tokenType)
        case None => None
      }
    }
  }

  def LookAheadAndSkip(expectedTokenType:Token):Unit={
    val nowLineNum = lineNum
    GetNextToken() match {
      case Some((lineNum,tokenType, token)) if (tokenType != expectedTokenType) =>
        this.lineNum = nowLineNum
        this.nextTokenType = tokenType
        this.nextToken = token
        this.nextTokenLineNum=lineNum
      case _ => ()
    }
  }

}
