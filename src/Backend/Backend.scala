package Backend

import Parser.Parser
import Ast._
import scala.collection.mutable.Map

object Backend {
  type GlobalVariables = Map[String, String]

  def resolveAssignment(g: GlobalVariables, variable: Variable,string:String): Unit = {
    variable match {
      case Variable(_,"") => printf("resolveAssignment(): variable name can NOT be empty.")
      case Variable(_,name) => g(name)=string
    }
  }

  def resolvePrint(g: GlobalVariables, variable: Variable): Unit = {
    variable match {
      case Variable(_,"") => printf("resolvePrint(): variable name can NOT be empty.")
      case Variable(_,name) => g get name match {
        case Some(string) => println(string)
        case None => printf("resolvePrint(): variable '$%s'not found.", name)
      }
    }
  }

  def resolveStatement(g: GlobalVariables, stmt: Statement):Unit = {
    stmt match {
      case Print(_, variable) => resolvePrint(g,variable)
      case Assignment( _ , variable, string ) => resolveAssignment(g,variable,string)
      case _ => printf("resolveStatement(): undefined statement type.")
    }
  }

  def resolveAST(g: GlobalVariables, ast: SourceCode): Unit = {
    def resolveASTAux(g:GlobalVariables, stmts:List[Statement]):Unit = {
      stmts match {
        case stmt::Nil => resolveStatement(g, stmt)
        case stmt::stmts_ => resolveStatement(g, stmt)
          resolveASTAux(g,stmts_)
      }
    }

    ast match{
      case SourceCode(_,Nil) => printf("resolveAST(): no code to execute, please check your input.")
        ()
      case SourceCode(_, statements) => resolveASTAux(g,statements)
    }
  }

  def execute(code:String): Unit ={
    var g = Map[String, String]()
    Parser.parse(code) match {
      case Some(ast) => resolveAST(g, ast)
      case None => ()
    }

  }

}
