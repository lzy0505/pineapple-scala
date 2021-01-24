package Ast

case class Assignment(lineNum: Int, variable: Variable, string: String) extends Statement
