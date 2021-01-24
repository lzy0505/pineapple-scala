package Lexer

object Token extends Enumeration {
  type Token = Value
  val EOF, VAR_PREFIX, LEFT_PAREN, RIGHT_PAREN,
  EQUAL, QUOTE, DUOQUOTE, NAME, PRINT, IGNORED = Value
}
