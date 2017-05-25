package com.github.cdclaxton.FunctionCallParser

object ParameterType extends Enumeration {
  type ParameterType = Value
  val QUOTED_STRING, LITERAL, NUMERIC = Value
}

case class ParsedFunctionCall(functionName: String,
                              params: Seq[Parameter])

case class Parameter(tpe: ParameterType.Value,
                     value: String)

object FunctionCallParser {

  object State extends Enumeration {
    type State = Value
    val START, FUNCTION_NAME, OPENING_BRACKET, WHITESPACE,
    PARAM_QUOTED_OPEN, PARAM_QUOTED_CLOSED, PARAM_QUOTED_INNER,
    PARAM_LITERAL, PARAM_NUMERIC,
    ESCAPED_CHAR, COMMA, CLOSE_BRACKET, INVALID = Value
  }

  private[FunctionCallParser] def start(c: Char): State.Value = {
    if (!Character.isAlphabetic(c)) State.INVALID
    else State.FUNCTION_NAME
  }

  private[FunctionCallParser] def inFunctionName(c: Char): State.Value = {
    if (Character.isAlphabetic(c) || Character.isDigit(c) || c == '_') State.FUNCTION_NAME
    else if (c == '(') State.OPENING_BRACKET
    else State.INVALID
  }

  private[FunctionCallParser] def inOpeningBracket(c: Char): State.Value = {
    if (Character.isWhitespace(c)) State.WHITESPACE
    else if (c == ')') State.CLOSE_BRACKET
    else inStartParam(c)  // maybe it's the start of a parameter?
  }

  private[FunctionCallParser] def inWhitespace(c: Char): State.Value = {
    if (c == ',') State.COMMA
    else if (Character.isWhitespace(c)) State.WHITESPACE
    else if (c == ')') State.CLOSE_BRACKET
    else inStartParam(c)  // maybe it's the start of a parameter?
  }

  private[FunctionCallParser] def inStartParam(c: Char): State.Value = {
    if (c == '"') State.PARAM_QUOTED_OPEN
    else if (Character.isDigit(c) || c == '.') State.PARAM_NUMERIC
    else if (Character.isAlphabetic(c)) State.PARAM_LITERAL
    else State.INVALID
  }

  private[FunctionCallParser] def inParamQuotedOpen(c: Char): State.Value = {
    if (c == '"') State.PARAM_QUOTED_CLOSED
    else if (c == '\\') State.ESCAPED_CHAR
    else State.PARAM_QUOTED_INNER
  }

  private[FunctionCallParser] def inParamQuotedInner(c: Char): State.Value = {
    if (c == '"') State.PARAM_QUOTED_CLOSED
    else if (c == '\\') State.ESCAPED_CHAR
    else State.PARAM_QUOTED_INNER
  }

  private[FunctionCallParser] def inParamQuotedClosed(c: Char): State.Value = {
    if (Character.isWhitespace(c)) State.WHITESPACE
    else if (c == ',') State.COMMA
    else if (c == ')') State.CLOSE_BRACKET
    else State.INVALID
  }

  private[FunctionCallParser] def inEscapedChar(c: Char): State.Value = {
    State.PARAM_QUOTED_INNER
  }

  private[FunctionCallParser] def inParamNumeric(c: Char): State.Value = {
    if (Character.isWhitespace(c)) State.WHITESPACE
    else if (c == ',') State.COMMA
    else if (Character.isDigit(c) || c == '.') State.PARAM_NUMERIC
    else if (c == ')') State.CLOSE_BRACKET
    else State.INVALID
  }

  private[FunctionCallParser] def inParamLiteral(c: Char): State.Value = {
    if (Character.isWhitespace(c)) State.WHITESPACE
    else if (c == ',') State.COMMA
    else if (Character.isAlphabetic(c) || Character.isDigit(c)) State.PARAM_LITERAL
    else if (c == ')') State.CLOSE_BRACKET
    else State.INVALID
  }

  private[FunctionCallParser] def inComma(c: Char): State.Value = {
    if (Character.isWhitespace(c)) State.WHITESPACE
    else inStartParam(c)  // maybe it's the start of a parameter?
  }

  private[FunctionCallParser] def endOfParameter(state: State.Value) = {
    state == State.CLOSE_BRACKET ||
      state == State.WHITESPACE ||
      state == State.COMMA
  }

  def parseFunctionCall(functionCall: String): Option[ParsedFunctionCall] = {
    val result = parseFunctionCallDetailed(functionCall)
    if (result.isDefined) Some(result.get._1)
    else None
  }

  def parseFunctionCallDetailed(functionCall: String): Option[(ParsedFunctionCall, Int)] = {

    // Get the length of the function call
    val len = functionCall.length

    // If the length is insufficient for a valid call, then return.
    if (len < 3) return None

    // Break up the string into an array of characters so they can be walked over
    val arrayOfChars: Array[Char] = functionCall.toCharArray

    // Initialisation of stateful variables
    var prevState = State.INVALID
    var currentState = State.INVALID
    var c: Char = 'a'  // dummy char just for initialisation purposes
    var ptr: Int = 0
    var parsedFunctionCall = ParsedFunctionCall("", List[Parameter]())

    // Walk through each character
    for (i <- 0 until len) {

      prevState = currentState
      c = arrayOfChars(i)  // get the i(th) character

      // Find the new 'state' of the state machine based on the previous state and the i(th) character
      currentState = if (i == 0) start(c)
      else if (prevState == State.FUNCTION_NAME) inFunctionName(c)
      else if (prevState == State.OPENING_BRACKET) inOpeningBracket(c)
      else if (prevState == State.WHITESPACE) inWhitespace(c)
      else if (prevState == State.COMMA) inComma(c)
      else if (prevState == State.PARAM_QUOTED_OPEN) inParamQuotedOpen(c)
      else if (prevState == State.PARAM_QUOTED_INNER) inParamQuotedInner(c)
      else if (prevState == State.PARAM_QUOTED_CLOSED) inParamQuotedClosed(c)
      else if (prevState == State.ESCAPED_CHAR) inEscapedChar(c)
      else if (prevState == State.PARAM_LITERAL) inParamLiteral(c)
      else if (prevState == State.PARAM_NUMERIC) inParamNumeric(c)
      else State.INVALID

      println("STATE: " + currentState)

      // End of function name
      if (currentState == State.OPENING_BRACKET && prevState == State.FUNCTION_NAME) {
        parsedFunctionCall = parsedFunctionCall.copy(functionName = functionCall.substring(0, i))
        println("Got function name: " + parsedFunctionCall)
      }

      // Start of a parameter
      if ((currentState == State.PARAM_LITERAL || currentState == State.PARAM_NUMERIC || currentState == State.PARAM_QUOTED_OPEN) &&
        (!(prevState == State.PARAM_LITERAL || prevState == State.PARAM_NUMERIC || prevState == State.PARAM_QUOTED_OPEN))) {
        ptr = i
      }

      // End of a parameter
      if (endOfParameter(currentState)) {
        val parameterString = functionCall.substring(ptr, i)

        val param: Option[Parameter] = if (prevState == State.PARAM_LITERAL) Some(Parameter(ParameterType.LITERAL, parameterString))
          else if (prevState == State.PARAM_NUMERIC) Some(Parameter(ParameterType.NUMERIC, parameterString))
          else if (prevState == State.PARAM_QUOTED_CLOSED) {
            val str = parameterString.substring(1, parameterString.length-1)
            Some(Parameter(ParameterType.QUOTED_STRING, str))
          }

          else None

        if (param.isDefined) parsedFunctionCall = parsedFunctionCall.copy(params = parsedFunctionCall.params ++ List(param.get))
      }

      // End of function call
      if (currentState == State.CLOSE_BRACKET) {
        println("Returning: " + parsedFunctionCall)
        return Some((parsedFunctionCall, i))
      }

      // Something went wrong with the parsing ...
      if (currentState == State.INVALID) {
        println("INVALID STATE")
        return None
      }
    }

    None
  }
}
