package com.github.cdclaxton.FunctionCallParser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class FunctionCallParserTest extends FlatSpec with Matchers {

  // No arguments
  // -------------------------------------------------------------------------------------------------------------------

  "parseFunctionCall()" should "work correctly when there aren't any parameters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f()""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "f", params = Nil))
  }

  it should "work correctly when there aren't any parameters and the function name has two characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn()""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn", params = Nil))
  }

  it should "work correctly when there aren't any parameters and there is trailing whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn()  """)
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn", params = Nil))
  }

  it should "work correctly when the function name contains a digit" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f1()""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "f1", params = Nil))
  }

  it should "work correctly when the function name contains an underscore" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f_g()""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "f_g", params = Nil))
  }

  // Numeric-only arguments
  // -------------------------------------------------------------------------------------------------------------------

  it should "handle a single numeric value" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(0)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "0"))))
  }

  it should "handle a double digit numeric value" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(12)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "12"))))
  }

  it should "handle a triple digit numeric value" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(123)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "123"))))
  }

  it should "handle a number preceded with a single whitespace character" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn( 123)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "123"))))
  }

  it should "handle a number preceded with two whitespace characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(  123)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "123"))))
  }

  it should "handle a number with a trailing whitespace character" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(123 )""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "123"))))
  }

  it should "handle a number with two trailing whitespace characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(123  )""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "123"))))
  }

  it should "handle a single negative numeric value" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(-1)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.NUMERIC, value = "-1"))))
  }

  it should "handle a function with two numeric arguments" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(1,2)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "1"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
  }

  it should "handle a function with two numeric arguments separated with whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(1, 2)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "1"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
  }

  it should "handle a function with three numeric arguments" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(1,2,3)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "1"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"),
        Parameter(tpe = ParameterType.NUMERIC, value = "3"))))
  }

  it should "handle a scientific notation numeric argument" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(1.2e3)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "1.2e3"))))
  }

  it should "handle a negative number in scientific notation" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(-1.2E3)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "-1.2E3"))))
  }

  it should "handle two arguments in scientific notation" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(-1.2E3, 2.9e10)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "-1.2E3"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2.9e10"))))
  }

  it should "handle four arguments in scientific notation" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""mdr(-4.566476837630887E-37, 1.590256498E9, 7.5888587E8, 1.1527215368903861E269)""")
    result.isDefined should be (true)
    result.get should be(ParsedFunctionCall(functionName = "mdr",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "-4.566476837630887E-37"),
        Parameter(tpe = ParameterType.NUMERIC, value = "1.590256498E9"),
        Parameter(tpe = ParameterType.NUMERIC, value = "7.5888587E8"),
        Parameter(tpe = ParameterType.NUMERIC, value = "1.1527215368903861E269"))))
  }

  // Literal arguments
  // -------------------------------------------------------------------------------------------------------------------

  it should "handle a literal composed of a single character" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(a)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.LITERAL, value = "a"))))
  }

  it should "handle a literal composed of two characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(ab)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a literal composed of two characters with leading whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn( ab)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a literal composed of two characters with trailing whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(ab )""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a function with two literal arguments" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(a,b)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.LITERAL, value = "a"),
        Parameter(tpe = ParameterType.LITERAL, value = "b"))))
  }

  it should "handle a function with two literal arguments separated with a space" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(a, b)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.LITERAL, value = "a"),
        Parameter(tpe = ParameterType.LITERAL, value = "b"))))
  }

  it should "handle a function with two literal arguments separated with a space and a capital" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(a, F)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.LITERAL, value = "a"),
        Parameter(tpe = ParameterType.LITERAL, value = "F"))))
  }

  // Quoted arguments
  // -------------------------------------------------------------------------------------------------------------------

  it should "handle a single quoted parameter with a single character" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("a")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = "a"))))
  }

  it should "handle a single quoted parameter with two characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("ab")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"))))
  }

  it should "handle a single quoted parameter with three characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("abc")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = "abc"))))
  }

  it should "handle a single quoted parameter with leading whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn( "abc")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = "abc"))))
  }

  it should "handle a single quoted parameter with trailing whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("abc" )""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = "abc"))))
  }

  it should "handle a single quoted parameter with an escaped quotation mark" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("ab\"")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = """ab\""""))))
  }

  it should "handle a single quoted parameter with an escaped quotation mark with a character after" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("ab\"c")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = """ab\"c"""))))
  }

  it should "handle a single quoted parameter with Chinese characters" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("㲧梍Û郓呸풕胫륺с룍럜馾գ픦ࠓ晣)")""")
    result.isDefined should be(true)
    result.get should be(ParsedFunctionCall(functionName = "fn",
      params = Seq(Parameter(tpe = ParameterType.QUOTED_STRING, value = """㲧梍Û郓呸풕胫륺с룍럜馾գ픦ࠓ晣)"""))))
  }

  // Literal and numeric arguments
  // -------------------------------------------------------------------------------------------------------------------

  it should "handle a single numeric and a single literal argument" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(2,ab)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "2"),
        Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a two numeric and a single literal argument" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(2,4,ab)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "2"),
        Parameter(tpe = ParameterType.NUMERIC, value = "4"),
        Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a single numeric and a single literal argument with whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(2, ab)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "2"),
        Parameter(tpe = ParameterType.LITERAL, value = "ab"))))
  }

  it should "handle a single literal argument and single numeric with whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(ab, 3)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.LITERAL, value = "ab"),
        Parameter(tpe = ParameterType.NUMERIC, value = "3"))))
  }

  // Literal and string arguments
  // -------------------------------------------------------------------------------------------------------------------

  it should "handle a single numeric and a single string argument" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn(2, "ab")""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.NUMERIC, value = "2"),
        Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"))))
  }

  it should "handle a single string and a single numeric argument" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""fn("ab", 2)""")
    result.isDefined should be (true)
    result.get should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
  }

  // Error cases
  // -------------------------------------------------------------------------------------------------------------------

  it should "return None if the function name starts with a space" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(""" a()""")
    result.isDefined should be (false)
  }

  it should "return None if the function name starts with a digit" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""21()""")
    result.isDefined should be (false)
  }

  it should "return None if the function name starts with a ." in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(""".1()""")
    result.isDefined should be (false)
  }

  it should "return None if the function doesn't have an open bracket" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f""")
    result.isDefined should be (false)
  }

  it should "return None if the function doesn't have a close bracket" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f(""")
    result.isDefined should be (false)
  }

  it should "return None if the function doesn't have a close bracket after whitespace" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f( """)
    result.isDefined should be (false)
  }

  it should "return None if the function has a close bracket without an open" in {
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f)""")
    result.isDefined should be (false)
  }

//  it should "return None if two numeric arguments aren't separated with a comma" in {
//    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall("""f(1 2)""")
//    result.isDefined should be (false)
//  }

  // parseFunctionCallDetailed() tests
  // -------------------------------------------------------------------------------------------------------------------

  "parseFunctionCallDetailed()" should "return the index of the last character of the function call" in {
    val result: Option[(ParsedFunctionCall, Int)] = FunctionCallParser.parseFunctionCallDetailed("""fn("ab", 2)""")
    result.isDefined should be (true)
    result.get._1 should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
    result.get._2 should be (10)
  }

  it should "return the index of the last character of the function call when there is trailing whitespace" in {
    val result: Option[(ParsedFunctionCall, Int)] = FunctionCallParser.parseFunctionCallDetailed("""fn("ab", 2) """)
    result.isDefined should be (true)
    result.get._1 should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
    result.get._2 should be (10)
  }

  it should "ignore characters after the closing bracket" in {
    val result: Option[(ParsedFunctionCall, Int)] = FunctionCallParser.parseFunctionCallDetailed("""fn("ab", 2),""")
    result.isDefined should be (true)
    result.get._1 should be (ParsedFunctionCall(functionName = "fn",
      params = Seq(
        Parameter(tpe = ParameterType.QUOTED_STRING, value = "ab"),
        Parameter(tpe = ParameterType.NUMERIC, value = "2"))))
    result.get._2 should be (10)
  }

}
