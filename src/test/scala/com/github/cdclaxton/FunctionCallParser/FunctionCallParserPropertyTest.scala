package com.github.cdclaxton.FunctionCallParser

import org.apache.commons.lang3.StringEscapeUtils
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.BooleanOperators

import scala.util.Random

object FunctionCallParserPropertyTest extends Properties("Function call parser") {

  /**
    * Build a valid function name (must start with a character).
    */
  val functionName: Gen[String] = for {
    start <- Gen.alphaChar
    remainder <- Gen.alphaStr
  } yield s"$start$remainder"

  val validFunctionName = functionName suchThat((x: String) => x.length > 0 && Character.isAlphabetic(x(0)))

  property("No arguments") = Prop.forAll(validFunctionName) { functionName =>
    val functionCall = s"$functionName()"
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

    result.isDefined :| "defined" &&
      (result.isDefined && result.get.functionName == functionName) :| "function name" &&
      (result.isDefined && result.get.params.isEmpty) :| "number of arguments"
  }

  property("Single integer numeric argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll{ value: Int =>

      val functionCall = s"$functionName($value)"
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.isDefined && result.get.functionName == functionName) :| "function name" &&
        (result.isDefined && result.get.params.length == 1) :| "number of arguments" &&
        (result.isDefined && result.get.params.head.tpe == ParameterType.NUMERIC) :| "parameter type" &&
        (result.isDefined && result.get.params.head.value == s"$value") :| "parameter value"
    }
  }

  property("Two integer numeric arguments") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll { value1: Int =>
      Prop.forAll{ value2: Int =>
        val functionCall = s"$functionName($value1, $value2)"
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == 2) :| "number of arguments" &&
          (result.isDefined && result.get.params.head.tpe == ParameterType.NUMERIC) :| "parameter type" &&
          (result.isDefined && result.get.params.head.value == s"$value1") :| "parameter value" &&
          (result.isDefined && result.get.params(1).tpe == ParameterType.NUMERIC) :| "parameter type" &&
          (result.isDefined && result.get.params(1).value == s"$value2") :| "parameter value"
      }
    }
  }

  property("Single literal argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll(validFunctionName) { literal =>
      val functionCall = s"$functionName($literal)"
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.isDefined && result.get.functionName == functionName) :| "function name" &&
        (result.isDefined && result.get.params.length == 1) :| "number of arguments" &&
        (result.isDefined && result.get.params.head.tpe == ParameterType.LITERAL) :| "parameter type" &&
        (result.isDefined && result.get.params.head.value == literal) :| "parameter value"
    }
  }

  property("Two literal arguments") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll(validFunctionName) { literal1 =>
      Prop.forAll(validFunctionName) { literal2 =>

        val functionCall = s"$functionName($literal1, $literal2)"
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == 2) :| "number of arguments" &&
          (result.isDefined && result.get.params.head.tpe == ParameterType.LITERAL) :| "parameter type" &&
          (result.isDefined && result.get.params.head.value == literal1) :| "parameter value" &&
          (result.isDefined && result.get.params(1).tpe == ParameterType.LITERAL) :| "parameter type" &&
          (result.isDefined && result.get.params(1).value == literal2) :| "parameter value"
      }
    }
  }

  property("Single quoted argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll { str: String =>

      val functionCall = s"""$functionName("$str")"""
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.isDefined && result.get.functionName == functionName) :| "function name" &&
        (result.isDefined && result.get.params.length == 1) :| "number of arguments" &&
        (result.isDefined && result.get.params.head.tpe == ParameterType.QUOTED_STRING) :| "parameter type" &&
        (result.isDefined && result.get.params.head.value == str) :| "parameter value"
    }
  }

  property("Two quoted arguments") = Prop.forAll(validFunctionName) { functionName =>
    (functionName.length > 0) ==> {
      Prop.forAll { str1: String =>
        Prop.forAll { str2: String =>

          val functionCall = s"""$functionName("$str1", "$str2")"""
          val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

          result.isDefined :| "defined" &&
            (result.isDefined && result.get.functionName == functionName) :| "function name" &&
            (result.isDefined && result.get.params.length == 2) :| "number of arguments" &&
            (result.isDefined && result.get.params.head.tpe == ParameterType.QUOTED_STRING) :| "parameter type 1" &&
            (result.isDefined && result.get.params.head.value == str1) :| "parameter value 1" &&
            (result.isDefined && result.get.params(1).tpe == ParameterType.QUOTED_STRING) :| "parameter type 2" &&
            (result.isDefined && result.get.params(1).value == str2) :| "parameter value 2"
        }
      }
    }
  }

  /**
    * Generate an arbitrary numeric parameter (integer or double type).
    */
  private val genNumericParameter: Gen[Parameter] = for {
    intValue <- Arbitrary.arbitrary[Int]
    doubleValue <- Arbitrary.arbitrary[Double]
    useIntValue <- Arbitrary.arbitrary[Boolean]
  } yield {
    val value: Double = if (useIntValue) intValue else doubleValue
    Parameter(ParameterType.NUMERIC, s"$value")
  }

  /**
    * Generate an arbitrary quoted string parameter.
    */
  private val genStringParameter: Gen[Parameter] = for {
    value <- Arbitrary.arbitrary[String]
  } yield Parameter(ParameterType.QUOTED_STRING, StringEscapeUtils.escapeJava(value))

  /**
    * Generate an arbitrary literal parameter.
    */
  private val genLiteralParameter = for {
    start <- Gen.alphaChar
    remainder <- Gen.alphaNumStr
  } yield Parameter(ParameterType.LITERAL, s"$start$remainder")

  /**
    * Generate a list of mixed parameter types.
    */
  private val genMixedParameters: Gen[List[Parameter]] = for {
    numeric <- Gen.listOfN(5, genNumericParameter)
    literal <- Gen.listOfN(5, genLiteralParameter)
    string <- Gen.listOfN(5, genStringParameter)
    numNumeric <- Gen.choose(0, 5)
    numLiteral <- Gen.choose(0, 5)
    numString <- Gen.choose(0, 5)
  } yield {
    Random.shuffle(numeric.take(numNumeric) ++ literal.take(numLiteral) ++ string.take(numString))
  }

  /**
    * Build the string represenation of a parameter as it would appear in a function call.
    *
    * @param param Parameter.
    * @return String representation.
    */
  private def buildParameterValue(param: Parameter): String = {
    if (param.tpe == ParameterType.QUOTED_STRING) s""""${param.value}""""
    else s"""${param.value}"""
  }

  /**
    * Build a function call given its name and a list of parameters.
    *
    * @param functionName Name of the function.
    * @param params       List of parameters.
    * @return String representation of a function call.
    */
  private def buildFunctionCall(functionName: String,
                                params: List[Parameter]): String = {
    functionName + params.map((p: Parameter) => buildParameterValue(p)).mkString("(", ", ", ")")
  }

  property("List of numeric arguments") = Prop.forAll(validFunctionName) { functionName =>
    (functionName.length > 0) ==> {
      Prop.forAll(Gen.nonEmptyListOf(genNumericParameter)) { params =>

        val functionCall = buildFunctionCall(functionName, params)
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == params.length) :| "number of parameters" &&
          (result.isDefined && result.get.params == params) :| "parameters correct"
      }
    }
  }

  property("List of string arguments") = Prop.forAll(validFunctionName) { functionName =>
    (functionName.length > 0) ==> {
      Prop.forAll(Gen.nonEmptyListOf(genStringParameter)) { params =>

        val functionCall = buildFunctionCall(functionName, params)
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == params.length) :| "number of parameters" &&
          (result.isDefined && result.get.params == params) :| "parameters correct"
      }
    }
  }

  property("List of literal arguments") = Prop.forAll(validFunctionName) { functionName =>
    (functionName.length > 0) ==> {
      Prop.forAll(Gen.nonEmptyListOf(genLiteralParameter)) { params =>

        val functionCall = buildFunctionCall(functionName, params)
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == params.length) :| "number of parameters" &&
          (result.isDefined && result.get.params == params) :| "parameters correct"
      }
    }
  }

  property("Mixed arguments") = Prop.forAll(validFunctionName) { functionName =>
    (functionName.length > 0) ==> {
      Prop.forAll(genMixedParameters) { params =>

        val functionCall = buildFunctionCall(functionName, params)
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.isDefined && result.get.functionName == functionName) :| "function name" &&
          (result.isDefined && result.get.params.length == params.length) :| "number of parameters" &&
          (result.isDefined && result.get.params == params) :| "parameters correct"
      }
    }
  }

}
