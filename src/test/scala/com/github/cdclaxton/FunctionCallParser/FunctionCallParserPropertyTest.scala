package com.github.cdclaxton.FunctionCallParser

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.BooleanOperators

object FunctionCallParserPropertyTest extends Properties("Function call parser") {

  /**
    * Build a valid function name (must start with a character).
    */
  val functionName: Gen[String] = for {
    start <- Gen.alphaChar
    remainder <- Gen.alphaNumStr
  } yield s"$start$remainder"

  val validFunctionName = functionName suchThat(_.length >= 0)

  property("No arguments") = Prop.forAll(validFunctionName) { functionName =>
    val functionCall = s"$functionName()"
    val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

    result.isDefined :| "defined" &&
      (result.get.functionName == functionName) :| "function name" &&
      result.get.params.isEmpty :| "number of arguments"
  }

  property("Single integer numeric argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll{ value: Int =>

      val functionCall = s"$functionName($value)"
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.get.functionName == functionName) :| "function name" &&
        (result.get.params.length == 1) :| "number of arguments" &&
        (result.get.params.head.tpe == ParameterType.NUMERIC) :| "parameter type" &&
        (result.get.params.head.value == s"$value") :| "parameter value"
    }
  }

  property("Two integer numeric arguments") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll { value1: Int =>
      Prop.forAll{ value2: Int =>
        val functionCall = s"$functionName($value1, $value2)"
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.get.functionName == functionName) :| "function name" &&
          (result.get.params.length == 2) :| "number of arguments" &&
          (result.get.params.head.tpe == ParameterType.NUMERIC) :| "parameter type" &&
          (result.get.params.head.value == s"$value1") :| "parameter value" &&
          (result.get.params(1).tpe == ParameterType.NUMERIC) :| "parameter type" &&
          (result.get.params(1).value == s"$value2") :| "parameter value"
      }
    }
  }

  property("Single literal argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll(validFunctionName) { literal =>
      val functionCall = s"$functionName($literal)"
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.get.functionName == functionName) :| "function name" &&
        (result.get.params.length == 1) :| "number of arguments" &&
        (result.get.params.head.tpe == ParameterType.LITERAL) :| "parameter type" &&
        (result.get.params.head.value == literal) :| "parameter value"
    }
  }

  property("Two literal arguments") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll(validFunctionName) { literal1 =>
      Prop.forAll(validFunctionName) { literal2 =>

        val functionCall = s"$functionName($literal1, $literal2)"
        val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

        result.isDefined :| "defined" &&
          (result.get.functionName == functionName) :| "function name" &&
          (result.get.params.length == 2) :| "number of arguments" &&
          (result.get.params.head.tpe == ParameterType.LITERAL) :| "parameter type" &&
          (result.get.params.head.value == literal1) :| "parameter value" &&
          (result.get.params(1).tpe == ParameterType.LITERAL) :| "parameter type" &&
          (result.get.params(1).value == literal2) :| "parameter value"
      }
    }
  }

  property("Single quoted argument") = Prop.forAll(validFunctionName) { functionName =>
    Prop.forAll { str: String =>

      val functionCall = s"""$functionName("$str")"""
      val result: Option[ParsedFunctionCall] = FunctionCallParser.parseFunctionCall(functionCall)

      result.isDefined :| "defined" &&
        (result.get.functionName == functionName) :| "function name" &&
        (result.get.params.length == 1) :| "number of arguments" &&
        (result.get.params.head.tpe == ParameterType.QUOTED_STRING) :| "parameter type" &&
        (result.get.params.head.value == str) :| "parameter value"
    }
  }

}
