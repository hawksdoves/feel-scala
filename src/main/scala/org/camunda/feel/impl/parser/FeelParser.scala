/*
 * Copyright Camunda Services GmbH and/or licensed to Camunda Services GmbH
 * under one or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information regarding copyright
 * ownership. Camunda licenses this file to you under the Apache License,
 * Version 2.0; you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.feel.impl.parser

import fastparse.JavaWhitespace._
import fastparse._
import org.camunda.feel._
import org.camunda.feel.syntaxtree._

import scala.util.Try

/**
  * The parser is written following the FEEL grammar definition in the DMN specification.
  *
  * In order to understand how the parser works, it is recommended to read the documentation first:
  * [[https://www.lihaoyi.com/fastparse]]. Additional resources:
  * [[https://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html]],
  * [[https://www.lihaoyi.com/post/BuildyourownProgrammingLanguagewithScala.html]]
  */
object FeelParser {

  def parseExpression(expression: String): Parsed[Exp] =
    parse(expression, fullExpression(_))

  def parseUnaryTests(expression: String): Parsed[Exp] =
    parse(expression, fullUnaryExpression(_))

  //  -------------------

  private def fullExpression[_: P] = P(Start ~ exp_ ~ End)

  private def fullUnaryExpression[_: P] = P(Start ~ unaryTests_ ~ End)

  //  ---------------

  private def reservedWord[_: P] =
    P(
      StringIn("null",
        "true",
        "false",
        "function",
        "if",
        "then",
        "else",
        "for",
        "between",
        "instance",
        "of",
        "some",
        "every"))

  // list of built-in function names with whitespaces
  // -- other names match the 'function name' pattern
  private def builtinFunctionName_[_: P]: P[String] = P(
    StringIn(
      "date and time",
      "years and months duration",
      "string length",
      "upper case",
      "lower case",
      "substring before",
      "substring after",
      "starts with",
      "ends with",
      "list contains",
      "insert before",
      "index of",
      "distinct values",
      "get entries",
      "get value",
      "is defined",
      "day of year",
      "day of week",
      "month of year",
      "week of year",
      "put all"
    ).!
  )

  // list of built-in function parameter names with whitespaces
  // -- other names match the 'parameter name' pattern
  private def builtinFunctionParameterName_[_: P]: P[String] = P(
    StringIn("start position", "grouping separator", "decimal separator").!
  )

  //  ------------------

  // an identifier which is not a reserved word. however, it can contain a reserved word.
  private def identifier_[_: P]: P[String] = P(
    reservedWord.? ~~ javaLikeIdentifier_
  )

  private def javaLikeIdentifier_[_: P]: P[String] = P(
    CharPred(Character.isJavaIdentifierStart) ~~ CharsWhile(Character.isJavaIdentifierPart, 0)
  ).!

  private def name_[_: P]: P[String] = P(
    identifier_ | escapedIdentifier_
  )

  private def escapedIdentifier_[_: P]: P[String] = P(
    "`" ~~ (!"`" ~~ AnyChar.!).repX(1) ~~ "`"
  ).map(_.mkString).log

  private def qualifiedName_[_: P]: P[List[String]] = P(
    name_.rep(1, sep = ".")
  ).map(_.toList)

  // characters or string escape sequences (\', \", \\, \n, \r, \t, \u269D, \U101EF)
  private def stringWithQuotes_[_: P]: P[String] = P(
    "\"" ~~ (("\\" | !"\"") ~~ AnyChar).repX.! ~~ "\""
  )

  private def optional_[_: P](op: Exp => P[Exp]): Exp => P[Exp] = { value =>
    op(value).?.map(
      _.fold(value)(opResult => opResult)
    )
  }

  //  -----------------------------

  private def exp_[_: P]: P[Exp] = expLvl1_

  private def expLvl1_[_: P]: P[Exp] = controlOp_ | disjunction_

  private def expLvl2_[_: P]: P[Exp] = conjunction_

  private def expLvl3_[_: P]: P[Exp] = expLvl4_.flatMap(optional_(comparisonOp_)) | simplePositiveUnaryTest_

  private def expLvl4_[_: P]: P[Exp] = expLvl5_.flatMap(optional_(valueOp_))

  private def expLvl5_[_: P]: P[Exp] = mathOp_

  private def expLvl6_[_: P]: P[Exp] = value_

  //  -----------------------------

  private def mathOp_[_: P]: P[Exp] = mathOpLvl1_

  private def mathOpLvl1_[_: P]: P[Exp] = addSub_

  private def mathOpLvl2_[_: P]: P[Exp] = mulDiv_

  private def mathOpLvl3_[_: P]: P[Exp] = exponential_

  private def mathOpLvl4_[_: P]: P[Exp] = negation_

  //  -----------------------------


  private def disjunction_[_: P]: P[Exp] = P(
    expLvl2_ ~ ("or" ~ expLvl2_).rep
  ).map { case (base, ops) => ops.foldLeft(base)(Disjunction)
  }.log

  private def conjunction_[_: P]: P[Exp] = P(
    expLvl3_ ~ ("and" ~ expLvl3_).rep
  ).map { case (base, ops) => ops.foldLeft(base)(Conjunction)
  }.log

  private def comparisonOp_[_: P](value: Exp): P[Exp] = P(
    binaryComparison_(value) | between_(value)
  ).log

  private def binaryComparison_[_: P](x: Exp): P[Exp] = P(
    StringIn("<=", ">=", "<", ">", "!=", "=").! ~ expLvl4_
  ).map {
    case ("=", y) => Equal(x, y)
    case ("!=", y) => Not(Equal(x, y))
    case ("<", y) => LessThan(x, y)
    case ("<=", y) => LessOrEqual(x, y)
    case (">", y) => GreaterThan(x, y)
    case (">=", y) => GreaterOrEqual(x, y)
  }.log

  private def between_[_: P](x: Exp): P[Exp] = P(
    "between" ~ expLvl4_ ~ "and" ~ expLvl4_
  ).map {
    case (a, b) => Conjunction(GreaterOrEqual(x, a), LessOrEqual(x, b))
  }.log

  private def addSub_[_: P]: P[Exp] = P(
    mathOpLvl2_ ~ (CharIn("+\\-").! ~ mathOpLvl2_).rep
  ).map {
    case (value, ops) => ops.foldLeft(value) {
      case (x, ("+", y)) => Addition(x, y)
      case (x, ("-", y)) => Subtraction(x, y)
    }
  }.log

  private def mulDiv_[_: P]: P[Exp] = P(
    mathOpLvl3_ ~ (CharIn("*/").! ~ mathOpLvl3_).rep
  ).map {
    case (value, ops) => ops.foldLeft(value) {
      case (x, ("*", y)) => Multiplication(x, y)
      case (x, ("/", y)) => Division(x, y)
    }
  }.log

  private def exponential_[_: P]: P[Exp] = P(
    mathOpLvl4_ ~ ("**" ~ mathOpLvl4_).rep
  ).map {
    case (value, ops) => ops.foldLeft(value)(Exponentiation)
  }.log

  private def negation_[_: P]: P[Exp] = P(
    "-".!.? ~ expLvl6_
  ).map {
    case (Some("-"), value) => ArithmeticNegation(value)
    case (None, value) => value
  }.log

  private def value_[_: P]: P[Exp] = P(
    terminalValue_.flatMap(optional_(chainedValueOp_))
  ).log

  private def terminalValue_[_: P]: P[Exp] = P(
    temporal_ | functionInvocation_ | variableRef_ | literal_ | inputValue_ | functionDefinition_ | "(" ~ exp_ ~ ")"
  ).log

  private def literal_[_: P]: P[Exp] = P(
    null_ | boolean_ | string_ | number_ | temporal_ | list_ | context_
  ).log

  private def null_[_: P]: P[Exp] = P(
    "null"
  ).map(_ => ConstNull).log

  private def boolean_[_: P]: P[Exp] = P(
    ("true" | "false").!
  ).map {
    case "true" => ConstBool(true)
    case "false" => ConstBool(false)
  }.log

  private def string_[_: P]: P[Exp] = P(
    stringWithQuotes_
  ).map(ConstString).log

  // TODO (saig0): check if it can be simpler
  private def number_[_: P]: P[Exp] = P(
    "-".? ~~ ((integral_ ~~ fractional_.?) | fractional_)
  ).!.map(number => ConstNumber(BigDecimal(number)))
    .log

  private def digits_[_: P]: P[String] = P(
    CharsWhileIn("0-9")
  ).!

  private def integral_[_: P]: P[String] = P(
    CharIn("0-9") ~~ digits_.?
  ).!

  private def fractional_[_: P]: P[String] = P(
    "." ~~ digits_
  ).!

  // TODO (saig0): simplify date format parsing
  private def temporal_[_: P]: P[Exp] = P(
    ("duration" | "date and time" | "date" | "time").! ~ "(" ~ stringWithQuotes_ ~ ")"
  ).map {
    case ("duration", value) => parseDuration(value)
    case ("date and time", value) => parseDateTime(value)
    case ("date", value) => parseDate(value)
    case ("time", value) => parseTime(value)
  }.log

  private def list_[_: P]: P[Exp] = P(
    "[" ~ exp_.rep(0, sep = ",") ~ "]"
  ).map(items => ConstList(items.toList)
  ).log

  private def context_[_: P]: P[Exp] = P(
    "{" ~ contextEntry_.rep(0, sep = ",") ~ "}"
  ).map(entries => ConstContext(entries.toList)
  ).log

  private def contextEntry_[_: P]: P[(String, Exp)] = P(
    (name_ | stringWithQuotes_) ~ ":" ~ exp_
  )

  // an (escaped) identifier but not the name of a function invocation or path expression
  private def variableRef_[_: P]: P[Exp] = P(
    (identifier_.! | escapedIdentifier_) ~ !"("
  ).map(n => Ref(List(n))).log

  private def inputValue_[_: P]: P[Exp] = P(
    "?"
  ).map(_ => ConstInputValue).log

  private def functionDefinition_[_: P]: P[Exp] = P(
    "function" ~ "(" ~ parameter_.rep(0, sep = ",") ~ ")" ~ (externalFunction_ | exp_)
  ).map {
    case (parameters, body) => FunctionDefinition(parameters.toList, body)
  }.log

  private def parameter_[_: P]: P[String] = P(
    parameterName_
  ).log

  // TODO (saig0): avoid builtin parameter names for function definition
  private def parameterName_[_: P]: P[String] = P(
    builtinFunctionParameterName_ | name_
  ).log

  private def externalFunction_[_: P]: P[Exp] = P(
    "external" ~ externalJavaFunction_
  ).log

  private def externalJavaFunction_[_: P]: P[Exp] = P(
    "{" ~
      "java" ~ ":" ~ "{" ~
      "class" ~ ":" ~ stringWithQuotes_ ~ "," ~
      "method signature" ~ ":" ~ javaMethodSignature_ ~
      "}" ~ "}"
  ).map { case (className, (methodName, parameters)) =>
    JavaFunctionInvocation(className, methodName, parameters.toList)
  }.log

  private def javaMethodSignature_[_: P]: P[(String, Seq[String])] = P(
    "\"" ~ name_ ~ "(" ~ javaMethodParameter_.rep(0, sep = ",") ~ ")" ~ "\""
  ).log

  private def javaMethodParameter_[_: P]: P[String] = P(
    qualifiedName_
  ).map(_.mkString)

  // TODO (saig0): check if we need a qualified function invocation
  // TODO (saig0): try to replace builtin function names with `name_ ~ (name_).rep ~ "("`
  private def functionInvocation_[_: P]: P[Exp] = P(
    (builtinFunctionName_.map(List(_)) | qualifiedName_) ~ "(" ~ functionParameters_.? ~ ")"
  ).map {
    case (name :: Nil, None) => FunctionInvocation(name, PositionalFunctionParameters(List.empty))
    case (name :: Nil, Some(parameters)) => FunctionInvocation(name, parameters)
    case (names, None) => QualifiedFunctionInvocation(Ref(names.dropRight(1)), names.last, PositionalFunctionParameters(List.empty))
    case (names, Some(parameters)) => QualifiedFunctionInvocation(Ref(names.dropRight(1)), names.last, parameters)

  }.log

  private def functionParameters_[_: P]: P[FunctionParameters] = P(
    namedParameters_ | positionalParameters_
  ).log

  // TODO (saig0): try to replace parameter name with `name ~ name.rep ~ ":"`
  private def namedParameters_[_: P]: P[NamedFunctionParameters] = P(
    (parameterName_ ~ ":" ~ exp_).rep(1, sep = ",")
  ).map(params => NamedFunctionParameters(params.toMap)
  ).log

  private def positionalParameters_[_: P]: P[PositionalFunctionParameters] = P(
    exp_.rep(1, sep = ",")
  ).map(params => PositionalFunctionParameters(params.toList)
  ).log

  private def chainedValueOp_[_: P](base: Exp): P[Exp] = P(
    (path_(base) | filter_(base)).flatMap(optional_(chainedValueOp_))
  ).log

  private def path_[_: P](base: Exp): P[Exp] = P(
    ("." ~ (valueProperty_ | name_)).rep(1)
  ).map(ops => ops.foldLeft(base)(PathExpression)).log

  // TODO (saig0): try to use `name ~ name.rep` instead
  private def valueProperty_[_: P]: P[String] = P(
    "time offset"
  ).!

  private def filter_[_: P](base: Exp): P[Exp] = P(
    ("[" ~ exp_ ~ "]").rep(1)
  ).map(ops => ops.foldLeft(base)(Filter)
  ).log

  // TODO (saig0): find a better name
  private def valueOp_[_: P](value: Exp): P[Exp] = instanceOf_(value) | in_(value)

  private def instanceOf_[_: P](value: Exp): P[Exp] = P(
    "instance" ~ "of" ~ typeName_
  ).map(InstanceOf(value, _)).log

  private def typeName_[_: P]: P[String] = P(
    qualifiedName_
  ).map(_.mkString(".")).log

  private def in_[_: P](value: Exp): P[Exp] = P(
    "in" ~ (("(" ~ positiveUnaryTests_ ~ ")") | positiveUnaryTest_)
  ).map(In(value, _)).log

  private def controlOp_[_: P]: P[Exp] = if_ | quantified_ | for_

  private def if_[_: P]: P[Exp] = P(
    "if" ~ exp_ ~ "then" ~ exp_ ~ "else" ~ exp_
  ).map { case (condition, thenExp, elseExp) => If(condition, thenExp, elseExp)
  }.log

  private def for_[_: P]: P[Exp] = P(
    "for" ~ listIterator_.rep(1, sep = ",") ~ "return" ~ exp_
  ).map { case (iterators, exp) => For(iterators.toList, exp)
  }.log

  private def listIterator_[_: P]: P[(String, Exp)] = P(
    name_ ~ "in" ~/ (range_ | value_)
  ).log

  private def range_[_: P]: P[Exp] = P(
    expLvl5_ ~ ".." ~ expLvl5_
  ).map { case (start, end) => Range(start, end)
  }.log

  private def quantified_[_: P]: P[Exp] = P(
    ("some" | "every").! ~ listIterator_.rep(1, sep = ",") ~ "satisfies" ~ exp_
  ).map {
    case ("some", iterators, condition) => SomeItem(iterators.toList, condition)
    case ("every", iterators, condition) => EveryItem(iterators.toList, condition)
  }.log

  // -------------------------------------

  private def unaryTests_[_: P]: P[Exp] = P(
    negationUnaryTests_ | positiveUnaryTests_ | anyInput_
  ).log

  private def anyInput_[_: P]: P[Exp] = P(
    "-"
  ).map(_ => ConstBool(true)).log

  private def negationUnaryTests_[_: P]: P[Exp] = P(
    "not" ~ "(" ~ positiveUnaryTests_ ~ ")"
  ).map(Not).log

  private def positiveUnaryTests_[_: P]: P[Exp] = P(
    positiveUnaryTest_.rep(1, sep = ",")
  ).map {
    case test :: Nil => test
    case tests => AtLeastOne(tests.toList)
  }.log

  // unary-test is ambiguous for boolean literals - give precedence to comparison with input
  private def positiveUnaryTest_[_: P]: P[Exp] = P(
    boolean_.map(InputEqualTo) |
      exp_.map(UnaryTestExpression)
  ).log

  private def simplePositiveUnaryTest_[_: P]: P[Exp] = P(
    unaryComparison_ | interval_
  ).log

  private def unaryComparison_[_: P]: P[Exp] = P(
    StringIn("<=", ">=", "<", ">").! ~ endpoint_
  ).map {
    case ("<", x) => InputLessThan(x)
    case ("<=", x) => InputLessOrEqual(x)
    case (">", x) => InputGreaterThan(x)
    case (">=", x) => InputGreaterOrEqual(x)
  }.log

  private def endpoint_[_: P]: P[Exp] = expLvl5_

  private def interval_[_: P]: P[Exp] = P(
    intervalStart_ ~ ".." ~ intervalEnd_
  ).map {
    case (start, end) => Interval(start, end)
  }.log

  private def intervalStart_[_: P]: P[IntervalBoundary] = P(
    CharIn("(", "]", "[").! ~ endpoint_
  ).map {
    case ("(", x) => OpenIntervalBoundary(x)
    case ("]", x) => OpenIntervalBoundary(x)
    case ("[", x) => ClosedIntervalBoundary(x)
  }.log

  private def intervalEnd_[_: P]: P[IntervalBoundary] = P(
    endpoint_ ~ CharIn(")", "[", "]").!
  ).map {
    case (y, ")") => OpenIntervalBoundary(y)
    case (y, "[") => OpenIntervalBoundary(y)
    case (y, "]") => ClosedIntervalBoundary(y)
  }.log

  //  ------------

  private def parseDate(d: String): Exp = {
    Try(ConstDate(d)).filter(_ => isValidDate(d)).getOrElse(ConstNull)
  }

  private def parseTime(t: String): Exp = {
    if (isOffsetTime(t)) {
      Try(ConstTime(t)).getOrElse(ConstNull)
    } else {
      Try(ConstLocalTime(t)).getOrElse(ConstNull)
    }
  }

  private def parseDateTime(dt: String): Exp = {
    if (isValidDate(dt)) {
      Try(ConstLocalDateTime((dt: Date).atTime(0, 0))).getOrElse(ConstNull)
    } else if (isOffsetDateTime(dt)) {
      Try(ConstDateTime(dt)).getOrElse(ConstNull)
    } else if (isLocalDateTime(dt)) {
      Try(ConstLocalDateTime(dt)).getOrElse(ConstNull)
    } else {
      ConstNull
    }
  }

  private def parseDuration(d: String): Exp = {
    if (isYearMonthDuration(d)) {
      Try(ConstYearMonthDuration(d)).getOrElse(ConstNull)
    } else if (isDayTimeDuration(d)) {
      Try(ConstDayTimeDuration(d)).getOrElse(ConstNull)
    } else {
      ConstNull
    }
  }
}
