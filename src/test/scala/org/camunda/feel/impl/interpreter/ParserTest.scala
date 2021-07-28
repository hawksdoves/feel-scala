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
package org.camunda.feel.impl.interpreter

import org.camunda.feel.FeelEngine.UnaryTests
import org.camunda.feel.impl.FeelIntegrationTest
import org.camunda.feel.syntaxtree._
import org.scalatest.{FlatSpec, Matchers}

import java.time.LocalDateTime

class ParserTest
  extends FlatSpec
    with Matchers
    with FeelIntegrationTest {

  "A literal" should "with path" in {
    eval("{a:1}.a") should be(ValNumber(1))
  }

  it should "with filter" in {
    eval("[1,2,3][1]") should be(ValNumber(1))
  }

  it should "with path and filter" in {
    eval("{a:[1,2,3]}.a[1]") should be(ValNumber(1))
  }

  it should "with filter and path" in {
    eval("[{a:1}][1].a") should be(ValNumber(1))
  }

  it should "with parentheses" in {
    eval("(1)") should be(ValNumber(1))
  }

  it should "with parentheses and filter" in {
    eval("({a:1}).a") should be(ValNumber(1))
  }

  it should "with parentheses and path" in {
    eval("([1])[1]") should be(ValNumber(1))
  }

  it should "with date and time" in {
    eval(""" date and time("2019-08-12T22:22:22") """) should be(
      ValLocalDateTime(LocalDateTime.parse("2019-08-12T22:22:22"))
    )
  }

  "A comparison" should "with literals" in {
    eval("1 = 1") should be(ValBoolean(true))
  }

  it should "with between" in {
    eval("2 between 1 and 3") should be(ValBoolean(true))
  }

  it should "with path" in {
    eval("{a:1}.a = 1") should be(ValBoolean(true))
  }

  it should "with filter" in {
    eval("[1][1] = 1") should be(ValBoolean(true))
  }

  "Or" should "with literal" in {
    eval("true or false") should be(ValBoolean(true))
  }

  it should "with multiple" in {
    eval("true or false or false") should be(ValBoolean(true))
  }

  it should "with comparison" in {
    eval("1 = 1 or 1 = 2") should be(ValBoolean(true))
  }

  it should "with and" in {
    eval("true and false or true and true") should be(ValBoolean(true))
  }

  "And" should "with literal" in {
    eval("true and true") should be(ValBoolean(true))
  }

  it should "with multiple" in {
    eval("true and true and true") should be(ValBoolean(true))
  }

  it should "with between" in {
    eval("1 between 1 and 3 and 2 between 1 and 3") should be(ValBoolean(true))
  }

  it should "with or" in {
    eval("true or false and true or false") should be(ValBoolean(true))
  }

  "Math" should "add" in {
    eval("1 + 2 + 3") should be(ValNumber(6))
  }

  it should "sub" in {
    eval("6 - 1 - 2") should be(ValNumber(3))
  }

  it should "add & sub" in {
    eval("1 - 2 + 5 - 1") should be(ValNumber(3))
  }

  it should "mul" in {
    eval("2 * 3 * 4") should be(ValNumber(24))
  }

  it should "div" in {
    eval("12 / 2 / 3") should be(ValNumber(2))
  }

  it should "mul & div" in {
    eval("2 * 6 / 3") should be(ValNumber(4))
  }

  it should "add & mul" in {
    eval("2 * 3 + 3 * 4") should be(ValNumber(18))
  }

  it should "exponent" in {
    eval("2 ** 3") should be(ValNumber(8))
  }

  it should "negation" in {
    eval("-3 * 2") should be(ValNumber(-6))
  }

  it should "add & exponent" in {
    eval("1 + 2 ** 3") should be(ValNumber(9))
  }

  it should "with comparison" in {
    eval("2 * 3 = 6") should be(ValBoolean(true))
  }

  "In" should "with literal" in {
    eval("5 in > 3") should be(ValBoolean(true))
  }

  it should "with tests" in {
    eval("5 in (< 3, >= 5)") should be(ValBoolean(true))
  }

  it should "with math" in {
    eval("2 * 3 in > 3") should be(ValBoolean(true))
  }

  it should "with equal test" in {
    eval("5 in 5") should be(ValBoolean(true))
  }

  it should "with interval test [..]" in {
    eval("5 in [3..8]") should be(ValBoolean(true))
  }

  it should "with interval test (..)" in {
    eval("5 in (3..8)") should be(ValBoolean(true))
  }

  "Instance of" should "with literal" in {
    eval("5 instance of number") should be(ValBoolean(true))
  }

  it should "with math" in {
    eval("2 * 3 instance of number") should be(ValBoolean(true))
  }

  "If" should "with literal" in {
    eval("if true then 1 else 2") should be(ValNumber(1))
  }

  it should "with comparison" in {
    eval("if 1 = 1 then 1 else 2") should be(ValNumber(1))
  }

  it should "with path" in {
    eval("if {a: true}.a then 1 else 2") should be(ValNumber(1))
  }

  it should "with filter" in {
    eval("if [true][1] then 1 else 2") should be(ValNumber(1))
  }

  it should "with and" in {
    eval("if true and true then 1 else 2") should be(ValNumber(1))
  }

  it should "with or" in {
    eval("if false or true then 1 else 2") should be(ValNumber(1))
  }

  it should "with in" in {
    eval("if 1 in < 5 then 1 else 2") should be(ValNumber(1))
  }

  it should "with instance of" in {
    eval("if 1 instance of number then 1 else 2") should be(ValNumber(1))
  }

  "Some" should "with literal" in {
    eval("some x in [1,2,3] satisfies x > 2") should be(ValBoolean(true))
  }

  it should "with multiple literal" in {
    eval("some x in [1,2,3], y in [4,5] satisfies x + y > 6") should be(ValBoolean(true))
  }

  it should "with range" in {
    eval("some x in 1..5 satisfies x > 3") should be(ValBoolean(true))
  }

  "Every" should "with literal" in {
    eval("every x in [1,2,3] satisfies x < 5") should be(ValBoolean(true))
  }

  it should "with multiple literal" in {
    eval("every x in [1,2,3], y in [4,5] satisfies x + y < 10") should be(ValBoolean(true))
  }

  it should "with range" in {
    eval("every x in 1..5 satisfies x < 10") should be(ValBoolean(true))
  }

  "For" should "with literal" in {
    eval("for x in [1,2,3] return x") should be(ValList(List(
      ValNumber(1), ValNumber(2), ValNumber(3)
    )))
  }

  it should "with multiple literal" in {
    eval("for x in [1,2,3], y in [4,5] return x + y") should be(ValList(List(
      ValNumber(5), ValNumber(6), ValNumber(6), ValNumber(7), ValNumber(7), ValNumber(8)
    )))
  }

  it should "with range" in {
    eval("for x in 1..3 return x") should be(ValList(List(
      ValNumber(1), ValNumber(2), ValNumber(3)
    )))
  }

  "A function" should "with no parameter" in {
    eval("function () 1") shouldBe a[ValFunction]
  }

  it should "with one parameter" in {
    eval("function (x) x + 1") shouldBe a[ValFunction]
  }

  it should "with parameters" in {
    eval("function (x,y) x + y") shouldBe a[ValFunction]
  }

  it should "be invoked no arguments" in {
    eval("{f: function () 1, r: f() }.r") should be (ValNumber(1))
  }

  it should "be invoked positional arguments" in {
    eval("{f: function (x,y) x + y, r: f(1,2) }.r") should be (ValNumber(3))
  }

  it should "be invoked named arguments" in {
    eval("{f: function (x,y) x ** y, r: f(x:2,y:3) }.r") should be (ValNumber(8))
  }

  "A Unary-Tests" should "be - (dash)" in {
    evalUnaryTests(1, "-") should be (ValBoolean(true))
  }

  it should "compare input value (equal)" in {
    evalUnaryTests(1, "1") should be (ValBoolean(true))
  }

  it should "compare input value (<)" in {
    evalUnaryTests(1, "< 5") should be (ValBoolean(true))
  }

  it should "compare input value (>)" in {
    evalUnaryTests(1, "> 0") should be (ValBoolean(true))
  }

  it should "compare input value (>=)" in {
    evalUnaryTests(1, ">= 1") should be (ValBoolean(true))
  }

  it should "compare input value with list" in {
    evalUnaryTests(1, "[1,2,3]") should be (ValBoolean(true))
  }

  it should "evaluate boolean expression with ?" in {
    evalUnaryTests(1, "odd(?)") should be (ValBoolean(true))
  }

  it should "compare input value with multiple tests" in {
    evalUnaryTests(1, "1,2,3") should be (ValBoolean(true))
  }

  it should "compare input value with negation" in {
    evalUnaryTests(1, "not(2,3)") should be (ValBoolean(true))
  }

  it should "compare input value with interval (..)" in {
    evalUnaryTests(5, "(3..8)") should be (ValBoolean(true))
  }

  it should "compare input value with interval ]..[" in {
    evalUnaryTests(5, "]3..8[") should be (ValBoolean(true))
  }

  it should "compare input value with interval [..]" in {
    evalUnaryTests(5, "[3..8]") should be (ValBoolean(true))
  }

}
