// Copyright 2018 EPFL DATA Lab (data.epfl.ch)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package squid

import org.scalatest.funsuite.AnyFunSuite
import squid.ir.AST
import squid.ir.SimpleANF
import squid.ir.SimpleAST
import squid.lang.InspectableBase
import utils.meta.RuntimeUniverseHelpers.sru
import utils.GenHelper

class MyFunSuite[DSL <: AST](override val DSL: DSL = TestDSL) extends MyFunSuiteBase[DSL](DSL) { funs =>
  import DSL._
  
  def eqt(a: CodeType[_], b: CodeType[_]) = eqtBy(a,b)(_ =:= _)
  def eqt(a: TypeRep, b: TypeRep) = eqtBy(a,b)(_ =:= _)
  
  def extractedBounds(t: CodeType[_]): (TypeRep, TypeRep) = {
    t.rep match {
      case et: ExtractedType => (et.lb, et.ub)
    }
  }
  
}

/** The reason we currently have {{{DSL <: InspectableBase}}} is because otherwise the 'eqt' functions have the same erasure... */
//class MyFunSuiteBase[DSL <: InspectableBase](val DSL: DSL = TestDSL2) extends MyFunSuiteTrait[DSL.type]
//class MyFunSuiteBase[DSL <: InspectableBase](val DSL: DSL = TestDSL2) extends MyFunSuiteTrait[DSL]
class MyFunSuiteBase[DSL <: InspectableBase](val DSL: DSL = TestDSL) extends MyFunSuiteTrait
//abstract class MyFunSuiteTrait[DSL <: InspectableBase] extends AnyFunSuite { funs =>
//trait MyFunSuiteTrait[DSL <: InspectableBase] extends AnyFunSuite { funs =>
  //val DSL: DSL
trait MyFunSuiteTrait extends AnyFunSuite { funs =>
  val DSL: InspectableBase
  import DSL._
  
  def hopefully(condition: Boolean) = assert(condition)
  def hopefullyNot(condition: Boolean) = assert(!condition)
  
  def sameScalaType[A: sru.TypeTag, B: sru.TypeTag] =
    if (!(sru.typeOf[A] =:= sru.typeOf[B])) fail(s"${sru.typeOf[A]} =/= ${sru.typeOf[B]}")
  
  class ofExactTypeHelper[B] {
    def apply[A: sru.TypeTag](a: A)(implicit B: sru.TypeTag[B]): Unit = sameScalaType[A,B]
  }
  def ofExactType[B] = new ofExactTypeHelper[B]
  
  implicit class TypeHelper[A: sru.TypeTag](self: A) {
    def ofType[B: sru.TypeTag]() = { sameScalaType[A,B]; self }
  }
  
  private def showBest(x:Any) = x match {
    //case r: DSL.Rep => r |> DSL.showRep // this no longer works, because Rep is an erased type
    case (_: SimpleAST # Rep) | (_: SimpleANF # Rep) => // so for now we just special-case actual known Rep classes
      x.asInstanceOf[DSL.Rep] |> DSL.showRep
    case _ => x.toString
  }
  
  def same[T](a: T, b: T) = assert(a == b)
  def eqtBy[T](a: T, b: T, truth: Boolean = true)(r: (T,T) => Boolean) =
    //assert(r(a, b), s"=> $a and $b are not equivalent")
    if (r(a, b) != truth) {
      System.err.println(s"FAILURE: ${a|>showBest} and ${b|>showBest}")
      fail(s"$a and $b are ${if (truth) "not " else ""}equivalent")
    }
  
  
  def subt(a: CodeType[_], b: CodeType[_]) = eqtBy(a,b)(_ <:< _)
  def subt(a: TypeRep, b: TypeRep) = eqtBy(a,b)(_ <:< _)
  
  def eqt(a: Rep, b: Rep) = eqtBy(a,b)(_ =~= _)
  def eqt(a: AnyCode[_], b: AnyCode[_], truth: Boolean = true) = eqtBy(a,b,truth)(_ =~= _)
  
  def matches(a: AnyCode[_])(pfs: PartialFunction[AnyCode[_],Unit]*) = {
    for (pf <- pfs) pf(a)
    MatchesAnd(a)
  }
  case class MatchesAnd[T](a: AnyCode[T]) {
    def and (pf: PartialFunction[AnyCode[_],Unit]) = {
      pf(a)
      this
    }
    def and_debug (pf: PartialFunction[AnyCode[_],Unit]) = DSL.debugFor(and(pf))
  }
  
  implicit class Matches(self: AnyCode[_]) {
    def matches(pfs: PartialFunction[AnyCode[_],Unit]*) = funs.matches(self)(pfs: _*)
    def eqt (that: AnyCode[_]) = funs.eqt(self.rep, that.rep)
    def dbg_eqt (that: AnyCode[_]) = {
      DSL debugFor (self.rep extractRep that.rep)
      DSL debugFor (that.rep extractRep self.rep)
      funs.eqt(self.rep, that.rep)
    }
    def neqt (that: Code[_,_]) = funs.eqt(self, that, false)
  }
  
  def implicitTypeOf[A: CodeType](x: Code[A,_]) = codeTypeOf[A].rep
  
  
  type Q[+T,-C] = Code[T,C]
  type TypeEv[T] = CodeType[T]
  type QuotedType[T] = CodeType[T]
  
  def typeEv[T: TypeEv] = codeTypeOf[T]
  
}

