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

package squid.statics

import org.scalatest.funsuite.AnyFunSuite

object CompileTimeTests {
  val s = compileTime{ Symbol("ok") }
  val s0 = Symbol("ko")
}
import CompileTimeTests._

class CompileTimeTests extends AnyFunSuite {
  
  test("Basics") {
    
    val t = compileTime{ s.name }
    val u = compileTime{ t.length + s.toString.size }
    
    assert(u == "ok".length + "'ok".size)
    
    // This raises an assertion error at compile time, making compilation fail:
    assertDoesNotCompile("compileTimeExec{ scala.Predef.assert(u == 0) }")
    // ^ Error:(39, 24) exception during macro expansion: java.lang.AssertionError: assertion failed
    
    //zt The default toString method in Symbol has changed from s"'$name" (2.12) to "Symbol($name)" (2.13)
    // https://github.com/scala/scala/blob/232521f418c1e2c535d3630b0a5b3972a06bbd4e/src/library/scala/Symbol.scala#L21C1-L21C53
    compileTimeExec{ scala.Predef.assert(u == 2 + 10) }
    
    val local = Symbol("oops")
    assertDoesNotCompile("compileTime{ local.name }")
    // Error:(43, 26) Non-static identifier 'local' of type: Symbol
    
    object Local { type T } 
    assertDoesNotCompile("assert(compileTimeEval{ Option.empty[Local.T] } == None)")
    // Error:(45, 31) Could not access type symbol squid.statics.CompileTimeTests#<local CompileTimeTests>#Local$#T. Perhaps it was defined in the same project, but should be compiled separately.
    // ^ TODO should detect that symbol 'squid.statics.CompileTimeTests#<local CompileTimeTests>#Local$#T' is fishy and use a type tag instead...
    // ^ Reference to local type symbols works! – EDIT: USED to work
    //   This is[was] because it falls back on uninterpretedType, which BaseInterpeter defines as `sru.typeOf[A].typeSymbol.asType`
    
  }
  
  test("Implicits") {
    
    def lostStaticValue(implicit sym: CompileTime[Symbol]) = {
      assertDoesNotCompile("compileTime{ sym.get.name }")
      sym.get
    }
    
    implicit val foo = CompileTime(Symbol("foo"))
    
    val bar = compileTime{ foo.get.name }
    
    assert(lostStaticValue == Symbol("foo"))
    assert(`test withStaticSymbol`(3) == "foofoofoo")
    assert(`test withStaticSymbol`(2)(Symbol("bar")) == "barbar") // implicit conersion/lifting to CompileTime
    
    val res = compileTime{ `test withStaticSymbol`(3) }
    assertDoesNotCompile("""compileTimeExec{ scala.Predef.assert(res == Symbol("foofoofoo0").name) }""")
    compileTimeExec{ scala.Predef.assert(res == Symbol("foofoofoo").name) }
  
  }
  
  test("Lack of Separate Compilation") {
    
    assertDoesNotCompile("compileTimeEval{ s0.name }")
    // ^ Error:(76, 20) Could not access type symbol squid.statics.CompileTimeTests.
    //                  Perhaps it was defined in the same project, but should be compiled separately.
    
  }
  
  test("Static Eval to Constants") {
    
    assert(compileTimeEval{ s.name } == "ok")
    
  }
  
  test("Static Eval to Serializable") {
    
    assert(compileTimeEval{ List(s,s,s) } == List(Symbol("ok"),Symbol("ok"),Symbol("ok")))
    
  }
  
}
