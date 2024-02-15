// Copyright 2017 EPFL DATA Lab (data.epfl.ch)
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

package sfusion

import org.scalatest.funsuite.AnyFunSuite
import squid.ir.ClassEmbedder
import squid.utils._
import impl._

import scala.collection.mutable

/**
  * TODO an assertEq function that also tries different consumption modes and watches return values
  * 
  */
class ImplTests extends AnyFunSuite {
  
  test("Unfold") {
    
    val s = Stream.continually(0)
    assert(take(unfold(s){xs => xs.headOption map (h => (h,xs.tail))})(10) |> toBuffer equals Seq.fill(10)(0))
    
  }
  
  test("Take") {
    
    def s = fromIndexed(1 to 10)
    assert((take(s)(3) |> toBuffer) == Seq(1,2,3))
    
    assert(take(s)(3)(a => true))
    
    //val t = s
    //println(take(s)(30)(a => println(a) before true))
    //println(take(s)(30)(a => true))
    
    assert((take(continually(() => 0))(10) |> toBuffer) == Seq.fill(10)(0))
    
  }
  
  test("TakeWhile") {
    
    def s = fromIndexed(1 to 15)
    val r = takeWhile(s)(_ < 5)
    
    val b = mutable.Buffer[Int]()
    for (i <- 1 to 4) {
      assert(!r{a => b += a;false})
    }
    assert(r{a => b += a;false})
    assert(r{a => fail})
    
    assert(b.mkString == "1234")
    
  }
  
  
  test("Concat") {
    
    def s0 = fromIndexed(1 to 3)
    def s1 = fromIndexed(10 to 15)
    val r0 = concat(s0,s1)
    
    val b = mutable.Buffer[Int]()
    
    for (i <- 1 to 5) {
      assert(!r0{a => b += a;false})
    }
    assert(b.mkString == "1231011")
    assert(r0{a => b += a;true})
    assert(r0{a => fail})
    assert(b.mkString == "123101112131415")
    
    b.clear
    
    val r1 = concat(s0,s1)
    for (i <- 1 to 8) assert(!r1{a => b += a;false})
    assert(r1{a => b += a;false})
    assert(b.mkString == "123101112131415")
    
    b.clear
    
    val r2 = concat(fromIndexed(1 until 1),s0)
    assert(!r2{a => b += a;false})
    assert(b.mkString == "1")
    
  }
  
  
}
