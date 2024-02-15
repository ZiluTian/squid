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

package squid.utils

import org.scalatest.funsuite.AnyFunSuite

class CollectionUtilsTest extends AnyFunSuite {
  import CollectionUtils._
  
  test("List Test") {
    
    val (even,odd) = List(1,2,3,4,5) collectPartition {
      case n if n % 2 == 1 => s"${n/2}*2+1"
    }
    
    assert((even: List[String]) == List("0*2+1", "1*2+1", "2*2+1"))
    assert((odd: List[Int]) == List(2, 4))
    
    {
      val (evenStrs,oddStrs) = List(1,2,3,4,5) mapSplit {
        case n if n % 2 == 0 => Left( s"${n/2} * 2")
        case n               => Right(s"${n/2} * 2 + 1")
      }
      
      assert((evenStrs: List[String]) == List("1 * 2", "2 * 2"))
      assert((oddStrs: List[String]) == List("0 * 2 + 1", "1 * 2 + 1", "2 * 2 + 1")) 
    }
    
    {
      val (evenStrs,oddStrs) = List(1,2,3,4,5) collectOr ({
        case n if n % 2 == 0 => s"${n/2} * 2"
      }, n => s"${n/2} * 2 + 1")
      
      assert((evenStrs: List[String]) == List("1 * 2", "2 * 2"))
      assert((oddStrs: List[String]) == List("0 * 2 + 1", "1 * 2 + 1", "2 * 2 + 1")) 
    }
    
  }
  
  test("String Test") {
    
    val (es,rest) = "el toro fuerte" collectPartition {
      case c @ ('e' | 'o') => c.toUpper
    }
    
    assert((es: String) == "EOOEE")
    assert((rest: String) == "l tr furt")
    
    val (as,bs) = "Asi es el Toro Fuerte" mapSplit {
      case c if c.isUpper => Left(c)
      case c => Right(c)
    }
    assert(as == "ATF" && bs == "si es el oro uerte")
    
    val (as0,bs0) = Map(1 -> "ok", 2 -> "ko") mapSplit {
      case kv if kv._1 % 2 == 0 => Right(kv)
      case kv => Left(kv)
    }
    assert(as0 == Map(1 -> "ok"))
    
  }
  
  
  test("Seq of Either to 2 Seqs") {
    
    val ls = Seq(Left(Symbol("ok")), Right(42), Right(666), Left(Symbol("ko")), Right(-1))
    
    val (syms, ints) = ls mapSplit identity
    
    assert((syms: Seq[Symbol]) == List(Symbol("ok"), Symbol("ko")))
    assert((ints: Seq[Int]) == List(42, 666, -1))
    
  }
  
  test("With map") {
    
    val ctx = Map(Symbol("a") -> 1, Symbol("b") -> 2) map {case(n,v) => n->(n,v)}
    val (bound, unbound) = Vector(Symbol("a"), Symbol("a"), Symbol("c"), Symbol("b")) collectPartition ctx
    
    assert( (bound: Vector[(Symbol, Int)], unbound: Vector[Symbol]) == (Vector((Symbol("a"),1), (Symbol("a"),1), (Symbol("b"),2)),Vector(Symbol("c"))) )
    
  }
  
  
  test("Zip and") {
    
    assert( (List(1,2,3) zipAnd List(0,1,2))(_ + _) == List(1,3,5) )
    // in fact, similar to:
    assert( (List(1,2,3), List(0,1,2)).zipped.map(_ + _) == List(1,3,5) )
    
  }
  
  
  test("In-place filter") {
    import scala.collection.mutable
    
    val b0 = mutable.Buffer(1,2,3,5,7,8)
    val b1 = b0.clone
    
    b0.filter_!(_ % 2 == 0)
    b1.filter_!(_ % 2 == 1)
    
    assert(b0 == Seq(2,8))
    assert(b1 == Seq(1,3,5,7))
  }
  
  
}


