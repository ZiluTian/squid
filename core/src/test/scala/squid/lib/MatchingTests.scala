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

package squid.lib
import org.scalatest.funsuite.AnyFunSuite
import squid.lib.matching._
// import squid.utils.shims._

class MatchingTests extends AnyFunSuite {
  
  val x = Option(1)
  
  test("Basic matchign tests") {
    
    assert( Match(x)(Case[Some[Int]](_.value + 1)) == 2 )
    assertThrows[MatchError]( Match(x)(Case[None](_ => 0)) )
    
    val m = Match(x)(
      Case[None.type](_ => 0),
      Case[Some[Int]](_.value + 1)
    )
    assert(m == 2)
    
    // for some reason these apparently compile now...:
    //assertDoesNotCompile("Match(Some(1))(Case[None.type](_ => 0))")
    // ^ found   : squid.lib.matching.Case[None.type,Int]; required: squid.lib.matching.Case[Some[Int],?]
    //assertDoesNotCompile("Match(None)(Case[Some[Int]](_.value + 1))")
    
    assert( Match(Some(1))(Case[Some[Int]](_.value + 1)) == 2 )
    
  }
  
}
