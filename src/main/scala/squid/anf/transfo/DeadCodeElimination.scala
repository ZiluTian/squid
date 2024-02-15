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

package squid
package anf.transfo

import utils._
import ir._
import utils.Debug.show

/*
  * 
  * TODO use a `referentiallyTransparent` method, typically provided by an effect
  * 
  */
trait DeadCodeElimination extends FixPointRuleBasedTransformer with BottomUpTransformer { self =>
  import base.Predef._
  import self.base.InspectableCodeOps
  import self.base.IntermediateCodeOps
  
  def referentiallyTransparent(r: base.Rep): Bool = true // !!!
  
  rewrite {
    
    case code"val x: $xt = $init; $body: $bt" if referentiallyTransparent(init.rep) =>
      body subs Symbol("x") -> Abort()
      
  }
  
}
