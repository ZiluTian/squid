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
package quasi

import utils._
import squid.lang.Base

import scala.reflect.macros.blackbox

/** Helper used for the dependent method type */
abstract class BaseUser[C <: blackbox.Context](val macroContext: C) {
  def apply(b: Base)(insert: (macroContext.Tree, Map[String, b.BoundVal]) => b.Rep): b.Rep
}

/** QuasiConfig provides a way to configure the static semantics of quoted code in Squid.
  * In order to use this, import the members of an instance of `MyIR.Predef[MyQuasiConfig]`.
  * For example: {{{
  *   object IR extends squid.ir.SimpleAST
  *   val MyPredef = new TestDSL.Predef[OpenCodeConfig]
  *   import MyPredef._
  * }}}
  * Important: for this to work, your `QuasiConfig` should be defined in a difference compilation unit (project). 
  */
abstract class QuasiConfig {
  
  /** If this is set to `true`, all inferred code types will be of the form `OpenCode[T]` (the context info is ignored).
    * This can be useful in situations when one doesn't care about context information, and wants type inference to work
    * well in the presence of invariant types and implicits. */
  val inferOpenCode: Bool = false
  
  /** This abstraction is likely to go away or be simplified because it complicates the Squid implementation, with little return. */
  def embed(c: blackbox.Context)(baseTree: c.Tree, baseType: c.Type, user: BaseUser[c.type]): c.Tree
  
}

class DefaultQuasiConfig extends QuasiConfig {
  
  def embed(c: blackbox.Context)(baseTree: c.Tree, baseType: c.Type, user: BaseUser[c.type]) = {
    import c.universe._
    
    object Meta extends MetaBases {
      private var cnt = 0
      val u: c.universe.type = c.universe
      //def freshName(hint: String) = c.freshName(u.TermName(hint)) // Creates problems with the Scala compiler when the produced code is processed by another macro
      def freshName(hint: String) = TermName(s"_${cnt}_$hint") alsoDo (cnt += 1)
    }
    object base extends Meta.MirrorBase(baseTree, baseType |> some)
    
    val code = user(base) {
      case (q"$tr: _*", bind) => q"$tr map (__$$ir => ${base.substitute(q"__$$ir.rep", bind mapValues base.readVal toMap)}): _*"
      case (tr, bind) => base.substitute(q"$tr.rep", bind mapValues base.readVal toMap)
    }
    
    q"..${base.mkSymbolDefs}; $code"
  }
  
}

/** A QuasiConfig where code types are always inferred to be of the form `OpenCode[T]` */
class OpenCodeConfig extends DefaultQuasiConfig {
  override val inferOpenCode = true
}
