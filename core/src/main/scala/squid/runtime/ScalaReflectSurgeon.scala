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

package scala.reflect
package runtime

import squid._
import utils._
import meta.RuntimeUniverseHelpers._

/** The Stuff of Nightmares */
object ScalaReflectSurgeon {
  
  lazy val cache: TwoWayCache[Class[_], sru.ClassSymbol] = {
    try {
        val cls = Class.forName("scala.reflect.runtime.JavaMirrors$JavaMirror")
        val mtd = cls.getDeclaredMethod("classCache")
        mtd.setAccessible(true)
        mtd.invoke(srum).asInstanceOf[TwoWayCache[Class[_], sru.ClassSymbol]]
    } catch {
        //zt Compilation error, method classCache not found
        case e: Exception => 
            new TwoWayCache[Class[_], sru.ClassSymbol]
    }

  }
  
  private val inl = sru.asInstanceOf[reflect.internal.Internals with reflect.macros.Universe]
  /** Open-heart surgery */
  def changeType_omgwtfbbq(sym: sru.Symbol, tpe: sru.Type): Unit = {
    inl.internal.setInfo(sym.asInstanceOf[inl.Symbol], tpe.asInstanceOf[inl.Type])
  }
  
}
