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

import annotation.StaticAnnotation
import reflect.api.Position

object MacroUtils {
  import scala.reflect.macros.blackbox.Context

  type DebugLevel = NoDebug
  // type DebugLevel = MacroDebug

  trait DebugInfo

  trait NoDebug extends DebugInfo
  def NoDebug = new NoDebug {}

  trait MacroDebug extends DebugInfo
  def MacroDebug = new MacroDebug {}

  trait ApplyDebug extends MacroDebug
  def ApplyDebug = new ApplyDebug {}

  trait UnapplyDebug extends MacroDebug
  def UnapplyDebug = new UnapplyDebug {}

  trait TypeParamDebug extends MacroDebug
  def TypeParamDebug = new TypeParamDebug {}

  trait TypeInferDebug extends MacroDebug
  def TypeInferDebug = new TypeInferDebug {}

  private[squid] class MacroSetting(debug: Boolean) extends StaticAnnotation

  private[squid] case class MacroDebugger[C <: Context](c: C) {

    import c.universe._

    val debugOption = c.macroApplication.symbol.annotations.filter(
      _.tree.tpe <:< typeOf[MacroSetting]
    ).headOption.flatMap(_.tree.children.tail.collectFirst {
        case q"true"  => true
        case q"false" => false
      }).getOrElse(false)

    def apply[Dbg <: DebugInfo: c.TypeTag] = {
      val dbgt = c.typeOf[Dbg]
      new Debugger(debugOption || dbgt <:< c.typeOf[DebugLevel] || {
        // We used to infer an implicit value to determine whether to print debug info or not, but this mechanism was
        // not very practical, compared to the more useful `dbg_` derivations of the usual methods, which allows more
        // precise selection of what debug info to print.
        // I actually disabled the implicit resolution way of configuring debugging following a change in Scala 2.12
        // that made the compiler crash with "illegal cyclic reference involving package object X" on embedded package
        // objects such as example/sfusion/algo/package and example/sfusion/impl/package.
        /*
        val edb = c.inferImplicitValue(c.typeOf[DebugInfo])
        dbgt <:< edb.tpe
        */
        false
      })
    }
    class Debugger(enabled: Boolean) {
      /**
       * Some IDEs (eg: Idea) display prints from macros as warnings, and prevents their repetition, hampering macro
       * debugging. This prints a unique integer in front of the message to always print it.
       */
      def apply(x: => Any, xs: Any*): Unit = if (enabled) {
        println(s"$debugNum> ${(x +: xs) mkString " "}")
        debugNum += 1
      }
      def pos(x: => Any, xs: Any*): Unit = if (enabled) {
        c.warning(c.enclosingPosition, s"${debugNum} > ${((x +: xs) mkString "  ")}")
        debugNum += 1
      }
      def debugOptionEnabled = debugOption
      
    }
    
  }

  private var debugNum = 0
  
  // cf: https://issues.scala-lang.org/browse/SI-7934
  @deprecated("", "") class Deprecated { def lineContentForwarder = (pos: Position)=>pos.lineContent }
  object Deprecated extends Deprecated
  
  private[squid] def showPosition(pos: Position, indent: String = "  ") = {
    val line = Deprecated.lineContentForwarder(pos)
    if (line exists (_ != ' ')) (
      indent + line + "\n"
      + indent + (" " * pos.column) + "^\n"
    )
    else ""
  }
  
  //private[scp] def showCodeOpen(t: Context# Tree) = {
  
  // Note: depending on where it comes from (typed tree or external symbol), Scala will not always display vararg symbols with the same name... (thanks for that!)
  val SCALA_REPEATED = Set("scala.<repeated>", "scala.<repeated...>")
  
}













