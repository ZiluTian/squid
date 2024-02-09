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
package quasi

import utils._
import squid.lang.Base

/* TODO: make it work with intermediate bases (eg: `ViaASTQuasiConfig`) */
abstract class QuasiTypeEmbedder[C <: scala.reflect.macros.blackbox.Context, B <: Base](val c: C, val base: B, debug: String => Unit) {
  import c.universe._
  
  val helper: meta.UniverseHelpers[c.universe.type]
  import helper._
  
  val baseTree: Tree
  
  class Impl extends ModularEmbedding[c.universe.type, B](c.universe, base, debug) {
    
    def insertTypeEvidence(ev: this.base.TypeRep): this.base.TypeRep = ev
    
    /* We override this method to make sure to try and find an implicit for a given abstract type, before decomposing it;
     * this is to allow the common metpaprogramming pattern where one carries bundles of abstract types and their
     * implicit representations with one's code values. */
    override def liftTypeUncached(tp: Type, wide: Boolean): this.base.TypeRep = tp match {
      case TypeRef(prefix, sym, targs)
        if prefix != NoPrefix
        && !sym.isClass
        && (ExtractedType unapply tp isEmpty)
      =>
        debug(s"$sym is not a class, so we should look for an implicit in scope first")
        lookForTypeImplicit(tp) getOrElse super.liftTypeUncached(tp, wide)
      case _ => super.liftTypeUncached(tp, wide)
    }
    
    val QTSym = symbolOf[QuasiBase#CodeType[_]]
    
    def lookForTypeImplicit(tp: Type): Option[this.base.TypeRep] = {
      
      val irType = c.typecheck(tq"$baseTree.CodeType[$tp]", c.TYPEmode) // TODO use internal.typeRef
      debug(s"Searching for implicit of type: $irType")
      c.inferImplicitValue(irType.tpe, withMacrosDisabled = true) match {
        case EmptyTree =>
        case impt =>
          debug(s"Found: "+showCode(impt))
          return Some(q"$impt.rep".asInstanceOf[this.base.TypeRep] // FIXME
            |> insertTypeEvidence)
      }
      
      val vals = c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.context.enclosingContextChain.flatMap {
        _.scope collect {
          case sym if sym.isVal
            && sym.isInitialized // If we look into the type of value being constructed (eg `val x = exp"42"`),
                                 // it will trigger a 'recursive value needs type' error
            //&& sym.name.toString == tp.typeSymbol.name.toString // used to compare names directly, but sometimes failed because they came from different cakes...
          =>
            sym -> sym.tpe.dealias // dealiasing so for example base.Predef.IR[_,_] ~> base.IR[_,_]
        }
      }.asInstanceOf[List[(TermSymbol, Type)]]
      
      //val PredefQTSym = symbolOf[QuasiBase#Predef[_ <: QuasiConfig]#CodeType[_]]
      //val PredefQTSym = typeOf[QuasiBase#Predef[_ <: QuasiConfig]#CodeType[_]].typeSymbol
      // ^ For some reson, these always return a symbol s where s.fullName == "squid.quasi.QuasiBase.CodeType"
      
      vals foreach {
        case (sym, TypeRef(tpbase, QTSym /*| PredefQTSym*/, tp0::Nil)) 
          if tpbase =:= baseTree.tpe 
          && tp0 <:< tp && tp <:< tp0 // NOTE: for some godforsaken reason, sometimes in Scala this is not the same as `tp0 =:= tp`
          // For example in  {{{ (typs:List[CodeType[_]]) map { case typ: CodeType[t] => dbg.implicitType[t] } }}}
        =>
          debug("FOUND QUOTED TYPE "+sym)
          return Some(q"$sym.rep".asInstanceOf[this.base.TypeRep] // FIXME
            |> insertTypeEvidence)
        //case (sym, TypeRef(tpbase, QTSym, tp::Nil)) =>
        //  debug(s"Note: $tpbase =/= ${baseTree.tpe}")
        //case (sym, TypeRef(tpbase, qtsym, tp::Nil)) =>
        //  debug(s"$qtsym ${qtsym.fullName} ${PredefQTSym.fullName} ${QTSym.fullName} ${qtsym == PredefQTSym}")
        case _ =>
      }
      
      debug(s"No implicit `$irType` found")
      None
    }
    
    /* This method is overridden to handle extracted types; for the record, this is an example log of how they get here: 
        > Matching type squid.FoldTupleVarOptim.ta.Typ
        > !not a class: squid.FoldTupleVarOptim.ta.Typ
        > (squid.FoldTupleVarOptim.ta.Typ,class scala.reflect.internal.Types$AbstractNoArgsTypeRef,squid.FoldTupleVarOptim.ta.Typ)
        > (squid.FoldTupleVarOptim.ta.type,type Typ,List())
        > ( <: squid.quasi.QuasiBase.<extruded type>,List())
        > Matching type squid.FoldTupleVarOptim.ta.Typ
        > !not a class: squid.FoldTupleVarOptim.ta.Typ
        > (squid.FoldTupleVarOptim.ta.Typ,class scala.reflect.internal.Types$AbstractNoArgsTypeRef,squid.FoldTupleVarOptim.ta.Typ)
        > (squid.FoldTupleVarOptim.ta.type,type Typ,List())
        > ( <: squid.quasi.QuasiBase.<extruded type>,List())
        > Unknown type, falling back: squid.FoldTupleVarOptim.ta.Typ
        > Lifting unknown type squid.FoldTupleVarOptim.ta.Typ (squid.FoldTupleVarOptim.ta.Typ)
        > Searching for an `FoldTupleVarOptim.this.base.CodeType[squid.FoldTupleVarOptim.ta.Typ]` implicit
        > FOUND QUOTED TYPE value ta
    */
    override def unknownTypefallBack(tp: Type): this.base.TypeRep = {
      
      debug(s"Lifting unknown type $tp (${tp.widen.dealias})")
      
      if (tp.widen =:= ExtrudedType || tp.widen.contains(ExtrudedType.typeSymbol)) { // was: contains(symbolOf[QuasiBase.`<extruded type>`])
        debug(s"Detected widened type hole: ${tp.widen}")
        val purged = tp.toString.replaceAll(ExtrudedType.toString, "<extruded type>")
        throw EmbeddingException(s"Precise info for extracted type was lost, " +
          s"possibly because it was extruded from its defining scope " +
          s"or because the least upper bound was obtained from two extracted types, in: $purged")
      }
      
      lookForTypeImplicit(tp) foreach (return _)
      
      if (tp <:< ExtrudedType && !(tp <:< Null) // Note that: tp <:< Nothing ==> tp <:< Null so no need for the test
          || ExtractedType.unapply(tp).nonEmpty) {
        
        throw EmbeddingException(s"Could not find type evidence associated with extracted type `$tp`.")
        
      } else {
        
        val tagType = c.typecheck(tq"_root_.scala.reflect.runtime.universe.TypeTag[$tp]", c.TYPEmode)
        
        c.inferImplicitValue(tagType.tpe) match {
          case EmptyTree =>

            if (tp.typeSymbol == symbolOf[Array[_]]) {
              // Note: srum.runtimeClass(sru.typeOf[Array[Int]]).toString == [I
              throw EmbeddingException.Unsupported("Arrays of unresolved type.")
            }

            throw EmbeddingException(s"Could not find type representation for: $tp\n\t" +
              s"consider providing a scala.reflect.runtime.universe.TypeTag implicit to embed it as uninterpreted.")
            
          case impt =>
            
            // We used to do the following, which helped to let-bind (using the caching mechanism of `ModularEmbedding`,
            // since this would call the base's `uninterpretedType`). However, now `uninterpretedType` is overridden and
            // inserted types are aggregated and named, so that caching can index on them, so this is no more necessary:
            /*
            // Note: not using `impt`! cf. below... (we just ensure it exists to give the better error message above)
            super.unknownTypefallBack(tp) // unnecessary, as it just generates a call to uninterpretedType without the implicit resolved
            // ^ actually useful to cache the tag!
            */
            
            q"$baseTree.uninterpretedType($impt)".asInstanceOf[this.base.TypeRep] |> insertTypeEvidence
            
            // Older:
            /* Does not help, as it bypasses the underlying base:
             * although the tree creation is cached, it will NOT get let-bound, and will be duplicated all over the place */
            //return typeCache.getOrElseUpdate(tp, q"$baseTree.uninterpretedType[$tp]($impt)".asInstanceOf[base.TypeRep])
            
        }
        
      }
      
    }
    
  }
  
}
