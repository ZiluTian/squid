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

package squid.utils.algo

import squid.utils._

/** Adapted from http://voidmainargs.blogspot.ch/2011/09/semantic-tableaux-in-less-than-90-lines.html */
class Tableaux {
  
  type Symbol
  def equal(s0:Symbol,s1:Symbol) = s0 == s1
  def hash(s:Symbol) = s.##
  
  import scala.annotation.tailrec
  
  sealed abstract class Formula {
    def triv(q: Formula) = (this,q) |>? { case (Trivial,_) => q case (_,Trivial) => this }
    def ∧(q: Formula) = triv(q) Else Conjunction(this, q)
    def ∨(q: Formula) = triv(q) Else Disjunction(this, q)
    def →(q: Formula) = triv(q) Else Implication(this, q)
    def ↔(q: Formula) = triv(q) Else Equivalence(this, q)
    def ⊕(q: Formula) = triv(q) Else Xor(this, q)
    def unary_! = ¬(this)
    override def toString: String = this match {
      case Atom(symbol) => s"$symbol"
      case Conjunction(p, q) => s"$p ∧ $q"
      case Disjunction(p, q) => s"($p ∨ $q)"
      case Implication(p, q) => s"$p → $q"
      case Equivalence(p, q) => s"$p → $q"
      case Xor(p, q) => s"$p ⊕ $q"
      case ¬(p) => s"¬($p)"
      case Trivial => "T"
    }
  }
  
  case class Atom(symbol: Symbol) extends Formula {
    override def equals(that: scala.Any): Bool = 
      (that |>? { case Atom(that) => equal(symbol,that) }) == Some(true)
    override def hashCode(): Int = hash(symbol)
  }
  case class Conjunction(p: Formula, q: Formula) extends Formula
  case class Disjunction(p: Formula, q: Formula) extends Formula
  case class Implication(p: Formula, q: Formula) extends Formula
  case class Equivalence(p: Formula, q: Formula) extends Formula
  case class Xor(p: Formula, q: Formula) extends Formula
  case class ¬(p: Formula) extends Formula
  case object Trivial extends Formula
  
  object Formulas {
    implicit def symbolToAtom(sym: Symbol): Atom = Atom(sym)
    
    type Leaf = Set[Formula]
    
    def applyRule(f: Formula): List[Leaf] = f match {
      case f if isLiteral(f) => List(Set(f))
      case ¬(¬(a)) => List(Set(a))
      case Conjunction(a, b) => List(Set(a, b))
      case ¬(Disjunction(a, b)) => List(Set(¬(a), ¬(b)))
      case ¬(Implication(a, b)) => List(Set(a, ¬(b)))
      case Disjunction(a, b) => List(Set(a), Set(b))
      case ¬(Conjunction(a, b)) => List(Set(¬(a)), Set(¬(b)))
      case (Implication(a, b)) => List(Set(¬(a)), Set(b))
      case Equivalence(a, b) => List(Set(a, b), Set(¬(a), ¬(b)))
      case ¬(Equivalence(a, b)) => List(Set(a, ¬(b)), Set(¬(a), b))
    }
    
    def isLiteral(f: Formula): Boolean =
      f match {
        case Atom(_) => true
        case ¬(Atom(_)) => true
        case Trivial => true
        case ¬(Trivial) => true
        case _ => false
      }
    
    def semanticTableau(f: Formula): List[Leaf] = { // TODO make it a lazy val in Formula
      
      def combine(rec: List[Leaf], f: Formula): List[Leaf] =
        for (a <- applyRule(f); b <- rec) yield (a ++ b)
      
      def openLeaf(leaf: Leaf): List[Leaf] =
        if (leaf forall isLiteral)
          List(leaf)
        else
          leaf.foldLeft(List(Set.empty: Leaf))(combine) flatMap (openLeaf)
      
      openLeaf(Set(f))
    }
    
    
    @tailrec
    def isClosedLeaf(f: Leaf): Boolean = {
      if (f.isEmpty) false
      else {
        (f.head match {
          case Atom(_) => f.tail.exists(_ == ¬(f.head))
          case ¬(Atom(a)) => f.tail.exists(_ == Atom(a))
          case Trivial => false
          case ¬(Trivial) => true // FIXME?
          case _ => false
        }) || isClosedLeaf(f.tail)
      }
    }
    
    def isOpenLeaf(f: Leaf) = !isClosedLeaf(f)
    
    def isValid(f: Formula): Boolean = {
      //println("Valid? "+f)
      semanticTableau(¬(f)) forall isClosedLeaf  //and (r => println(s"Valid? $f  =>  $r"))
    }
    
    def isSatisfiable(f: Formula): Boolean = semanticTableau(f) exists isOpenLeaf  //and (r => println(s"Sat? $f  =>  $r"))
    
  }
  
}
