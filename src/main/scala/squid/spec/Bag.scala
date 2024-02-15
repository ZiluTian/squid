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

package squid.spec

import scala.collection.mutable

/**
  * The generic data structure
  * 
  * Contracts:
  *   no double remove
  *   no access after remove
  *   no get on non-existing refs
  *   no pop on empty
  *   etc.
  * 
  * Note: could use options for things like 'pop', and otimize them away? (state as future work? we don't currently have generic ADT optims)
  * 
  * TODO: we use Refs in the API; this may make the client code perform unnecessary lookups; can we optimize it? or make the API better?
  * 
  * TODO: verify our system works well with PDTs like myBag.Ref
  * 
  * 
  */
trait Bag[A] {
  
  type Ref
  
  // Basic Operations
  
  def add(x: A): Ref
  def get(r: Ref): A
  def remove(r: Ref): Unit  
  def size: Int
  
  /** Pops an arbitrary element */
  def pop: A
  
  
  // Iteration
  
  def fold[R](init: R)(f: (R, A) => R): R
  def foreach(f: A => Unit): Unit
  
  
  // More Specialized
  
  //def find(p: A => Boolean): Bag[Ref]  // TODO can specialize this? cf. depending on the context! -- may need something like CanBuildFrom
  def find(p: A => Boolean): Iterator[Ref]
  def findFirst(p: A => Boolean): Option[Ref] = {val f = find(p); if (f.hasNext) Some(f.next()) else None}
  
  //def popMin: A
  //def popMax: A
  
  def count(p: A => Boolean): Int
  
  
  // Q: actually useful to have that as an inner class?
  def stats(implicit ord: Ordering[A]): Stats
  trait Stats {
    def min: Int
    def max: Int
    def popMin: A
    def popMax: A
  }
  
}
object Bag {
  
  def apply[A](xs: A*) = new DebugBag[A](mutable.Buffer(xs: _*))
  
  
}

/**
  * The default implementation of Bag, geared towards easy debugging rather than efficiency
  * It will consistently and deterministically throw exceptions for contract violations
  * 
  * Implementation
  * Main approaches
  *   - [relocating] on remove, fill up with last elem and reduce size -- but update reference! (or reference table?)
  *     would probably work by Ref's being mutable objects
  *   - on remove, add freed index to a free list
  *   - on remove, shift all elements (pretty bad -- but could be amortized if use several buffers?)
  *     need to keep a table of index offsets (not sure feasible)
  * 
  * Note that for actual specialized versions, the relocating approach will probably become worst as values become big if we have SOA optim
  * 
  */
class DebugBag[A](elems: mutable.Buffer[A]) extends Bag[A] {
  
  type Ref = Int // represents an index in the buffer
  
  def add(x: A): Ref = { elems += x; elems.size-1 }
  def get(r: Ref): A = elems(r)
  def remove(r: Ref) = ???
  
  def size: Int = elems.size
  
  def pop = elems.remove(elems.size-1)
  
  def fold[R](init: R)(f: (R, A) => R): R = elems.foldLeft(init)(f)
  def foreach(f: A => Unit) = elems.foreach(f)
  
  //def find(p: A => Boolean) = Bag(elems.find(p))
  def find(p: A => Boolean) = elems.iterator.zipWithIndex.filter(e => p(e._1)).map(_._2)
  //def findFirst(p: A => Boolean): Option[Ref] = elems.iterator.zipWithIndex.collectFirst{case (e,i) if p(e) => i}
  
  def popMin: A = ???
  def popMax: A = ???
  
  def count(p: A => Boolean): Int = elems.count(p)
  
  def stats(implicit ord: Ordering[A]) = ???
  
}
















