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

import scala.collection.BuildFrom
import scala.collection.mutable
import scala.collection.IterableOnce
import scala.collection.IterableOps

object CollectionUtils {
// Cannot construct a collection of type Right with elements of type QuasiEmbedder.this.c.universe.SelectFromTypeTree based on a collection of type scala.collection.immutable.Map[QuasiEmbedder.this.c.universe.TermName,QuasiEmbedder.this.c.universe.Type]
// scp: Iterable[(TermName,Type)]
// val f: ((TermName, Type)) => Either[(TermName, Type), Tree] = (x: (TermName, Type))
  // implicit def buildFromAny[Element, Collection[+Element] <: Iterable[Element] with IterableOps[Any, Collection, Any]]
  //   : BuildFrom[Collection[Any], Element, Collection[Element]] =
  //   scala.collection.BuildFrom.buildFromIterableOps[Collection, Any, Element]

  /** Works, but not AnyVal */
  implicit class TraversableOnceHelper[A,Repr](private val repr: Repr)(implicit isTrav: Repr => IterableOnce[A]) {
    def collectPartition[B,Lft](pf: PartialFunction[A, B])
    (implicit bfLeft: BuildFrom[Repr, B, Lft], bfRight: BuildFrom[Repr, A, Repr]): (Lft, Repr) = {
      val left = bfLeft.newBuilder(repr)
      val right = bfRight.newBuilder(repr)
      val it = repr.iterator
      while (it.hasNext) {
        val next = it.next()
        if (!pf.runWith(left += _)(next)) right += next
      }
      left.result() -> right.result()
    }
    
    // This is probably the most useful version:
    def collectOr[B,C,Lft,Rgt](pf: PartialFunction[A, B], f: A => C)
    (implicit bfLeft: BuildFrom[Repr, B, Lft], bfRight: BuildFrom[Repr, C, Rgt]): (Lft, Rgt) = {
      val left = bfLeft.newBuilder(repr)
      val right = bfRight.newBuilder(repr)
      val it = repr.iterator
      while (it.hasNext) {
        val next = it.next()
        if (!pf.runWith(left += _)(next)) right += f(next)
      }
      left.result() -> right.result()
    }
    
    def mapSplit[B,C,Lft,Rgt](f: A => Either[B,C])
    (implicit bfLeft: BuildFrom[Repr, B, Lft], bfRight: BuildFrom[Repr, C, Rgt]): (Lft, Rgt) = {
      val left = bfLeft.newBuilder(repr)
      val right = bfRight.newBuilder(repr)
      val it = repr.iterator
      while (it.hasNext) {
        f(it.next()) match {
          case Left(next) => left += next
          case Right(next) => right += next
        }
      }
      left.result() -> right.result()
    }
    
    
    def zipAnd[B,C,NewRepr](other: IterableOnce[B])(f: (A,B) => C)
    (implicit bf: BuildFrom[Repr, C, NewRepr]): NewRepr = {
      val res = bf.newBuilder(repr)
      val it0 = repr.iterator
      val it1 = other.iterator
      while (it0.hasNext && it1.hasNext) {
        val next = f(it0.next(), it1.next())
        res += next
      }
      res.result()
    }
    
  }
  
  implicit class MutBufferHelper[A](private val repr: mutable.Buffer[A]) {
    def filter_!(p: A => Boolean) = {
      var removed = 0
      var i = 0
      while (i < repr.size) {
        val e = repr(i)
        if (p(e)) {
          if (removed != 0)
            repr(i-removed) = e
        } else {
          removed += 1
        }
        i += 1
      }
      repr.dropRightInPlace(removed)
    }
  }
  
  implicit class MutSetHelper[A](private val repr: mutable.Set[A]) {
    @inline def setAndIfUnset(a: A, action: => Unit): Unit = {
      if (!repr(a)) {
        repr += a
        action 
      }
    }
    @inline def setAndIfUnset[R](a: A, action: => R, default: => R): R = {
      if (!repr(a)) {
        repr += a
        action 
      } else default
    }
  }
  
}
