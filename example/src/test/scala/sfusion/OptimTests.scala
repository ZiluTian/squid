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

package sfusion

import org.scalatest.funsuite.AnyFunSuite
import squid.TestDSL
import squid.ir._
import squid.utils._

class OptimTests extends AnyFunSuite {
  
  object Compiler extends compiler.TestCompiler
  import Compiler.Code.Predef._
  import Compiler.Code.Quasicodes._
  Compiler.Code.embed(algo.`package`)
  
  test("Basics") {
    
    val c0 = code"Sequence(1,2,3,4,5,6,7,8,9,10).toString"
    
    val res = "Sequence(1,2,3,4,5,6,7,8,9,10)"
    assert(c0.run == res)
    
    val c1 = Compiler.wrapOptim("Basics") {
      Compiler.optimize(c0)
    }
    
    //assert(c1.run == res) // the hacky InterpreterBase has problems with by-name parameters due to Scala reflexion limitations... (here affecting squid.lib.And)
    assert(c1.compile == res)
    
  }
  
  test("Sieve") {
    
    val c0 = code{algo.primeSum(100)}
    
    val res = 101
    assert(c0.run == res)
    
    Compiler.wrapOptim("Sieve") {
      val r = Compiler.optimize(c0)
      assert(r.run == res)
    }
    
  }
  
  
  test("Join Lines") {
    
    val ls = List("lol","okay","test")
    val res = "lol\nokay\ntest"
    
    val c0 = code{algo.joinLinesSimple(_:Iterable[String])}
    val r0 = algo.joinLinesSimple(ls)
    
    assert(r0 == res)
    assert((ls |> c0.run) == r0)
    
    Compiler.wrapOptim("JoinLines") {
      
      // Note: for some reason, `transfo.IdiomsNormalizer` does not seem to eliminate extra `toString` calls,
      // despite that transformer working well on simple examples as in `NormalizationTests`.
      // This might indicate a subtle typing problem or an overridden symbol, to investigate...
      
      val r = Compiler.optimize(c0)
      assert((ls |> r.run) == r0)
      // ^ Note: when not inlined completely, `run` used to throw: scala.ScalaReflectionException: expected a member of class Boolean, you provided method squid.lib.And
    }
    
    val c1 = code{algo.joinLinesComplex(_:Iterable[String])}
    val r1 = algo.joinLinesComplex(ls)
    
    assert(r1 == res)
    assert((ls |> c1.run) == r1)
    
    Compiler.wrapOptim("JoinLinesComplexs") {
      val r = Compiler.optimize(c1)
      assert((ls |> r.run) == r0)
    }
    
  }
  test("FlatMap") {
    import Sequence._
    
    /*
    val ls = IndexedSeq[IndexedSeq[Char]]("lol","okay","test")
    
    //val c0 = ir{(xs:Iterable[Iterable[Char]]) => fromIterable(xs).flatMap(a => fromIterable(a)).fold(0)((ac,_) => ac + 1)}
    //val c0 = ir{(xs:IndexedSeq[IndexedSeq[Char]]) => fromIndexed(xs).flatMap(a => fromIndexed(a)).fold(0)((ac,_) => ac + 1)} // FIXME flatMap fusion does not kick in
    val c0 = ir{(xs:IndexedSeq[IndexedSeq[Char]]) => fromIndexed(xs).flatMap(a => fromIterable(a)).fold(123)((ac,_) => ac + 1)} // here it does
    val r0 = fromIndexed(ls).flatMap(a => fromIterable(a)).fold(123)((ac,_) => ac + 1)
    
    Compiler.wrapOptim("FlatMap") {
      val r = Compiler.optimize(c0)
      assert((ls |> r.run) == r0)
    }
    */
    
    val l1 = IndexedSeq(IndexedSeq(IndexedSeq(1,2,3,4)))
    
    val c1 = code{ (xs: IndexedSeq[IndexedSeq[IndexedSeq[Int]]]) => 
      fromIndexed(xs).flatMap(a => fromIndexed(a).flatMap(b => fromIndexed(b))).fold(123)(_ + _)
    }
    val r1 = l1 |> c1.run
    
    Compiler.wrapOptim("FlatMapInFlatMap") {
      val r = Compiler.optimize(c1)
      //assert((l1 |> r.run) == r1) // does not terminate (cf. interpreter's problems with by-names)
      assert((l1 |> r.compile) == r1)
    }
  
    val l2 = IndexedSeq(IndexedSeq(1,2,3))
    
    val c2 = code{ (xs: IndexedSeq[IndexedSeq[Int]]) => 
      //fromIndexed(xs).flatMap(a => fromIndexed(a)).flatMap(b => fromIndexed(b)).fold(123)(_ + _)
      //fromIndexed(xs).flatMap(a => fromIndexed(a).map(fromIndexed)).flatMap(b => b).fold(123)(_ + _) // FIXME doesn't fuse
      fromIndexed(xs).map(fromIndexed).flatMap(b => b).fold(123)(_ + _) // FIXedME doesn't fuse
    }
    val r2 = l2 |> c2.run
    
    Compiler.wrapOptim("MapThenFlatMap") {
      val r = Compiler.optimize(c2)
      assert((l2 |> r.compile) == r2)
    }
    
    
    val c3 = code{ (xs: IndexedSeq[IndexedSeq[IndexedSeq[Int]]]) => 
      fromIndexed(xs).flatMap(a => fromIndexed(a).map(fromIndexed)).flatMap(b => b).fold(123)(_ + _) // FIXME doesn't fuse
    }
    val r3 = l1 |> c3.run
  
    Compiler.wrapOptim("FlatMapThenFlatMap") {
      val r = Compiler.optimize(c3)
      assert((l1 |> r.compile) == r3)
    }
    
  }
  
  test("Zip") {
    import Sequence._
    
    val xs = IndexedSeq(1,2,3)
    //val ys = IndexedSeq('a, 'b, 'c, 'd)
    val ys = Seq('a, 'b, 'c, 'd)
    
    /*
    //val c0 = ir{(xs:IndexedSeq[Int],ys:IndexedSeq[Symbol]) => fromIndexed(xs)}
    val c0 = ir{(xs:IndexedSeq[Int],ys:Iterable[Symbol]) => 
      fromIndexed(xs).zip(fromIterable(ys).map(_.name)).fold("")((acc,xy) => acc + xy._1 + ":" + xy._2 + "; ")}
    //val r0 = fromIndexed(ls).flatMap(a => fromIterable(a)).fold(123)((ac,_) => ac + 1)
    
    Compiler.wrapOptim("ZipLinearLinear") {
      val r = Compiler.optimize(c0)
      println(r.run.apply(xs,ys))
      //assert(r.run.apply() == r0)
    }
    */
  
    val zs = ys.map(_.hashCode)
    
    val c1 = code{(xs:IndexedSeq[Int],ys:Iterable[Int]) => 
      fromIndexed(xs).flatMap(x => fromIterable(ys).map(_ + x)).zip(fromIterable(ys)).fold("")((acc,xy) => acc + xy._1 + xy._2)}
    
    val r1_ = c1.run.apply(xs,xs)
    val r1 = c1.run.apply(xs,zs)
    
    Compiler.wrapOptim("ZipFlatmapLinear") {
      val r = Compiler.optimize(c1) // FIXME flatmap fusion does not kick in
      //println(r)
      assert(r.run.apply(xs,xs) == r1_)
      //println(xs.flatMap(x => xs.map(_ + x)).zip(xs).foldLeft("")((acc,xy) => acc + xy._1 + xy._2))
      assert(r.run.apply(xs,zs) == r1)
    }
    
  }
  
  /* // TODO test:
  
  zip(map(fromIndexed,f),map(fromIterable,g))  --> should use efficient linear-fused impl
  
  zip(nested,linear)
  zip(linear,nested)
  zip(zip(linear,linear),linear)
  
  try to encode unzip as sugar for two maps...
    but then we'll probably need horrizontal fusion -- possibly in the ImplFlowOptimizer
  
  */
  
  
  /* // TODO
  test("Avg Words") {
    
    val txt = "Hello there. This is a sentence. This is another, longer one."
    val res = 11.0/3
    
    val c0 = ir{algo.avgWordsPerSentence _}
    val r0 = algo.avgWordsPerSentence(txt)
    //println(r0)
    //println(c0)
    
    assert(r0 == res)
    assert((txt |> c0.run) == r0)
    
    Compiler.wrapOptim("AvgWords") {
      val r = Compiler.optimize(c0)
      //assert((txt |> r.run) == r0)
    }
    
  }
  */
  
  
  /*
  test("WAT") {
    
    val c0 = ir{algo.wat}
    println(c0)
    Compiler.wrapOptim("Wat") {
      val r = Compiler.optimize(c0)
    }
    
  }
  */
  
}

