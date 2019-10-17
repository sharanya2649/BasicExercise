package com.techsophy.excercise
import org.scalatest.FunSuite

class Excercise02Test extends FunSuite {
  val exer=new Exercise02()
  test("reverse with tail recursion"){
      val input1=exer.reverse2(List(1,2,3,4))
      val output1=List(4,3,2,1)
      assert(input1===output1)
  }
  test("reverse using foldleft"){
    val input2=exer.reverse_fold(List(1,2,3,4))
    val output2=List(4,3,2,1)
    assert(input2===output2)
  }

  test("reverse using recursion"){
    val input2=exer.reverse3(List(1,2,3,4))
    val output2=List(4,3,2,1)
    assert(input2===output2)
  }
  test("trait"){
    val input3=exer.eval(Multiply(Number(2),Multiply(Number(2), Number(5))))
    val output3=20
    assert(input3===output3)
  }

  test("window"){
    val input5=exer.window(3,List(1,2,3,4,5,6))
    val output5=List(List(1,2,3),List(4,5,6))
    assert(input5===output5)
  }
  test("myzip"){
    val input6=exer.myZip(List(1,2,3),List("one","two","three"))
    val output6=List((1,"one"), (2,"two"), (3,"three"))
    assert(input6===output6)
  }
  test("fuse"){
    val input7=exer.fuse( Some(2),Some(4))
    val output7=Some((2,4))
    assert(input7===output7)
  }
  test("reduce"){
    val input8=exer.reduce(List(Map("Hi"->3),Map("Hello"->5,"Hi"->5)))
    val output8=Map("Hi" -> 8, "Hello" -> 5)
    assert(input8===output8)
  }

  test("reduce2"){
    val input9=exer.reduce2(List(Map("hi"->Map("hello"->2)),Map("good"->Map("abc"->4),"hi"->Map("ab"->8))))
    val output9=Map("hi"->Map("ab"->8,"hello"->2),"good"->Map("abc"->4))
    assert(input9===output9)
  }
  test("merge"){
    val input10=exer.merge(Map("hi"->5,"hello"->3),Map("abc"->2))
    val output10=Map("hi"->5,"hello"->3,"abc"->2)
    assert(input10===output10)
  }
  test("merge2"){
    val input11=exer.merge2(Map("hello"->Map("hi"->1)),Map("good"->Map("wer"->3),"hello"->Map("ab"->6)))
    val output11=Map("hello" -> Map("ab" -> 6, "hi" -> 1), "good" -> Map("wer" -> 3))
    assert(input11===output11)
  }
  test("merge3"){
    val input12=exer.merge3(Map("hi"->Map("ab"->Map("pq"->2))),Map("as"->Map("fg"->Map("hello"->6)),"hi"->Map("no"->Map("noo"->4))))
    val output12=Map("hi"->Map("no"->Map("noo"->4),"ab"->Map("pq"->2)),"as"->Map("fg"->Map("hello"->6)))
    assert(input12===output12)
  }
  test("wordcount"){
    val input13=exer.wordCount("hello how are you hello")
    val output13=List(("hello",2), ("are",1), ("how",1), ("you",1))
    assert(input13===output13)
  }
}
