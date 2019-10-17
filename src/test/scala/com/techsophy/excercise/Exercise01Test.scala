package com.techsophy.excercise

import org.scalatest.FunSuite


class Exercise01Test extends FunSuite {

  val exercise = new Exercise01()
  val exercise2 = new Exercise02()

  test("Verify word count") {
    val wordCounts = exercise.wordCount("hello how are you hello")
    val expectedResult1 = Map("are" -> 1, "you" -> 1, "how" -> 1, "hello" -> 2)
    assert(wordCounts === expectedResult1)
  }
  test("Verify reverse list") {
    val reverse = exercise.reverse_foldLeft(List(1, 2, 3, 4))
    val expect1 = List(4, 3, 2, 1)
    assert(reverse === expect1)
  }

  test("Verify concatenate") {
    val concat = exercise.concatenate(Map("hello" -> 2, "hi" -> 3), Map("hello" -> 2, "wer" -> 1))
    val expect2 = Map("hello" -> 4, "hi" -> 3, "wer" -> 1)
    assert(concat === expect2)
  }
  test("Verify rev rec") {
    val rev = exercise.revRec(List(1, 2, 3, 4))
    val expect3 = List(4, 3, 2, 1)
    assert(rev === expect3)
  }
  test("Verify max list") {
    val max=exercise.maxList(List(-6,-2,-8,-1))
    val exp1 = -1
    assert(max===exp1)
  }


  test("show fun"){
    val lis=exercise.show(3,4)
    val rlis=List(1,1,1,2,2,2,3,3,3,4,4,4)
    assert(lis===rlis)
  }
  test("rotate array"){
    val rot=exercise.rotate(Array(1,2,3,4,5),2)
    val res=List(3,4,5,1,2)
    assert(rot===res)
  }

  test("sum function1"){
    val sum=exercise.sumOption(None,None)
    val res=None
    assert(sum===res)
  }

  test("sum function2"){
    val sum=exercise.sumOption(Some(1),None)
    val res=Some(1)
    assert(sum===res)
  }



  test("duplicates in list"){
    val lis=exercise.dup(List(1,2,3,1,2,4))
    val res=List(1,2,3,4)
    assert(lis===res)
  }

  test("Implicit square class"){
    val obj=new exercise.Square()
    val square=obj.sq(2)
    val res=4
    assert(square===res)
  }
  test("count files in directory"){
    val files=exercise.countFiles("home/sharanya/Downloads")
    val res=6
    assert(files===res)
  }
  test("Remove keys"){
    val keys=exercise.removeKeys(List("hello","are"),Map("hello" -> 1, "how" -> 2))
    val res=Map("how" -> 2)
    assert(keys==res)
  }
  test("concat list using foldLeft"){
    val lis=exercise.concatList(List(1,2,3),List(4,5,6))
    val res=List(1,2,3,4,5,6)
    assert(lis===res)
  }
  test("zip lists"){
    val lis=exercise.zip(List(1,2,3),List("hello","hi"))
    val res=List((1,"hello"), (2,"hi"))
    assert(lis===res)
  }
  test("sort list"){
    val lis=exercise.sort(List(1,5,2,6,3,4))
    val res=List(1, 2, 3, 4, 5, 6)
    assert(lis===res)
  }
  test("appraisal of employee"){
    val listOfEmployees = List(exercise.Employee(1,"abc@techsophy.com",exercise.Salary(2345,1256,1234),22),exercise.Employee(2,"pqr@techsophy.com",exercise.Salary(4567,1456,1564),56))
    val result = exercise.appraisal(listOfEmployees)
    val res=List(exercise.Employee(1,"abc@techsophy.com",exercise.Salary(2579.5,1256.0,1234.0),22), exercise.Employee(2,"pqr@techsophy.com",exercise.Salary(5023.7,145.6,1564.0),56))
    assert(result===res)
  }
  test("split by branch"){
    val st=List(exercise.Student(1,"abc",22,"cs"),exercise.Student(2,"pqr",22,"me"),exercise.Student(3,"avc",22,"cs"),exercise.Student(4,"klo",22,"me"),exercise.Student(5,"klo",22,"ee"),exercise.Student(6,"klo",22,"ce"))
    val split=exercise.spiltByBranch(st)
    val res=(List(exercise.Student(1,"abc",22,"cs"),exercise.Student(3,"avc",22,"cs")),List( exercise.Student(2,"pqr",22,"me"),exercise.Student(4,"klo",22,"me")),List(exercise.Student(5,"klo",22,"ee")),List(exercise.Student(6,"klo",22,"ce")))
    assert(split===res)
  }


}
