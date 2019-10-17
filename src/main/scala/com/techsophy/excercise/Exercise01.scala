package com.techsophy.excercise

import java.io.File

import scala.annotation.tailrec

class Exercise01 {

  def revRec(lis: List[Int]): List[Int] = {
    @tailrec def help(result: List[Int], rest: List[Int]): List[Int] = {
      if (rest isEmpty)
        result
      else
        help(rest.head :: result, rest.tail)
    }
   help(Nil, lis)
  }

  def maxList(l: List[Int]): Int = {
    l.foldLeft(l(0)) { (acc, ele) => if (ele > acc) ele else acc }
  }

  def show(i: Int, n: Int): List[Int] = {
    val l = List.range(1, n + 1)
    l.flatMap(n => List.fill(i)(n))
  }

  def concatenate(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    //    val res2 = map1 ++ map2 //add two maps
    val res: Map[String, Int] = map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
    res

  }

  //  rotate array
  def rotate(a: Array[Int], r: Int): Array[Int] = {
//    val a = arr.toList.slice(0, n)
//
//    val b = arr.toList.slice(n, arr.length)
//    b ++ a

    val s1: (Array[Int], Array[Int]) =a.splitAt(r)
    s1._2++s1._1

  }

  //rev list using foldleft
  def reverse_foldLeft(lis: List[Int]): List[Int] = {
    lis.foldLeft(List[Int]()) { (r, h) => h :: r }
  }

  def wordCount(text: String): Map[String, Int] = {
    if (text.isEmpty) {
      Map()
    }
    val words: Array[String] = text.split(" ")
    val wordCounts: Map[String, Int] =
      words
        .groupBy(word => word)
        .map { case (key, value) => (key, value.length) }
    wordCounts
  }

  def sumOption(i: Option[Int], j: Option[Int]): Option[Int] = {

    val op = Option(i.getOrElse(0) + j.getOrElse(0))

    op match {
      case Some(0)=>None
      case Some(b)=>op

    }
  }

  def dup(lis: List[Int]): List[Int] = {
    lis.foldLeft(List[Int]()) {
      case (acc, ele) if acc.contains(ele) => acc
      case (acc, ele) => acc :+ ele
    }
  }

//  def countFiles(dir: String): Int = {
//    val folder = new File("/home/sharanya/Downloads")
//    if (folder.exists && folder.isDirectory) {
//    folder.listFiles.toList.size
//    } else {
//      0
//    }
//  }

  def countFiles(dir: String): Int = {
    val folder: File = new File("/home/sharanya/Downloads")
    def check(folder:List[File],res:List[File]): List[File] = folder match {
      case Nil=>res
      case h :: t if h.isDirectory=>
        check(h.listFiles.toList:::t,res)
      case h :: t if h.isFile=>
        check(t,h::res)


    }
    check(List(folder),Nil).length
  }

  def removeKeys(keys: List[String], map: Map[String, Int]): Map[String, Int] = {
    map.filter { case (key, value) => !keys.contains(key) }
  }

  def concatList(list1: List[Int], list2: List[Int]): List[Int] = {
    list2.foldLeft(list1) { (acc, ele) => acc :+ ele }
  }

  def zip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    val res: List[(Int, String)] = list1 zip list2
    //        val un:(List[Int],List[String])=res.unzip
    res
  }

  def sortonce(x: Int, list: List[Int]):List[Int]=list match {
    case Nil=>List(x)
    case h::t=>
      if(h>=x) x::list
      else h::sortonce(x,t)
  }
  def sort(l: List[Int]): List[Int] =l match {
    case Nil=>Nil
    case h::t=>sortonce(h,sort(t))

    //        l.sortWith((a,b)=>a<b)
    //    for (i <- 0 to l.length) {
    //      for (j <- i + 1 to l.length - 1) {
    //        if (l(i) > l(j)) {
    //          val temp = l(i)
    //          l(i) = l(j)
    //          l(j) = temp
    //        }
    //      }
    //    }
    //    l
  }

  def appraisal(emps: List[Employee]): List[Employee] = {
    emps.map { x =>
      val empSalary = x.salary
      val modifiedHra = if (x.age > 50) (empSalary.hra * 0.1) else empSalary.hra
      x.copy(salary = x.salary.copy(basic = empSalary.basic + (empSalary.basic * 0.1), hra = modifiedHra))
    }
  }

  def spiltByBranch(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    (list.filter(student => student.branch == "cs"),
      list.filter(student => student.branch == "me"),
      list.filter(student => student.branch == "ee"),
      list.filter(student => student.branch == "ce"))
  }

  case class Employee(id: Int, email: String, salary: Salary, age: Int)

  case class Salary(basic: Double, hra: Double, ta: Double)

  case class Student(id: Int, name: String, age: Int, branch: String)

  class Square {
    def sq(n: Int): Int = {
      val k = n * n
      k
    }
  }


}
