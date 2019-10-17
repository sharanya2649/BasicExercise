package com.techsophy.excercise

import scala.annotation.tailrec


sealed trait Expr

case class Number(i: Int) extends Expr

case class Sum(expr1: Expr, expr2: Expr) extends Expr

case class Subtract(expr1: Expr, expr2: Expr) extends Expr

case class Multiply(expr1: Expr, expr2: Expr) extends Expr

class Exercise02 {
  //tail recursion list
  def reverse2(list: List[Int]): List[Int] = {
    @tailrec def reverseTail(result: List[Int], rest: List[Int]): List[Int] = {
      if (rest isEmpty)
        result
      else
        reverseTail(rest.head :: result, rest.tail)
    }
    reverseTail(Nil, list)
  }

  //reverse using recursion
  def reverse3(list: List[Int]): List[Int] = {
    if (list isEmpty)
      Nil
    else
      reverse3(list.tail) :+ list.head
  }
  def eval(expr: Expr): Int = expr match {
    case Number(i) => i
    case Sum(expr1, expr2) => eval(expr1)+eval(expr2)
    case Subtract(expr1, expr2) => eval(expr1)-eval(expr2)
    case Multiply(expr1, expr2) => eval(expr1)*eval(expr2)

  }

  def reverse_fold(ls: List[Int]): List[Int] =
    ls.foldLeft(List[Int]()) { (r, h) => h :: r }

  def window(length:Int, list:List[Int]):List[List[Int]]={
    if (list.isEmpty) Nil
    else (list take length) :: window(length, list drop length)
  }

  def wordCount(text: String): List[(String, Int)] = {
    val words = text.split(" ")
    val wCount = words.groupBy(w => w).map { case (k, v) => (k, v.length) }
    wCount.toList.sortWith((k,v)=> k._2>v._2||k._2==v._2&&k._1<v._1)
  }

  def myZip(list1:List[Int],list2:List[String]):List[(Int,String)]={
    def my(list1: List[Int], list2: List[String], new_list: List[(Int, String)]): List[(Int, String)] = {
      if (list1.isEmpty) new_list
      else my(list1.tail, list2.tail, new_list :+ (list1.head, list2.head))
    }
    my(list1, list2, List[(Int, String)]())
  }

  def fuse[A,B](a:Option[A], b:Option[B]):Option[(A,B)]={
    a.flatMap(x=>b.map(y=>(x,y)))

  }

//  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
//    val res=list.foldLeft(Map[String, Int]()) { (acc, ele) => acc ++ ele.map{case (k, v) => k -> (v + acc.getOrElse(k, 0))} }
//    res
//  }
  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
    def helpReduce(list: List[Map[String, Int]], result: Map[String, Int]): Map[String, Int]=
      list match {
        case Nil => result
        case head :: tail => helpReduce(tail, head.foldLeft(result) { (acc, element) => if (!acc.contains(element._1)) acc + element else acc + (element._1 -> (element._2 + acc.getOrElse(element._1, 0))) })
      }

    helpReduce(list, Map())
  }

  def reduce2(list: List[Map[String, Map[String, Int]]]): Map[String, Map[String, Int]] = {
    list.foldLeft(Map[String, Map[String, Int]]()) { (acc, ele) => acc ++ ele.map{case (k, v) => k -> (v ++ acc.getOrElse(k, Map()))} }
  }


  def merge(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
  }

  def merge2(map1: Map[String, Map[String, Int]], map2: Map[String, Map[String, Int]]):
  Map[String, Map[String, Int]] = {
    map1 ++ map2.map { case (k, v) => k -> merge(v , map1.getOrElse(k, Map())) }
  }

  def merge3(map1: Map[String, Map[String, Map[String, Int]]], map2: Map[String,
    Map[String, Map[String, Int]]]): Map[String, Map[String, Map[String, Int]]] = {
    map1 ++ map2.map { case (k, v) => k -> merge2(v , map1.getOrElse(k, Map())) }
  }

}
