package com.techsophy.excercise
import java.io.File

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

class FutureExercise {
  def main(args: Array[String]): Unit = {
    //        println(countFiles("/home/sharanya/Downloads/data"))
    println(Await.result(countFiles("/home/sharanya/Downloads/data"), 1 second))
    //    countFiles("/home/sharanya/Downloads/data").onComplete {
    //      case Success(value) => println(value)
    //      case Failure(e) => e.getMessage
    //    }

  }

  def countFiles(str: String): Future[Map[String, Int]] = {

    val folder: File = new File(str)

    def listOfFiles(folder: List[File], result: List[File]): List[File]=

        folder match {
      case Nil =>result
      case head :: tail if head.isDirectory =>
        listOfFiles(head.listFiles.toList ::: tail, result)
      case head :: tail if head.isFile =>
        listOfFiles(tail, head :: result)

    }

    val listFiles: Future[List[File]] = Future{listOfFiles(List(folder), Nil)}

    def help(list: List[File], result: List[String]): List[String]= {
      if (list.isEmpty) result
      else {
        val w: String = Source.fromFile(list.head).getLines.mkString
        help(list.tail, w :: result)
      }
    }

    val listText: Future[List[String]] = Future{listFiles.map { a => help(a, Nil) }}.flatten

    def help2(list: List[String], result: List[Map[String, Int]]): List[Map[String, Int]] = {
      if (list.isEmpty) result
      else help2(list.tail, wordCount(list.head) :: result)
    }

    val listMap: Future[List[Map[String, Int]]] = Future{listText.map { a => help2(a, Nil) }}.flatten
    val listResult: Future[Map[String, Int]] = Future{listMap.map { a => reduce(a) }}.flatten
    listResult
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

  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
    def helpReduce(list: List[Map[String, Int]], result: Map[String, Int]): Map[String, Int]=
      list match {
        case Nil => result
        case head :: tail => helpReduce(tail, head.foldLeft(result) { (acc, element) => if (!acc.contains(element._1)) acc + element else acc + (element._1 -> (element._2 + acc.getOrElse(element._1, 0))) })
      }

    helpReduce(list, Map())
  }

}
