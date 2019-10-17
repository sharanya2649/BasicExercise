import java.io.File

import scala.io.Source

object futex {
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
  def countFiles(str: String):Map[String,Int]={
    val folder: File = new File(str)
    def check(folder:List[File],res:List[File]): List[File] = folder match {
      case Nil=>res
      case h :: t if h.isDirectory=>
        check(h.listFiles.toList:::t,res)
      case h :: t if h.isFile=>
        check(t,h::res)
    }
    val list_files=check(List(folder),Nil)

    def help(list:List[File],res:List[String]):List[String]={
      if(list.isEmpty) res
      else{
        val w: String =Source.fromFile(list.head).getLines.mkString

        help(list.tail,w::res)
      }
    }
    val list_text=help(list_files,Nil)
    def help2(list: List[String],result:List[Map[String,Int]]):List[Map[String,Int]]={
      if(list.isEmpty) result
      else help2(list.tail,wordCount(list.head)::result)
    }
    reduce(help2(list_text,Nil))

  }
//  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
//    val res=list.foldLeft(Map[String, Int]()) { (acc, ele) => acc ++ ele.map{case (k, v) => k -> (v + acc.getOrElse(k, 0))} }
//    res
//  }
  def reduce(list: List[Map[String, Int]]): Map[String, Int]= {
    def help(list: List[Map[String, Int]], result: Map[String, Int]): Map[String, Int] =
      list match {
        case Nil => result
        case head :: tail => help(tail, head.foldLeft(result) { (acc, ele) => if(!acc.contains(ele._1)) acc + ele else acc+(ele._1->(ele._2+acc.getOrElse(ele._1,0))) })
      }
    help(list, Map())
  }
  def main(args: Array[String]): Unit = {
    println(countFiles("/home/sharanya/Downloads/data"))
  }
}

//import java.io.File
//
//import scala.concurrent.{Await, Future}
//import scala.io.Source
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration._
//import scala.util._
//
//object FutureExercise {
//  def wordCount(text: String): Map[String, Int] = {
//    if (text.isEmpty) {
//      Map()
//    }
//    val words: Array[String] = text.split(" ")
//    val wordCounts: Map[String, Int] =
//      words
//        .groupBy(word => word)
//        .map { case (key, value) => (key, value.length) }
//    wordCounts
//  }
//  def reduce(list: List[Map[String, Int]]): Future[Map[String, Int]] = {
//    Future {
//      list.foldLeft(Map[String, Int]()) { (acc, ele) => acc ++ ele.map { case (k, v) => k -> (v + acc.getOrElse(k, 0)) } }
//
//    }
//  }
//  def countFiles(str: String):Future[Map[String,Int]]= {
//
//    val folder: File = new File(str)
//
//    def ListOfFiles(folder: List[File], res: List[File]): List[File] = folder match {
//      case Nil => res
//      case h :: t if h.isDirectory =>
//        ListOfFiles(h.listFiles.toList ::: t, res)
//      case h :: t if h.isFile =>
//        ListOfFiles(t, h :: res)
//    }
//
//    val list_files = ListOfFiles(List(folder), Nil)
//
//    def help(list: List[File], res: List[String]): List[String] = {
//      if (list.isEmpty) res
//      else {
//        val w: String = Source.fromFile(list.head).getLines.mkString
//
//        help(list.tail, w :: res)
//      }
//    }
//
//    val list_text = help(list_files, Nil)
//
//    def help2(list: List[String], result: List[Map[String, Int]]): List[Map[String, Int]] = {
//      if (list.isEmpty) result
//      else help2(list.tail, wordCount(list.head) :: result)
//    }
//
//    val list_map: Future[Map[String, Int]] = reduce(help2(list_text, Nil))
//    list_map
//  }
//  def main(args: Array[String]): Unit = {
//    //        println(countFiles("/home/sharanya/Downloads/data"))
//    //    println(Await.result(countFiles("/home/sharanya/Downloads/data"),1 second))
//    countFiles("/home/sharanya/Downloads/data").onComplete {
//      case Success(value) => println(value)
//      case Failure(e) => e.getMessage
//    }
//
//  }
//
//}

