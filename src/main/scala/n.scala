import java.io.File

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

object n {
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
  def reduce(list: List[Map[String, Int]]): Future[Map[String, Int]] = {
    Future {
      list.foldLeft(Map[String, Int]()) { (acc, ele) => acc ++ ele.map { case (k, v) => k -> (v + acc.getOrElse(k, 0)) } }

    }
  }
  def countFiles(str: String):Future[Map[String,Int]]= {

    val folder: File = new File(str)

    def check(folder: List[File], res: List[File]): Future[List[File]] =

        if (folder.isEmpty) Future{res}
        else {
          if (folder.head.isDirectory) check(folder.head.listFiles.toList ::: folder.tail, res)
          else check(folder.tail, folder.head :: res)

        }


    val list_files: Future[List[File]] = Future{check(List(folder), Nil)}.flatten

    def help(list: List[File], res: List[String]): Future[List[String]] = {
      if (list.isEmpty) Future{res}
      else {
        val w: String = Source.fromFile(list.head).getLines.mkString
        help(list.tail, w :: res)
      }
    }

    val list_text: Future[List[String]] = list_files.map{ a=>help(a, Nil)}.flatten

    def help2(list: List[String], result: List[Map[String, Int]]): Future[List[Map[String, Int]]] = {
      if (list.isEmpty) Future{result}
      else help2(list.tail, wordCount(list.head) :: result)
    }

    val list_map: Future[List[Map[String, Int]]] = list_text.map{ a=>help2(a, Nil)}.flatten
    val list_result=list_map.map{a=>reduce(a)}.flatten
    list_result
  }
  def main(args: Array[String]): Unit = {
    //        println(countFiles("/home/sharanya/Downloads/data"))
    println(Await.result(countFiles("/home/sharanya/Downloads/data"),1 second))
    //    countFiles("/home/sharanya/Downloads/data").onComplete {
    //      case Success(value) => println(value)
    //      case Failure(e) => e.getMessage
    //    }

  }

}
