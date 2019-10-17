package com.techsophy.excercise

import org.scalatest.FunSuite

import scala.concurrent.Await
import scala.concurrent.duration._
class FutureExerciseTest extends FunSuite{
  val exercise=new FutureExercise()
  test("test future"){
    val input=Await.result(exercise.countFiles("/home/sharanya/Downloads/data"), 1 second)
    val output=Map("good" -> 4, "Scala" -> 1, "afternoon" -> 1, "hi" -> 1, "file" -> 1, "hello" -> 4, "morning" -> 2, "evening" -> 1)
    assert(input===output)
  }

}
