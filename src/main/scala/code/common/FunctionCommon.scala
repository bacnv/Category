package code.common

import java.util.UUID

/**
 * Created by phong on 7/16/2015.
 */
object FunctionCommon {
  def Insert[A](list : Map[String, Any], db : A)  : Boolean = {


    true
  }

  def main (args: Array[String]) {
    val a : String = UUID.randomUUID().toString
//    val b = (UUID)a

  println(a)
//    println(b)
  }

  class Stack[T] {
    var elems: List[T] = Nil
    def push(x: T) { elems = x :: elems }
    def top: T = elems.head
    def pop() { elems = elems.tail }
  }

  object GenericsTest extends App {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push('a')
    println(stack.top)
    stack.pop()
    println(stack.top)
  }
}
