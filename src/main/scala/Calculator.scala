/**
 * Created by haroldportocarrero on 9/5/16.
 */

import scala.collection.mutable.Stack

object Calculator {

  def handleOperator(token: String, stack: Stack[Int]): Boolean = token match {
    case "+" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs + rhs)
      true
    case "-" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs - rhs)
      true
    case "*" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs * rhs)
      true
    case "/" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs / rhs)
      true
    case _ => false
  }

  def handleNumber(token: String, stack: Stack[Int]): Boolean = try {
    stack.push(token.toInt)
    true
  } catch {
    case _: NumberFormatException => false
  }

  def calculate(expression: String): Int = {
    val stack = new Stack[Int]
    //handle each token
    for(token <- expression.split(" "))
      if(!handleOperator(token, stack) && !handleNumber(token, stack))
        throw new IllegalArgumentException("invalid token: " + token)
    stack.pop()
  }

  def main(args: Array[String]) {
    if(args.length != 1){
      // expect exactly one argument
      throw  new IllegalArgumentException("Usage: Calculator <expression>")
    }else
      println(calculate(args(0)))
  }

}
