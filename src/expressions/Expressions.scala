package expressions

object Expressions {
  def add_delimiter(expression: String, order: List[List[String]]): String = {
    var new_expression: String = expression
    new_expression = new_expression.replace(order.head.head,"_" + order.head.head + "_")
    new_expression = new_expression.replace(order.apply(1).head,"_" + order.apply(1).head + "_")
    new_expression = new_expression.replace(order.apply(1).apply(1),"_" + order.apply(1).apply(1) + "_")
    new_expression = new_expression.replace(order.apply(2).head,"_" + order.apply(2).head + "_")
    new_expression = new_expression.replace(order.apply(2).apply(1),"_" + order.apply(2).apply(1) + "_")
    new_expression = new_expression.replace(")","_" + ")" + "_")
    new_expression = new_expression.replace("(", "_" + "(" + "_")
    new_expression
  }

  def infix_to_postfix(expression: String, order: List[List[String]]): List[String] = {
    val new_expression: String = add_delimiter(expression, order)
    val each_token: Array[String] = new_expression.split("_")
    val precedence: Map[String, Int] = Map(
      order.head.head -> 2,
      order.apply(1).head -> 1,
      order.apply(1).apply(1) -> 1,
      order.apply(2).head -> 0,
      order.apply(2).apply(1) -> 0
    )
    var my_list: List[String] = List()
    for(index <- each_token.indices){
      my_list = my_list :+ each_token(index)
    }
    var stack: List[String] = List()
    var result: List[String] = List()
    for(index <- my_list.indices){
      if(my_list.apply(index) == "("){
        stack = my_list.apply(index) :: stack
      }
      else if(my_list.apply(index) == ")"){
        result = result :+ stack.head
        stack = stack.drop(1)
        if(stack.head == "("){
          stack = stack.drop(1)
        }
        else{
          result = result :+ stack.head
          stack = stack.drop(1)
          if(stack.head == "("){
            stack = stack.drop(1)
          }
          else{
            result = result :+ stack.head
            stack = stack.drop(1)
          }
        }
      }
      else if(my_list.apply(index) == ""){

      }
      else if(order.head.contains(my_list.apply(index)) || order.apply(1).contains(my_list.apply(index)) || order.apply(2).contains(my_list.apply(index))){
        if(stack.isEmpty){
          stack = stack :+ my_list.apply(index)
        }
        else{
          if(stack.head == "("){
            stack = my_list.apply(index) :: stack
          }
          else if(precedence(stack.head) > precedence(my_list.apply(index))) {
            result = result :+ stack.head
            stack = stack.drop(1)
            if (stack.isEmpty) {
              stack = stack :+ my_list.apply(index)
            }
            else{
              if(stack.head == "("){
                stack = my_list.apply(index) :: stack
              }
              else if(precedence(stack.head) > precedence(my_list.apply(index))) {
                result = result :+ stack.head
                stack = stack.drop(1)
                if (stack.isEmpty) {
                  stack = stack :+ my_list.apply(index)
                }
                else{
                  if(stack.head == "("){
                    stack = my_list.apply(index) :: stack
                  }
                  else if(precedence(stack.head) > precedence(my_list.apply(index))) {
                    result = result :+ stack.head
                    stack = stack.drop(1)
                    if (stack.isEmpty) {
                      stack = stack :+ my_list.apply(index)
                    }
                    else{
                      result = result :+ stack.head
                      stack = stack.drop(1)
                      stack = my_list.apply(index) :: stack
                    }
                  }
                  else if(precedence(stack.head) == precedence(my_list.apply(index))){
                    result = result :+ stack.head
                    stack = stack.drop(1)
                    stack = my_list.apply(index) :: stack
                  }
                  else{
                    stack = my_list.apply(index) :: stack
                  }
                }
              }
              else if(precedence(stack.head) == precedence(my_list.apply(index))){
                result = result :+ stack.head
                stack = stack.drop(1)
                stack = my_list.apply(index) :: stack
              }
              else{
                stack = my_list.apply(index) :: stack
              }
            }
          }
          else if(precedence(stack.head) == precedence(my_list.apply(index))){
            result = result :+ stack.head
            stack = stack.drop(1)
            stack = my_list.apply(index) :: stack
          }
          else{
            stack = my_list.apply(index) :: stack
          }
        }
      }
      else{
        result = result :+ my_list.apply(index)
      }
    }
    for(i <- stack.indices){
      result = result :+ stack.apply(i)
    }
    result
  }

  def evaluate[A](expression: String, f: String => A, operatorTable: Map[String, (A, A) => A], order: List[List[String]]): A = {
    val postfix: List[String] = infix_to_postfix(expression, order)
    var replaced: List[String] = List()
    for (a <- postfix.indices){
      val b = postfix.apply(a).split("\\s+").mkString
      replaced = replaced :+ b
    }
    var stack: List[String] = List()
    for (index <- replaced.indices){
      if(operatorTable.contains(replaced.apply(index))){
        if(replaced.apply(index) == order.head.head){
          if(index + 1 == replaced.length){
            val evaluated = operatorTable(replaced.apply(index))(f(stack.apply(stack.size - 2)), f(stack.last))
            stack = stack.dropRight(2)
            stack = stack :+ evaluated.toString
          }
          else {
            if (replaced.apply(index + 1) == order.head.head) {
              val evaluated = operatorTable(replaced.apply(index))(f(stack.apply(stack.size - 3)), f(stack.apply(stack.size - 2)))
              val stored: String = stack.last
              stack = stack.dropRight(3)
              stack = stack :+ evaluated.toString
              stack = stack :+ stored
            }
            else {
              val evaluated = operatorTable(replaced.apply(index))(f(stack.apply(stack.size - 2)), f(stack.last))
              stack = stack.dropRight(2)
              stack = stack :+ evaluated.toString
            }
          }
        }
        else {
          val evaluated = operatorTable(replaced.apply(index))(f(stack.apply(stack.size - 2)), f(stack.last))
          stack = stack.dropRight(2)
          stack = stack :+ evaluated.toString
        }
      }
      else if(replaced.apply(index) == ""){

      }
      else if(replaced.apply(index) == ")"){

      }
      else if(replaced.apply(index) == "("){

      }
      else{
        stack = stack :+ replaced.apply(index)
      }
    }
    val result: A = f(stack.head)
    result
  }

  def evaluateArithmetic(expression: String): Double = {
    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b

    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub
    )

    val order = List(List("^"), List("*", "/"), List("+", "-"))

    evaluate(expression, (s: String) => s.toDouble, operatorTable, order)
  }

  def evaluateBoolean(expression: String): Boolean = {
    val and = (a: Boolean, b: Boolean) => a && b
    val or = (a: Boolean, b: Boolean) => a || b
    val xor = (a: Boolean, b: Boolean) => (a || b) && !(a && b)
    val implies = (a: Boolean, b: Boolean) => !(a && !b)
    val iff = (a: Boolean, b: Boolean) => (a && b) || (!a && !b)

    val operatorTable: Map[String, (Boolean, Boolean) => Boolean] = Map(
      "&&" -> and,
      "||" -> or,
      "xor" -> xor,
      "->" -> implies,
      "<>" -> iff
    )

    val order = List(List("&&"), List("||", "xor"), List("->", "<>"))

    evaluate(expression, (s: String) => s.toBoolean, operatorTable, order)
  }

  def main(args: Array[String]): Unit = {
    println(evaluateArithmetic("4+5*(3+1-2)-20/2"))
    println(infix_to_postfix("4+5*(3+1-2)-20/2", List(List("^"), List("*", "/"), List("+", "-"))))
  }

}