def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if ( a > b ) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}


def cube(x: Int): Int = x*x*x

sum((x:Int) => x, 0, 10)
sum(cube, 1,10)


// using high order function

def sum(f: Int => Int): (Int, Int) => Int = {

  def sumF(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumF(a+1, b)
  }

  sumF
}

val sumCube = sum(cube)
sum(x => x)(0,10)
sum(cube)(0,4)
sumCube(0,4)

def sum_2(f: Int => Int)(a: Int , b: Int): Int = {
  if ( a > b ) 0
  else f(a) + sum_2(f)(a+1, b)
}

val sumCube2 = sum_2(cube)_
sum_2(x => x)(0,10)
sum_2(cube)(0,4)
sumCube2(0, 10)

def product(f: Int=> Int)(a: Int, b: Int): Int = {
  if ( a > b ) 1 else f(a) * product(f)(a+1, b)
}

def factorial(x: Int): Int = product(x=>x)(1, x)

product(x => x)(1, 10)

factorial(1)
factorial(2)
factorial(3)
factorial(4)
factorial(5)

def perform(op: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a == b) b else op(f(a), perform(op)(f)(a+1, b))
}

perform((x,y) => x+y)(x => x)(1,10)
perform((x,y) => x*y)(x => x)(1,10)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a:Int, b:Int): Int = {
  if (a > b) unit
  else combine(f(a), mapReduce(f, combine, unit)(a+1, b))
}

mapReduce(x=>x, (x,y) => x+y, 0)(1,10)
mapReduce(x=>x, (x,y) => x*y, 1)(1,10)

