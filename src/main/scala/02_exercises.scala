@main def main: Unit =     
  println(fib(8))
  println(isSorted(Array(1,2,2,3,4), _ > _)) 

def fib(n:Int) =
  def go(n: Int, curr: Int, next: Int): Int =
    if n == 0 then curr else go(n-1, next, curr + next)
  go(n, 0, 1)    

def isSorted[A](as: Array[A], isGreater: (A, A) => Boolean): Boolean =
  def check(index: Int): Boolean = 
    if(index+1 >= as.length) then true
    else if isGreater(as(index), as(index+1)) then false
    else check(index+1)
  check(0)

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => ((b:B) => f(a,b))

def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
