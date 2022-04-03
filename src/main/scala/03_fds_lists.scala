import scala.annotation.tailrec
import java.{util => ju}
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def check(as: List[Int]): Int = 
    as match
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
  
  //TODO: Head of an emtry string      
  def head[A](as: List[A]): Option[A] =
    as match
      case Nil => None
      case Cons(x, xs) => Some[A](x)
  
  def tail[A](as: List[A]): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) => xs
    
  def setHead[A](as: List[A], newHead: A): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) => Cons(newHead, xs)

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    n match
      case 0 => as
      case x => drop(tail(as), x-1)

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) => if(f(x)) then dropWhile(xs, f) else as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    // a1 match
      // case Nil => a2
      // case Cons(h, t) => Cons(h, append(t, a2))
    
    foldLeft(reverse(a1), a2, (acc, elem) => Cons(elem, acc))
  
  def init[A](as: List[A]): List[A] =    
    as match
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
  
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))
  
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = 
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
  
  def length[A](as: List[A]): Int =
    // as match
      // case Nil => 0
      // case Cons(x, xs) => 1 + length(xs)
    foldRight(as, 0, (elem:A ,acc:Int) => (1 + acc))
  
  def lengthFL[A](as: List[A]): Int =
    foldLeft(as, 0, (acc:Int, elem:A) => (1 + acc))

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List(), (acc: List[A], elem: A) => Cons(elem, acc))
  
  def concatenate[A](as: List[List[A]]): List[A] =
    // foldLeft(as, List(), (acc: List[A], elem: List[A]) => append(acc, elem))
    foldRight(as, List(), (acc: List[A], elem: List[A]) => append(acc, elem))
  
  def addOne(as: List[Int]): List[Int] = 
    as match
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, addOne(xs))
  
  def doubleToString(as: List[Double]): List[String] = 
    as match
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))
  
  def map[A, B](as: List[A], f: A => B): List[B] = 
    as match
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs, f))
  
  def filter[A](as: List[A], f: A => Boolean): List[A] = 
    as match
      case Nil => Nil
      case Cons(x, xs) => if f(x) then Cons(x, filter(xs, f)) else filter(xs, f)
  
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = 
    as match
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs, f))
  
  def filterFlatMap[A](as: List[A], f: A => Boolean): List[A] = 
    flatMap(as, (elem: A) => if(f(elem)) then List(elem) else Nil)
    
  def operate[A,B,C](as: List[A], bs:List[B], operator: (A,B) => C): List[C] = 
    as match
      case Nil => Nil
      case Cons(h, xs) => Cons(operator(head(as).get, head(bs).get), operate(tail(as), tail(bs), operator))

  
  def hasPrefix[A](superset: List[A], prefix: List[A]): Boolean = 
    (superset, prefix) match            
      case(_, Nil) => true
      case(Nil, _) => false
      case(Cons(h1, t1), Cons(h2, t2)) =>         
        if(h1 != h2) then false else hasPrefix(t1, t2)

  def hasSubset[A](superset: List[A], subset: List[A]): Boolean =     
    (subset, superset) match
      case(Nil, _) => true
      case(_, Nil) => false
      case(Cons(h1, t1), Cons(h2, t2)) => if(hasPrefix(superset, subset)) then true else hasSubset(t2, subset)

def runCheck(): Unit = 
  println("Check")
  println(List.check(List(1,2,3,4,5)))
  println(List.check(List(1,2,4,5)))
  println(List.check(List(1,2,34,4,5)))

def runTail(): Unit = 
  println("tail")
  println(List.tail(List(1,2,3,4,5)))
  println(List.tail(List("foo", "bar", "baaz", "cheetah")))

def runSetHead(): Unit = 
  println("setHead")
  println(List.setHead(List(1,2,3,4,5), 3))
  println(List.setHead(List("foo", "bar", "baaz", "cheetah"), 3))

def runDrop(): Unit = 
  println("drop")
  println(List.drop(List(1,2,3,4,5), 3))
  println(List.drop(List("foo", "bar", "baaz", "cheetah"), 3))
  println(List.drop(List(), 3))

def runDropWhile(): Unit = 
  println("dropWhile")
  println(List.dropWhile(List(1,2,3,4,5), (_ % 4 != 0)))
  println(List.dropWhile(List("foo", "bar", "baaz", "cheetah"), (_.length > 1)))
  println(List.dropWhile(List[Int](), (_ % 4 != 0)))

def runAppend(): Unit = 
  println("append")
  println(List.append(List(1,2,3,4,5), List(11, 12)))
  println(List.append(List("foo", "bar", "baaz", "cheetah"), List()))
  println(List.append(List[Int](), List[String]()))

def runInit(): Unit = 
  println("init")
  println(List.init(List(1,2,3,4)))
  println(List.init(List()))

def runFoldRight(): Unit = 
  println("foldRight")
  println(List.foldRight(List(1,2,3,4), 0, _+_))
  println(List.foldRight(List(1, 2, 3), List[Int](): List[Int], List.Cons(_, _)))

def runLenght(): Unit = 
  println("length")
  println(List.length(List(1,2,3,4)))
  println(List.length(List()))
  println(List.lengthFL(List(1,2,3,4)))
  println(List.lengthFL(List()))

def runFoldLeft(): Unit = 
  println("foldLeft")
  println(List.foldLeft(List(1,2,3,4), 0, _+_))
  println(List.foldLeft(List(1,2,3,4), 1, _*_))  

def runReverse(): Unit = 
  println("reverse")
  println(List.reverse(List(1,2,3,4)))

def runConcatenate(): Unit = 
  println("concatenate")
  println(List.concatenate(List(List(1,2,3,4), List(11,12,13,34))))
  println(List.length(List.concatenate(List(List(1,2,3,4), List(11,12,13,34)))))
  println(List.concatenate(List(List(), List())))

def runMap(): Unit = 
  println("without map")
  println(List.addOne(List(1,2,3,4,5)))
  println(List.doubleToString(List(1,2,3,4,5)))

  println("with map")
  println(List.map(List(1,2,3,4,5), (_+1)))
  println(List.map(List[Double](1,2,3,4,5), _.toString()))

def runFilter(): Unit = 
  println("filter")
  println(List.filter(List(1,2,3,4,5,6,7), (_%2==0)))
  println(List.filter(List[Int](), (_%2==0)))
  println(List.filter(List[String]("foo", "bar", "baz", "cheetah"), (_.length>0)))

  println("filterFlatMap")
  println(List.filterFlatMap(List(1,2,3,4,5,6,7), (_%2==0)))
  println(List.filterFlatMap(List[Int](), (_%2==0)))
  println(List.filterFlatMap(List[String]("foo", "bar", "baz", "cheetah"), (_.length>0)))

def runFlatMap(): Unit = 
  println("flatMap")
  println(List.flatMap(List(1,2,3,4), i => List(i,i)))
  println(List.flatMap(List(), i => List(i,i)))

def runOperator(): Unit = 
  println("operator")
  println(List.operate(List(1,2,3,4), List(5,6,7,8), _+_))  
  println(List.operate(List("foo","bar","baaz","cheetah"), List(0,1,3,4), _.charAt(_)))  

def runHasSubset(): Unit = 
  println("hasSubset")
  println(List.hasSubset(List(1,2,3,4), List(1)))  
  println(List.hasSubset(List("foo","bar","baaz","cheetah"), List("foo","bar","baaz")))
  println(List.hasSubset(List("foo","bar","baaz","cheetah"), List("bar","baaz")))
  println(List.hasSubset(List("foo","bar","baaz","cheetah"), List("foo","baaz")))
  println(List.hasSubset(List("foo","bar","baaz","cheetah"), List()))
  println(List.hasSubset(List(), List("foo")))
  println(List.hasSubset(List(), List()))

@main def functional_data_structure: Unit = 
  // runCheck()
  // runTail()
  // runSetHead()
  // runDrop()
  // runDropWhile()
  // runAppend()
  // runInit()
  // runFoldRight()  
  // runFoldLeft()
  // runLenght()
  // runReverse()
  // runConcatenate()
  // runMap()
  // runFilter()
  // runFlatMap()
  // runOperator()
  runHasSubset()