trait Ord[T]:
  def >=(x: T, y: T): Boolean =
    compare(x, y) >= 0
  def >(x: T, y: T): Boolean =
    compare(x, y) > 0
  def <=(x: T, y: T): Boolean =
    compare(x, y) <= 0
  def <(x: T, y: T): Boolean =
    compare(x, y) < 0
  def compare(x: T, y: T): Int

given Ord[Int] with
  def compare(x: Int, y: Int): Int =
    if x == y then
      0
    else
      (x - y) / Math.abs(x - y)

def isSortedWithDirection[T](list: List[T], direction: (T, T) => Boolean): Boolean =
  list.zip(list.tail).forall((x, y) => direction(x, y))

// Equivivalent to
// def isSorted[T](list: List[T])(using ord: Ord[T]): Boolean =
def isSorted[T: Ord](list: List[T]): Boolean =
  val ord = summon[Ord[T]]
  import ord._
  isSortedWithDirection(list, >=) || isSortedWithDirection(list, <=)

@main def contextBounds: Unit =
  List(
    List(1, 2, 3, 4, 5),
    List(3, 2, 1),
    List(1, 2, 1, 3, 4)
  ).foreach(values =>
    println(isSorted(values))
  )