var items = List(new I1, new I2, new I3, new I4, new I5)

val transactions = List(
  List(new I1, new I2, new I5),
  List(new I2, new I4),
  List(new I2, new I3),
  List(new I1, new I2, new I4),
  List(new I1, new I3),
  List(new I2, new I3),
  List(new I1, new I3),
  List(new I1, new I2, new I3, new I5),
  List(new I1, new I2, new I3))

def join(items: List[UniqueItem]): List[(UniqueItem, UniqueItem)] = items match {
  case List() => Nil
  case x :: xs => xs.map(item => (x, item)) union join(xs)
}

var joined = join(items)
joined.size

val candidates = List((new I1, new I2), (new I1, new I3),
  (new I1, new I4), (new I1, new I5))
val c = for {
  transaction <- transactions
  candidate <- candidates
  if transaction.contains(candidate._1) && transaction.contains(candidate._2)
} yield candidate

var unique = Set(new I1, new I2, new I3, new I4, new I5)

unique union unique
