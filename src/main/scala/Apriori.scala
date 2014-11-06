/**
 * Created by iurii.susuk on 05.11.2014.
 */

abstract class UniqueItem(val id: String) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[UniqueItem]

  override def equals(other: Any): Boolean = other match {
    case that: UniqueItem =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = {
    id
  }

}

class I1(override val id: String = "item 1") extends UniqueItem(id)

class I2(override val id: String = "item 2") extends UniqueItem(id)

class I3(override val id: String = "item 3") extends UniqueItem(id)

class I4(override val id: String = "item 4") extends UniqueItem(id)

class I5(override val id: String = "item 5") extends UniqueItem(id)

class Apriori(val transactions: List[List[UniqueItem]], val minSupportCount: Int = 2) {

  def filter1ItemCandidates(): Map[UniqueItem, Int] = {
    transactions.flatten groupBy (i => i) mapValues (_.size) filter (_._2 >= minSupportCount)
  }

  def generateAndFilter2ItemCandidates(items: Set[UniqueItem]): Map[Set[UniqueItem], Int] = {
    Map(Set(new I1, new I2) -> 0)
  }
}
