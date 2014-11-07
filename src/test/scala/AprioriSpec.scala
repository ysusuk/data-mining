import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by iurii.susuk on 05.11.2014.
 */
class AprioriSpec extends FlatSpec with Matchers {
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

  "Apriori" should "filter items with min support count" in {
    val apriori = new Apriori(transactions)
    val candidates = apriori.countAndFilter1ItemCandidates()
    candidates get (new I1) should be(Some(6))
    candidates get (new I2) should be(Some(7))
    candidates get (new I3) should be(Some(6))
    candidates get (new I4) should be(Some(2))
    candidates get (new I5) should be(Some(2))
  }

  it should "" in {
    val apriori = new Apriori(transactions)
    val items: List[UniqueItem] = apriori.countAndFilter1ItemCandidates().keys.toList
    println(items)
    val candidates = apriori.generateCountAndFilter2ItemCandidates(items)
    println(candidates)
    candidates get (new I1, new I2) should be(Some(4))
    candidates get (new I1, new I3) should be(Some(4))
    candidates get (new I1, new I5) should be(Some(2))
    candidates get (new I2, new I3) should be(Some(4))
    candidates get (new I2, new I4) should be(Some(2))
    candidates get (new I2, new I5) should be(Some(2))
  }
}
