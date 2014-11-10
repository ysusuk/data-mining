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
    val candidates = apriori.countAndFilter1CandidateItemSets()
    candidates get (new I1) should be(Some(6))
    candidates get (new I2) should be(Some(7))
    candidates get (new I3) should be(Some(6))
    candidates get (new I4) should be(Some(2))
    candidates get (new I5) should be(Some(2))
  }

  it should "filter 2-items with min support count" in {
    val apriori = new Apriori(transactions)
    val frequentItemSets1: List[UniqueItem] = apriori.countAndFilter1CandidateItemSets().keys.toList.sortWith((item1, item2) => item1.toString.compareTo(item2.toString) < 0)
    val frequentItemSets2 = apriori.generateCountAndFilter2CandidateItemSets(frequentItemSets1.map(Set(_)))
    frequentItemSets2 get Set(new I1, new I2) should be(Some(4))
    frequentItemSets2 get Set(new I1, new I3) should be(Some(4))
    frequentItemSets2 get Set(new I1, new I5) should be(Some(2))
    frequentItemSets2 get Set(new I2, new I3) should be(Some(4))
    frequentItemSets2 get Set(new I2, new I4) should be(Some(2))
    frequentItemSets2 get Set(new I2, new I5) should be(Some(2))
  }

  it should "filter 3-items with min support count" in {
    val apriori = new Apriori(transactions)
    val frequentItemSets1: List[UniqueItem] = apriori.countAndFilter1CandidateItemSets().keys.toList.sortWith((item1, item2) => item1.toString.compareTo(item2.toString) < 0)
    val frequentItemSets2 = apriori.generateCountAndFilter2CandidateItemSets(frequentItemSets1.map(Set(_)))
    val frequentItemSets4 = apriori.generateCountAndFilter3CandidateItemSets(frequentItemSets2.keys.toList)
    println(frequentItemSets4)
    frequentItemSets4 get Set(new I1, new I2, new I3) should be(Some(2))
    frequentItemSets4 get Set(new I1, new I2, new I5) should be(Some(4))
  }
}
