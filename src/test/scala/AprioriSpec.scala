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

  val candidates: Map[UniqueItem, Integer] = Map(new I1 -> 6)

  "Apriori" should "" in {
    val apriori = new Apriori(transactions)
    val candidates = apriori.findAndFilterCandidates()
    println(candidates)
  }
}
