import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by iurii.susuk on 05.11.2014.
 */
class AprioriSpec extends FlatSpec with Matchers {
  val transactions = Set(Set(new I1, new I2))

  "Apriori" should "" in {
    val apriori = new Apriori(transactions)
    val candidates = apriori.frequency()

  }
}
