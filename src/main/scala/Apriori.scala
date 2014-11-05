/**
 * Created by iurii.susuk on 05.11.2014.
 */
trait Item

class I1(id: String = "item 1") extends Item
class I2(id: String = "item 2") extends Item
class I3(id: String = "item 3") extends Item
class I4(id: String = "item 4") extends Item
class I5(id: String = "item 5") extends Item

class Apriori(transactions: Set[Set[Item]]) {

  def frequency(): Map[Item, Integer] = {
     null
  }
}
