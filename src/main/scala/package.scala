import order.Order

import scala.collection.immutable.{SortedMap, SortedSet}

package object exchange {
  type OrderBook = SortedMap[Int, SortedSet[Order]]

  val orderBookDefault: OrderBook = SortedMap[Int, SortedSet[Order]]().withDefaultValue(SortedSet[Order]())
}