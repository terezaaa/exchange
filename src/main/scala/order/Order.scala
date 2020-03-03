package order

import stock.StockKind

case class Order(
                  id: Int,
                  orderType: OrderType,
                  stockKind: StockKind,
                  amount: Int,
                  price: Int,
                  client: String
                ) extends Ordered[Order] {
  override def compare(that: Order) = orderType match {
    case BUY => that.id.compareTo(id)
    case SELL => id.compareTo(that.id)
  }
}
