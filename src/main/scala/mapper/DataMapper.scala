package mapper

import client.Client
import order.{BUY, Order, SELL}
import stock.{A, B, C, D}

import scala.collection.mutable

class DataMapper {
  private val clientsPattern = """^(\w+)[\t| ](\d+)[\t| ](\d+)[\t| ](\d+)[\t| ](\d+)[\t| ](\d+)$""".r
  private val ordersPattern = """^(\w+)[\t| ](\w)[\t| ](\w+)[\t| ](\d+)[\t| ](\d+)$""".r

  def clients(clients: Iterable[String]): Map[String, Client] = {
    clients.map {
      case clientsPattern(name, balance, amountA, amountB, amountC, amountD) =>
        Client(
          name,
          balance.toInt,
          Map(A -> amountA.toInt, B -> amountB.toInt, C -> amountC.toInt, D -> amountD.toInt)
        )
    }
    }.map(client => client.name -> client).toMap

  def orders(clients: Map[String, Client], orders: Iterable[String]): List[Order] = {
    val seq = mutable.Queue.from(1 to orders.size)
    orders.map {
      case ordersPattern(client, orderType, stockKind, amount, price) =>
        Order(
          seq.dequeue(),
          orderType match {
            case "b" => BUY
            case "s" => SELL
          },
          stockKind match {
            case "A" => A
            case "B" => B
            case "C" => C
            case "D" => D
          },
          amount.toInt,
          price.toInt,
          clients.get(client).get.name
        )
    }.toList
  }
}
