package exchange

import client.Client
import order.{BUY, Order, SELL}
import stock.StockKind

class Exchange(
                val clients: Map[String, Client],
                val buyOrders: Map[StockKind, OrderBook] = Map().withDefaultValue(orderBookDefault),
                val sellOrders: Map[StockKind, OrderBook] = Map().withDefaultValue(orderBookDefault)
              ) {

  private val rules = List(
    (order: Order) => order.orderType != BUY || (order.amount * order.price) <= clients(order.client).balance,
    (order: Order) => order.orderType != SELL || clients(order.client).stocks.getOrElse(order.stockKind, 0) >= order.amount
  )

  def order(order: Order): Exchange = {
    order match {
      case order if !rules.forall(_ (order)) => new Exchange(clients, buyOrders, sellOrders)
      case Order(_, _, _, 0, _, _) => new Exchange(clients, buyOrders, sellOrders)
      case _ =>
        nextMatch(order) match {
          case Some(m) =>
            val (orderRest, matchRest, clients) = deal(order, m)
            val (buyOrders, sellOrders) = order.orderType match {
              case BUY => (this.buyOrders, replace(m, matchRest))
              case SELL => (replace(m, matchRest), this.sellOrders)
            }

            (orderRest, matchRest) match {
              case (orderRest, matchRest) if orderRest == order && matchRest == m =>
                new Exchange(clients, buyOrders, sellOrders)
              case _ => new Exchange(clients, buyOrders, sellOrders).order(orderRest)
            }

          case None =>
            val (buyOrders, sellOrders) = order.orderType match {
              case BUY => (add(order), this.sellOrders)
              case SELL => (this.buyOrders, add(order))
            }
            new Exchange(clients, buyOrders, sellOrders)
        }
    }
  }

  private def replace(from: Order, to: Order): Map[StockKind, OrderBook] = to match {
    case Order(_, BUY, _, 0, _, _) =>
      buyOrders + (from.stockKind -> (buyOrders(from.stockKind) + (from.price -> (buyOrders(from.stockKind)(from.price) - from))))
    case Order(_, BUY, _, _, _, _) =>
      buyOrders + (from.stockKind -> (buyOrders(from.stockKind) + (from.price -> (buyOrders(from.stockKind)(from.price) - from + to))))
    case Order(_, SELL, _, 0, _, _) =>
      sellOrders + (from.stockKind -> (sellOrders(from.stockKind) + (from.price -> (sellOrders(from.stockKind)(from.price) - from))))
    case Order(_, SELL, _, _, _, _) =>
      sellOrders + (from.stockKind -> (sellOrders(from.stockKind) + (from.price -> (sellOrders(from.stockKind)(from.price) - from + to))))
  }

  private def add(order: Order): Map[StockKind, OrderBook] = order.orderType match {
    case BUY => buyOrders + (order.stockKind -> (buyOrders(order.stockKind) + (order.price -> (buyOrders(order.stockKind)(order.price) + order))))
    case SELL => sellOrders + (order.stockKind -> (sellOrders(order.stockKind) + (order.price -> (sellOrders(order.stockKind)(order.price) + order))))
  }

  private def deal(left: Order, right: Order): (Order, Order, Map[String, Client]) = {
    val price = left.orderType match {
      case BUY => left.price.min(right.price)
      case SELL => left.price.max(right.price)
    }

    val (buyer, seller) = left.orderType match {
      case BUY => (clients(left.client), clients(right.client))
      case SELL => (clients(right.client), clients(left.client))
    }

    val amount = left.amount
      .min(right.amount)
      .min(buyer.balance / price)
      .min(seller.stocks(left.stockKind))

    val (restLeftClient, restRightClient) = left.orderType match {
      case BUY =>
        (
          clients(left.client).buyStocks(left.stockKind, amount, price),
          clients(right.client).sellStocks(left.stockKind, amount, price)
        )
      case SELL =>
        (
          clients(left.client).sellStocks(left.stockKind, amount, price),
          clients(right.client).buyStocks(left.stockKind, amount, price)
        )
    }

    (
      left.copy(amount = left.amount - amount),
      right.copy(amount = right.amount - amount),
      clients
        + (restLeftClient.name -> restLeftClient)
        + (restRightClient.name -> restRightClient)
    )
  }

  private def nextMatch(order: Order): Option[Order] = order.orderType match {
    case BUY => sellOrders(order.stockKind).rangeTo(order.price).flatMap(_._2).headOption
    case SELL => buyOrders(order.stockKind).rangeFrom(order.price).flatMap(_._2).takeRight(1).headOption
  }
}
