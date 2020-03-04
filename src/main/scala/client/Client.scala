package client

import stock.StockKind

case class Client(
                   name: String,
                   balance: Int,
                   stocks: Map[StockKind, Int]
                 ) {

  def updateBalance(value: Int): Client = this.copy(balance = balance + value)

  def buyStocks(stockKind: StockKind, amount: Int, price: Int): Client = this.copy(
    balance = balance - amount * price,
    stocks = stocks + (stockKind -> (stocks(stockKind) + amount))
  )

  def sellStocks(stockKind: StockKind, amount: Int, price: Int): Client = this.copy(
    balance = balance + amount * price,
    stocks = stocks + (stockKind -> (stocks(stockKind) - amount))
  )
}
