package exchange

import mapper.DataMapper
import org.scalatest.FunSuite

class ExchangeTest extends FunSuite {
  test("Simple buy") {
    check(
      List("Bob 100 1 1 1 1", "Jack 100 1 1 1 1"),
      List("Bob s C 14 5"),
      List("Bob 100 1 1 1 1", "Jack 100 1 1 1 1"),
    )
  }

  test("Partial buy") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Bob b A 5 1", "Jack b A 5 1", "Jack b A 5 1", "Charlie s A 9 1"),
      List("Bob 95 15 1 1 1", "Jack 96 14 1 1 1", "Charlie 109 1 1 1 1"),
    )
  }

  test("Partial sell") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Bob s A 5 1", "Bob s A 5 1", "Jack s A 5 1", "Charlie b A 11 1"),
      List("Bob 110 0 1 1 1", "Jack 101 9 1 1 1", "Charlie 89 21 1 1 1"),
    )
  }

  test("Buy priority with the same price") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Bob b A 5 1", "Bob b A 5 1", "Jack b A 5 1", "Charlie s A 10 1"),
      List("Bob 90 20 1 1 1", "Jack 100 10 1 1 1", "Charlie 110 0 1 1 1"),
    )
  }

  test("Buy profitable rate") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Jack s A 10 2", "Charlie s A 5 1", "Bob b A 10 2"),
      List("Bob 85 20 1 1 1", "Jack 110 5 1 1 1", "Charlie 105 5 1 1 1"),
    )
  }

  test("Sell priority") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Bob b A 10 1", "Jack b A 5 2", "Charlie s A 10 1"),
      List("Bob 95 15 1 1 1", "Jack 90 15 1 1 1", "Charlie 115 0 1 1 1"),
    )
  }

  test("Available stocks limit") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 10 1 1 1"),
      List("Bob s A 10 1", "Bob s A 10 1", "Charlie b A 20 1"),
      List("Bob 110 0 1 1 1", "Jack 100 10 1 1 1", "Charlie 90 20 1 1 1"),
    )
  }

  test("Available balance limit") {
    check(
      List("Bob 100 10 1 1 1", "Jack 100 10 1 1 1", "Charlie 100 200 1 1 1"),
      List("Bob b A 100 1", "Bob b A 100 1", "Charlie s A 200 1"),
      List("Bob 0 110 1 1 1", "Jack 100 10 1 1 1", "Charlie 200 100 1 1 1"),
    )
  }

  def check(
             clientsActual: Iterable[String],
             orders: Iterable[String],
             clientsExpected: Iterable[String]
           ): Unit = {
    val dataMapper = new DataMapper
    val clients = dataMapper.clients(clientsActual)

    val clientsResult = dataMapper.orders(clients, orders).foldLeft(new Exchange(clients)) {
      (exchange, order) => exchange.order(order)
    }.clients

    assertResult(dataMapper.clients(clientsExpected))(clientsResult)
  }
}
