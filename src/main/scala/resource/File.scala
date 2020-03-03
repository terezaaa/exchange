package resource

import java.io.PrintWriter

import client.Client
import mapper.DataMapper
import order.Order
import stock.{A, B, C, D}

import scala.io.Source

class File(private val dataMapper: DataMapper) {
  private val clientsPath = "clients.txt"
  private val ordersPath = "orders.txt"
  private val resultPath = "result.txt"

  def clients(): Map[String, Client] = dataMapper.clients(Source.fromResource(clientsPath).getLines().toList)

  def orders(clients: Map[String, Client]): List[Order] = dataMapper.orders(clients, Source.fromResource(ordersPath).getLines().toList)

  def writeClients(clients: Map[String, Client]): Unit = {

    new PrintWriter(resultPath) {
      write(clients.values.map(client => s"${client.name} ${client.balance} ${client.stocks(A)} ${client.stocks(B)} ${client.stocks(C)} ${client.stocks(D)}").mkString("\n"))
      close
    }
  }
}
