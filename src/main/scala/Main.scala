import exchange.Exchange
import mapper.DataMapper
import resource.File

object Main extends App {

  val fileResource = new File(new DataMapper)
  val clients = fileResource.clients()
  val orders = fileResource.orders(clients)

  val result = orders.foldLeft(new Exchange(clients)) {
    (exchange, order) => exchange.order(order)
  }

  fileResource.writeClients(result.clients)
}