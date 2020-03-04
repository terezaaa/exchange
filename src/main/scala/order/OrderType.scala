package order

sealed trait OrderType

case object BUY extends OrderType

case object SELL extends OrderType
