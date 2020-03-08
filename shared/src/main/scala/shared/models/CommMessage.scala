package shared.models

import java.nio.ByteBuffer

import boopickle.Default._

sealed trait CommMessage {
  final def encode: ByteBuffer = CommMessage.encode(this)
}

final case class WelcomeMessage(id: String) extends CommMessage

final case class MovementCommand(id: Int, movement: Movement) extends CommMessage

sealed trait Movement

object Movement {
  case object Fire extends Movement

  case object Up extends Movement

  case object Down extends Movement

  case object Right extends Movement

  case object Left extends Movement

  def fromDirection(direction: Direction): Movement = direction match {
    case Direction.UP    => Up
    case Direction.DOWN  => Down
    case Direction.LEFT  => Left
    case Direction.RIGHT => Right
  }

  def fromNumber(n: Int): Movement = n match {
    case 0 => Up
    case 1 => Down
    case 2 => Right
    case 3 => Left
    case _ => Fire
  }
}

object CommMessage {

  private def encode(msg: CommMessage): ByteBuffer = Pickle.intoBytes(msg)

  def decode(bb: ByteBuffer): Either[Exception, CommMessage] =
    try Right(Unpickle[CommMessage].fromBytes(bb))
    catch {
      case ex: Exception => Left(ex)
    }

}
