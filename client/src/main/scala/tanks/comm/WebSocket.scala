package tanks.comm
import monix.eval.Task
import org.scalajs.dom
import shared.models.{CommMessage, GameState}

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

final class WebSocket private (socket: dom.WebSocket) {

  println(socket.binaryType)

  def open(): Task[Unit] =
    Task.async { cb =>
      socket.onopen = _ => cb.onSuccess(())
    }

  def send(msg: CommMessage): Task[Unit] = Task.eval {
    socket.send(msg.encode.arrayBuffer())
  }

  def doOnMessage[A](f: GameState => Task[A]): Task[Unit] = Task.deferAction { implicit s =>
    Task {
      socket.onmessage = (e: dom.MessageEvent) => {
        println(e.data)
        GameState
          .decode(TypedArrayBuffer.wrap(e.data.asInstanceOf[ArrayBuffer]))
          .fold(
            err => println(s"Could not decode ${err}"),
            msg => {
              f(msg).runToFuture
            }
          )
      }
    }
  }
}

object WebSocket {

  def apply(url: String): Task[WebSocket] = Task.eval {
    val ws = new dom.WebSocket(url)
    ws.binaryType = "arraybuffer"
    new WebSocket(ws)
  }
}
