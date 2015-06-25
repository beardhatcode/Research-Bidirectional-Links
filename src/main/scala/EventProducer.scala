import scala.collection.mutable

/**
 * A trait for things that can send events and have listeners
 * @todo do in thread maybe?
 * @tparam T Type (as in class) of the event object that will be passed around (usecase: case class)
 */
trait EventProducer[T] {
  private val listeners = mutable.Set.empty[T => Unit]
  def addListener(eventListener: T => Unit) = {
    listeners += eventListener
  }
  def removeListener(eventListener: T => Unit) = {
    listeners -= eventListener
  }
  def sendEvent(event: T) = listeners.foreach(_(event))
}
