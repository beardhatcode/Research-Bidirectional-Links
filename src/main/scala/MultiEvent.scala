/**
 * Created by garonn on 25/06/15.
 */
abstract class MultiEvent {
  type From
  type To
  val multi:Multi[From,To]
}

case class MultiAdditionEvent[FromEl,ToEl](multi :Multi[FromEl,ToEl],added:ToEl) extends MultiEvent {
  override type From = FromEl
  override type To = ToEl
}

case class MultiRemovalEvent[FromEl,ToEl](multi :Multi[FromEl,ToEl],removed:ToEl) extends MultiEvent {
  override type From = FromEl
  override type To = ToEl
}
