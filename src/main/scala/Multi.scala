/**
 * Created by garonn on 19/06/15.
 */
abstract class Multi[+From, +To] {
  def getOthers[T >: To]():Set[T]
  def getOwner[F >: From]:F
  def addOther(other:Any)
  protected def addToOthers[T >: Multi[To,From]](other:T)
  def removeOther(other:Any)
  protected def removeFromOthers[T >: Multi[To,From]](other:T)
}

object Multi{
  def make[From,To](owner:From,getMulti:To=>Multi[To,From]):Multi[From,To] = {
    var others:Set[Multi[To,From]] = Set();

    new Multi[From,To] {

      override def getOthers[T >: To](): Set[T] = others.map(_.getOwner)

      override def getOwner[F >: From]: F = owner.asInstanceOf[F]

      override def addOther(other: Any): Unit = {
        require(other.isInstanceOf[To])
        val multi: Multi[To,From] = getMulti(other.asInstanceOf[To])
        others = others + multi
        multi.addToOthers(this.getOwner)
      }

      override protected def addToOthers[T >: Multi[To, From]](other: T): Unit = {
        require(other.isInstanceOf[To])
        val multi: Multi[To,From] = getMulti(other.asInstanceOf[To])
        others = others + multi
      }

      override def removeOther(other: Any): Unit = {
        require(other.isInstanceOf[To])
        val multi: Multi[To, From] = getMulti(other.asInstanceOf[To])
        others = others - multi
        multi.removeFromOthers(this.getOwner)
      }

      override protected def removeFromOthers[T >: Multi[To, From]](other: T): Unit = {
        require(other.isInstanceOf[To])
        val multi: Multi[To, From] = getMulti(other.asInstanceOf[To])
        others = others - multi
      }
    }
  }
}