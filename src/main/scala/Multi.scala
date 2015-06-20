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
  def filter[NewFrom,NewTo]: Multi[NewFrom,NewTo]
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

      override def filter[NewFrom, NewTo]: Multi[NewFrom, NewTo] = {
        val realMulti = this
        new Multi[NewFrom,NewTo] {
          override def getOthers[T >: NewTo](): Set[T] = this.getOthers().filter(_.isInstanceOf[NewTo]).asInstanceOf

          override def filter[NewFrom, NewTo]: Multi[NewFrom, NewTo] = realMulti.filter[NewFrom,NewTo]

          override def addOther(other: Any): Unit = realMulti.addOther(other)

          override protected def removeFromOthers[T >: Multi[NewTo, NewFrom]](other: T): Unit = realMulti.removeFromOthers(other)

          override def removeOther(other: Any): Unit = realMulti.removeOther(other)

          override def getOwner[F >: NewFrom]: F = owner.asInstanceOf[F]

          override protected def addToOthers[T >: Multi[NewTo, NewFrom]](other: T): Unit = realMulti.addToOthers(other)
        }
      }
    }
  }
}