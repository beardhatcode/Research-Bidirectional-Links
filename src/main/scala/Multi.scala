/**
 * Abstract base class of a multi that represents bidirectional links
 *
 * Creates bidirectional link between elements that can be released. Events will be fired on
 * creation of link on the multi to whom a method call was invoked
 *
 * @tparam From the type of the owner
 * @tparam To   the type of the targets
 * @author Robbert Gurdeep Singh (@beardhatcode)
 */
abstract class Multi[+From, +To] extends EventProducer[MultiEvent] with Traversable[To]{
  /**
   * Get all elments that are linked
   * @tparam T Type of the set to return
   * @return set of bi-directionaly linked elements
   */
  def getOthers[T >: To]():Set[T]

  /**
   * Returns the owner of this multi
   * @tparam F  type oto cast te owner to
   * @return the owner of the multi
   */
  def owner[F >: From]:F

  /**
   * Make a bidirectional link with this element. That is add and element
   * Note: Type will be checked @Runtime !
   * @param other the other element
   */
  def ++(other:Any)

  /**
   * Internal method to only add to this multi without looking to the other side
   * of te relation
   * @param other the multi to add
   * @tparam T  type of the multi
   */
  protected def addToOthers[T >: Multi[To,From]](other:T)

  /**
   * Remove the link with an elemnt
   * @param other the element to unlink
   */
  def --(other:Any)

  /**
   * Internal method to remove a multi only on this side
   * @param other
   * @tparam T
   */
  protected def removeFromOthers[T >: Multi[To,From]](other:T)

  /**
   * Create a new multi that is in sync with the current multi but is of a lower type
   *
   * This can be used to make a Multi[A,C] form a Multi[A,B] if C<:B
   *
   * Note: a filtered multi is .equal() with it's origin
   * Note: if you choose types that do not fit you will get nothing because nothing will match
   * your filer.
   * @tparam NewFrom  new from type
   * @tparam NewTo    new to type
   * @return a new multi that contains only the filtered elments
   */
  def filter[NewFrom,NewTo]: Multi[NewFrom,NewTo]

  /**
   * Place to store the base instance of a multi, used to know the source multi when filtering
   */
  protected val baseInstance:Multi[Any,Any]

  /**
   * Overide equals
   *
   * 2 multi's are equal if the origionated from the same Multi
   * that is the ref of the {@see baseInstance} is checked.
   *
   * @param o the element to compare with
   * @return true if the supplied element is the same or a filtered multi from this
   */
  override def equals(o: scala.Any): Boolean = {
    o match {
      case multi:Multi[Any,Any] => this.baseInstance.eq(multi.baseInstance)
      case _ => false
    }
  }

  override def foreach[U](f: (To) => U): Unit = getOthers().foreach(f)
}

/**
 * Factory object for Multi's
 */
object Multi{
  /**
   * Make a multi. That creates bi directional links by using getMulti to fetch
   * the other multi.
   *
   * @param _owner     the owner off the multi
   * @param getMulti  a function that get's the multi of the other side
   * @tparam From     Type of owner
   * @tparam To       Type of targets
   * @return  a new multi
   */
  def make[From,To](_owner:From,getMulti:To=>Multi[To,From]):Multi[From,To] = {
    var others:Set[Multi[To,From]] = Set();

    new Multi[From,To] {

      override def getOthers[T >: To](): Set[T] = others.map(_.owner)

      override def owner[F >: From]: F = _owner.asInstanceOf[F]

      override def ++(other: Any): Unit = {
        val otherElement: To = other.asInstanceOf[To]
        val multi: Multi[To,From] = getMulti(otherElement)
        others = others + multi
        multi.addToOthers(this.owner)
        sendEvent(new MultiAdditionEvent[From,To](this,otherElement))
      }

      override protected def addToOthers[T >: Multi[To, From]](other: T): Unit = {
        val multi: Multi[To,From] = getMulti(other.asInstanceOf[To])
        others = others + multi
      }

      override def --(other: Any): Unit = {
        val otherElement: To = other.asInstanceOf[To]
        val multi: Multi[To, From] = getMulti(otherElement)
        others = others - multi
        multi.removeFromOthers(this.owner)
        sendEvent(new MultiRemovalEvent[From,To](this,otherElement))
      }

      override protected def removeFromOthers[T >: Multi[To, From]](other: T): Unit = {
        val multi: Multi[To, From] = getMulti(other.asInstanceOf[To])
        others = others - multi
      }

      override protected val baseInstance: Multi[Any, Any] = this

      /**
       * Filtering is implemented by making a new multi that delegates all calls to it's origin
       * and filters out things of incompatible type when getting values
       *
       * @tparam NewFrom  new from type
       * @tparam NewTo    new to type
       * @return a new multi that contains only the filtered elments
       */
      override def filter[NewFrom, NewTo]: Multi[NewFrom, NewTo] = {
        val realMulti = this
        new Multi[NewFrom,NewTo] {
          override def getOthers[T >: NewTo](): Set[T] = this.getOthers().filter(_.isInstanceOf[NewTo]).asInstanceOf

          override def filter[NewFrom, NewTo]: Multi[NewFrom, NewTo] = realMulti.filter[NewFrom,NewTo]

          override def ++(other: Any): Unit = realMulti.++(other)

          override protected def removeFromOthers[T >: Multi[NewTo, NewFrom]](other: T): Unit = realMulti.removeFromOthers(other)

          override def --(other: Any): Unit = realMulti.--(other)

          override def owner[F >: NewFrom]: F = _owner.asInstanceOf[F]

          override protected def addToOthers[T >: Multi[NewTo, NewFrom]](other: T): Unit = realMulti.addToOthers(other)

          override protected val baseInstance: Multi[Any,Any] = realMulti
        }
      }

    }
  }
}