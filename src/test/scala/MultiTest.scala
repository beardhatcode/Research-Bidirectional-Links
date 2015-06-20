import org.scalatest._

/**
 * Created by garonn on 20/06/15.
 */
class MultiTest extends FlatSpec with Matchers{

  behavior of "Making links"

  it should "Link bidirectionaly" in {
    val aa=new ClassA
    val b1=new ClassB
    val b2=new ClassB

    aa.myBs.addOther(b1)
    b2.myAs.addOther(aa)

    aa.myBs.getOthers() should contain allOf (b1,b2)
    b1.myAs.getOthers() should contain only aa
    b2.myAs.getOthers() should contain only aa

    b2.myAs.removeOther(aa)
    aa.myBs.getOthers() should contain only b1
    b1.myAs.getOthers() should contain only aa
    b2.myAs.getOthers() should have size 0
  }

  it should "have correct hierarchy" in {
  }
}


class ClassA {
  val myBs:Multi[ClassA,ClassB]  = Multi.make[ClassA,ClassB](this,_.getMulti)
  def getMulti = myBs
}

class ClassB {
  val myAs:Multi[ClassB,ClassA]  = Multi.make[ClassB,ClassA](this,_.getMulti)
  def getMulti = myAs
}
/*
class SwaggyA extends ClassA {
  override val myBs:Multi[ClassA,ClassB]  = Multi.make[SwaggyA,ClassB](this,_.getMulti)
}

class SwaggyB extends ClassB {
  val p:Multi[ClassB,ClassA]  = Multi.make[SwaggyB,ClassA](this,_.getMulti)
  override def getMulti = p
}

*/