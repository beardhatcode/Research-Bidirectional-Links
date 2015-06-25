import org.scalatest._

/**
 * Test for Multi's and their events
 */
class MultiTest extends FlatSpec with Matchers{

  behavior of "Linking"

  it should "Link bidirectionaly" in {
    val aa = new ClassA
    val b1 = new ClassB
    val b2 = new ClassB

    aa.myBs ++ b1
    b2.myAs ++ aa

    aa.myBs.getOthers() should contain allOf(b1, b2)
    //Quik test of the foreach
    aa.myBs.foreach(_.myAs.getOthers() should contain only aa)
    b1.myAs.getOthers() should contain only aa
    b2.myAs.getOthers() should contain only aa

    b2.myAs -- aa
    aa.myBs.getOthers() should contain only b1
    b1.myAs.getOthers() should contain only aa
    b2.myAs.getOthers() should have size 0
  }

  it should "have a correct owner" in {
    val aa = new ClassA
    val bb = new ClassB
    val sa = new SwaggyA
    val sb = new SwaggyB
    aa.myBs.owner shouldBe theSameInstanceAs(aa)
    bb.myAs.owner shouldBe theSameInstanceAs(bb)
    sa.myBs.owner shouldBe theSameInstanceAs(sa)
    sb.myAs.owner shouldBe theSameInstanceAs(sb)
    sa.p.owner shouldBe theSameInstanceAs(sa)
    sb.p.owner shouldBe theSameInstanceAs(sb)

  }

  behavior of "Hierarchy"

  it should "typecheck" in {
    val sa = new SwaggyA
    "val a:Multi[ClassA,ClassB]  = Multi.make[SwaggyB,ClassB](new SwaggyB,_.myAs.filter)" shouldNot typeCheck
    "val a:Multi[ClassA,ClassB]  = Multi.make[ClassA,ClassB](new SwaggyA,_.myAs.filter)" should compile
  }

  it should "Allow other types" in {
    val aa = new ClassA
    val bb = new ClassB
    val sa = new SwaggyA
    val sb = new SwaggyB

    sa.p ++ sb
    sa.p ++ bb
    sa.p.getOthers() should contain only(bb,sb)
    sa.p -- bb
    sa.p.getOthers() should contain only(sb)
    sa.p -- sb
    sa.p.getOthers() should have size(0)

  }



  behavior of "events"
  it should "Send an event on one side" in {
    val aa = new ClassA
    val b1 = new ClassB
    val b2 = new ClassB
    var events = Seq.empty[(String,Multi[_,_],_)]
    aa.myBs.addListener {
      case MultiAdditionEvent(multi, element) => events = events :+ ("A add",multi,element)
      case MultiRemovalEvent(multi, element) => events = events :+ ("A remove",multi,element)
    }
    b1.myAs.addListener {
      case MultiAdditionEvent(multi, element) => events = events :+ ("B add",multi,element)
      case MultiRemovalEvent(multi, element) => events = events :+ ("B remove",multi,element)
    }
    aa.myBs ++ b1
    b1.myAs -- aa

    events should contain inOrderOnly(("A add",aa.myBs,b1),("B remove",b1.myAs,aa))

  }

}

class ClassA {
  val myBs:Multi[ClassA,ClassB]  = Multi.make[ClassA,ClassB](this,_.myAs)
}

class ClassB {
  val myAs:Multi[ClassB,ClassA]  = Multi.make[ClassB,ClassA](this,_.myBs)
}

class SwaggyA extends ClassA {
  val p:Multi[SwaggyA,ClassB]  = Multi.make[SwaggyA,ClassB](this,_.myAs.filter[ClassB, SwaggyA])
}

class SwaggyB extends ClassB {
  val p:Multi[SwaggyB,ClassA]  = Multi.make[SwaggyB,ClassA](this,_.myBs.filter[ClassA,SwaggyB])
}

