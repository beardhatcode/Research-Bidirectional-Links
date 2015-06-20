import org.scalatest._

/**
 * Created by garonn on 20/06/15.
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
      b1.myAs.getOthers() should contain only aa
      b2.myAs.getOthers() should contain only aa

      b2.myAs -- aa
      aa.myBs.getOthers() should contain only b1
      b1.myAs.getOthers() should contain only aa
      b2.myAs.getOthers() should have size 0
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

