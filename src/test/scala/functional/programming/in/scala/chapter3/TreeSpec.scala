package functional.programming.in.scala.chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers{

  val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3),Leaf(1)))

  "size" should "cal total leafs of a tree correctly" in {
    Tree.size(t) should be(7)
    Tree.sizeViaFold(t) should be(7)
  }

  "maximum" should "cal max leaf value of a tree correctly" in {
    Tree.maximum(t) should be(3)
    Tree.maximumViaFold(t) should be(3)
  }

  "depth" should "cal depth of a tree correctly" in {
    Tree.depth(t) should be(2)
    Tree.depthViaFold(t) should be(2)
  }

}
