package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert (e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll{ (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val min = if (a <= b) a else b
    findMin(h2) == min
  }


  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll{ a: A =>
    val h1 = insert(a, empty)
    isEmpty(h1) == false
    val h2 = deleteMin(h1)
    isEmpty(h2) == true
  }
  def getHeapList(h: H, acc: Seq[A]) : Seq[A] = if (isEmpty(h)) acc
  else {
    val min = findMin(h)
    val newHeap = deleteMin(h)
    getHeapList(newHeap, acc ++ Seq(min))
  }
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. ") = forAll{  (h: H) =>
    val lFromHeap = getHeapList(h, Nil)
    lFromHeap == lFromHeap.sorted
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll{  (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val prevMin = if (min1<= min2) min1 else min2
    val newHeap = meld(h1, h2)
    val newHeapMin = findMin(newHeap)
    prevMin == newHeapMin
  }
  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(empty, h) == h
    }

  property("melding 3 times and deleting 3 mins, next min are equal") =
    forAll { (h: H) =>
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      isEmpty(h2) || findMin(h1) == findMin(h2)
    }
}
