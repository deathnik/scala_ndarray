import ndarray.NDArray
import org.scalatest._
import shapeless._,Nat._


class NDShapeManipulationsTest extends FunSuite {
  val arr = (for (i <- 0 to 5) yield i).toArray

  test(" ndarray a == (a.t).t") {
    val nda1 = NDArray(arr, 1::2::3::HNil)
    val nda2 =  nda1.t.t
    assert(nda1.iterator zip nda2.iterator filter( x => x._1 != x._2)  isEmpty )
  }

  test("simple slice check"){
    val nda1 = NDArray(arr, 1::2::3::HNil)
    assert( nda1.slice(List(0,0,0),List(1,2,2)).iterator.toList.size  == 4)
  }
}
