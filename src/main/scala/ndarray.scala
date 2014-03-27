package ndarray
import scala.collection.{mutable, LinearSeq}
import shapeless.ops.hlist.Length
import shapeless.{Generic, Nat, HList}

object NDFlags {
  def apply() = new NDFlags()

  def apply(hs: mutable.HashSet[String]) = new NDFlags()
}

class NDFlags(container: mutable.HashSet[String]) {
  def this() = this(new mutable.HashSet[String]())

  //contains
  def apply(f: NDFlag): Boolean = container.contains(f.str)

  def +=(f: NDFlag) = {
    container.add(f.str)
    this
  }

  //inplace
  def |(f: NDFlag) = this += f

  def +(f: NDFlag) = NDFlags(container.clone()) += f

  def +(fls: NDFlags) = NDFlags(container | fls.getContent)


  def -=(f: NDFlag) = {
    container.remove(f.str)
    this
  }

  def -(f: NDFlag) = NDFlags(container.clone()) -= f

  def -(fls: NDFlags) = NDFlags(container &~ fls.getContent)

  def getContent = container

  override def clone = NDFlags(container.clone())
}

trait NDFlag {
  def str: String
}

case object BORROWS_DATA extends NDFlag {
  def str: String = "bd"
}

case object ACCESS_MODIFIED extends NDFlag {
  def str: String = "am"
}


class NDShape(shape: LinearSeq[Int], var indexesMult: LinearSeq[Int], var accessFunction: LinearSeq[Int] => LinearSeq[Int],
              var margins: LinearSeq[Int], var flags: NDFlags) {
  def this(shape: LinearSeq[Int], indexesMult: LinearSeq[Int], accessFunction: LinearSeq[Int] => LinearSeq[Int]) = this(shape, indexesMult, accessFunction, shape.map(_ => 0), new NDFlags())

  /*
    checks whether all elements of l1 greater or equal of those in l2
  * */
  private def allGE(l1: LinearSeq[Int], l2: LinearSeq[Int]) = l1.zip(l2).forall( x => x._1 >= x._2)

  def apply(ind: LinearSeq[Int]): Int = {
    val indexes = accessFunction(ind)
    if (indexes.length != shape.length) throw new IndexOutOfBoundsException("Wrong dimensions number: " + indexes.length + " != " + shape.length)
    var sum = 0
    for (i <- 0 to indexes.length - 1) {
      if (ind(i) >= shape(i)) throw new IndexOutOfBoundsException(ind + " not in " + shape)
      sum += (indexes(i) + margins(i)) * indexesMult(i)
    }
    sum
  }

  def slice(left: LinearSeq[Int], right: LinearSeq[Int]): NDShape ={
    if( !allGE(shape,right) || !left.forall( _ >= 0) || !allGE(right,left))
      throw new IndexOutOfBoundsException("Could not prepare slice" + left + "  " + right + " for shape " + shape)
    new NDShape(right zip left map (x => x._1 - x._2), indexesMult, accessFunction, margins zip left map (x => x._1 + x._2), flags.clone | BORROWS_DATA)
  }

  def transpose(): NDShape =
    new NDShape(shape.reverse, indexesMult, x => accessFunction(x.reverse), margins, flags | BORROWS_DATA | ACCESS_MODIFIED)


  def ordering(): Iterator[Int] = elementsIndexes(mutable.MutableList(shape.map(x => 0): _*))


  def elementsIndexes(indexes: mutable.MutableList[Int]): Iterator[Int] = {
    val cur = this(indexes)
    val len = indexes.length
    for (i <- 1 to len) {
      if (indexes(len - i) + 1 < shape(len - i)) {
        indexes(len - i) = indexes(len - i) + 1
        lazy val v = Iterator.single(cur) ++ elementsIndexes(indexes)
        return v
      }
      else {
        indexes(len - i) = 0
      }
    }
    Iterator.single(cur)
  }

  def flaggedWith(f: NDFlag) = flags(f)

}


object NDArray {
  //constructor  from tuples or from HList. shapeless.Nat._ should be imported
  def apply[T, P <: Product, L <: HList, N <: Nat](data: Array[T], p: P)
                                                  (implicit gen: Generic.Aux[P, L],
                                                   len: Length.Aux[L, N],
                                                   ev: shapeless.ops.hlist.ToList[L, Int]) =
    new NDArray[T, N](data, gen.to(p).toList[Int](ev))

  def getIndexesMult(lst: LinearSeq[Int]): List[Int] = lst match {
    case Nil => Nil
    case x :: Nil => 1 :: Nil
    case x :: y =>
      val sz = getIndexesMult(y)
      y.head * sz.head :: sz
  }
}


class NDArray[T, N <: Nat](data: Array[T], var shape: NDShape) extends Iterable[T] {
  def apply(ind: LinearSeq[Int]) = data(shape(ind))

  def update(ind: LinearSeq[Int], v: T) = data(shape(ind)) = v

  def iterator = if(! flaggedWith(BORROWS_DATA)) data.iterator else shape.ordering() map (x => data(x))

  def slice(left: LinearSeq[Int], right: LinearSeq[Int]): NDArray[T, N] =
    new NDArray[T, N](data, shape.slice(left, right))

  def t(): NDArray[T, N] =
    new NDArray[T, N](data, shape.transpose())

  def flaggedWith(f:NDFlag) = shape.flaggedWith(f)

  def this(data: Array[T], lst: LinearSeq[Int]) =
    this(data, new NDShape(lst, NDArray.getIndexesMult(lst), x => x))

  def sh = shape
}