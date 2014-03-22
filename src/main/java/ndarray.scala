import scala.collection.{LinearSeq, mutable}
import shapeless.ops.hlist.Length
import shapeless.{Generic, Nat, HList}

class NDShape(shape: List[Int], var indexesMult: List[Int], var accessFunction: LinearSeq[Int] => LinearSeq[Int], var margins: List[Int]) {
  def this(shape: List[Int], indexesMult: List[Int], accessFunction: LinearSeq[Int] => LinearSeq[Int]) = this(shape, indexesMult, accessFunction, shape.map(_ => 0))

  def apply[T <: LinearSeq[Int]](indexes: T): Int = {
    if (shape.zip(indexes).exists(x => x._2 >= x._1) || indexes.length != shape.length) throw new ArrayIndexOutOfBoundsException
    accessFunction(indexes).zip(margins).zip(indexesMult).map(x => (x._1._1 + x._1._2) * x._2).sum
  }

  def slice(left: List[Int], right: List[Int]): NDShape =
    new NDShape(right zip left map (x => x._1 - x._2), indexesMult, accessFunction, margins zip left map (x => x._1 + x._2))

  def transpose(): NDShape =
    new NDShape(shape.reverse, indexesMult, x => accessFunction(x.reverse), margins)


  def ordering(): Iterator[Int] = elementsIndexes(mutable.MutableList(shape.map(x => 0): _*))


  def elementsIndexes(indexes: mutable.MutableList[Int]): Iterator[Int] = {
    val cur = this(indexes)
    val len = indexes.length
    val last = (1 to len).toStream.takeWhile(x => indexes(len - x) + 1 >= shape(len - x))
    last.foreach(x => indexes(len - x) = 0)
    if (last.length == len) Iterator.single(cur)
    else {
      indexes(len - last.length - 1) += 1
      lazy val v = Iterator.single(cur) ++ elementsIndexes(indexes)
      v
    }
  }
}

object NDArray {
  //constructor  from tuples or from HList
  def apply[T, P <: Product, L <: HList, N <: Nat](data: Array[T], p: P)
                                                  (implicit gen: Generic.Aux[P, L],
                                                   len: Length.Aux[L, N],
                                                   ev: shapeless.ops.hlist.ToList[L, Int])=
    new NDArray[T, N](data, gen.to(p).toList[Int](ev))


  def getIndexesMult(lst: List[Int]): List[Int] = lst match {
    case Nil => Nil
    case x :: Nil => 1 :: Nil
    case x :: y => {
      val sz = getIndexesMult(y)
      y.head * sz.head :: sz
    }
  }
}

class NDArray[T, N <: Nat](data: Array[T], var shape: NDShape) extends Iterable[T] {
  def apply(ind: LinearSeq[Int]) = data(shape(ind))

  def update(ind: LinearSeq[Int], v: T) = data(shape(ind)) = v

  def iterator = shape.ordering() map (x => data(x))

  def slice(left: List[Int], right: List[Int]): NDArray[T, N] =
    new NDArray[T, N](data, shape.slice(left, right))

  def t(): NDArray[T, N] =
    new NDArray[T, N](data, shape.transpose())


  def this(data: Array[T], lst: List[Int]) =
    this(data, new NDShape(lst, NDArray.getIndexesMult(lst), x => x))

}
