package com.github.vic.dll

sealed trait DoubleLinked[+T]:
  def value: Option[T]

  def prev: Option[DoubleLinked[T]]
  def next: Option[DoubleLinked[T]]

  def first: Option[DoubleLinked[T]]
  def last: Option[DoubleLinked[T]]

  def isEmpty = this == DoubleLinked.empty
  def hasNext = next.isDefined
  def hasPrev = prev.isDefined

  // Updates the value of the current node.
  def update[S >: T](value: S): DoubleLinked[S]

  // Adds a new node at the beginning of the list but returns the current node
  def prepend[S >: T](e: S): DoubleLinked[S]
  // Adds a new node at the end of the list but returns the current node
  def append[S >: T](e: S): DoubleLinked[S]
  // Removes current node from the list
  def drop(): DoubleLinked[T]

  def toIterable: Iterable[T]

  def length: Int
  def size: Int = length

  // The index of the current node in list
  def valueIndex: Option[Int]

  // Move the pointer of current node to the given index.
  // Note that even when the user is responsible for checking that index is bounded
  // by the size of this list, this operation is safe, and providing invalid indices
  // will never create an invalid pointer.
  def pointAt(index: Int): DoubleLinked[T]
  def pointAtNext: DoubleLinked[T]
  def pointAtPrev: DoubleLinked[T]
  def pointAtFirst: DoubleLinked[T]
  def pointAtLast: DoubleLinked[T]

  // Splits into two lists, the second one starts with the current node.
  def split(): (DoubleLinked[T], DoubleLinked[T])
  def slice(fromInclusiveIndex: Int, toExclusiveIndex: Int): DoubleLinked[T]

  def insertNext[S >: T](l: DoubleLinked[S]): DoubleLinked[S]
  def insertNext[S >: T](e: S): DoubleLinked[S] =
    insertNext(DoubleLinked.empty[S].update(e))

  def insertPrev[S >: T](l: DoubleLinked[S]): DoubleLinked[S]
  def insertPrev[S >: T](e: S): DoubleLinked[S] =
    insertPrev(DoubleLinked.empty[S].update(e))

object DoubleLinked:

  given [T]: Conversion[DoubleLinked[T], Iterable[T]] =
    _.toIterable

  def fromIterable[T](i: Iterable[T]): DoubleLinked[T] =
    i.foldLeft(empty: DoubleLinked[T])(_ append _)

  def empty[T]: DoubleLinked[T] = Empty

  object Empty extends DoubleLinked[Nothing]:
    override def length = 0
    override def toIterable = Iterable.empty

    override def pointAt(index: Int) = this
    override def pointAtNext = this
    override def pointAtPrev = this
    override def pointAtFirst = this
    override def pointAtLast = this

    override def value = None
    override def valueIndex = None

    override def prev = None
    override def next = None

    override def first = None
    override def last = None

    override def update[S](e: S) = NonEmpty(0, List(e))
    override def drop() = this

    override def prepend[S](e: S) = update(e)
    override def append[S](e: S) = update(e)

    override def split() = (this, this)
    override def slice(fromInclusiveIndex: Int, toExclusiveIndex: Int) = this

    override def insertNext[S](l: DoubleLinked[S]) = l
    override def insertPrev[S](l: DoubleLinked[S]) = l

  @throws[IllegalArgumentException](
    "if the list is empty or index is out of bounds"
  )
  final private[DoubleLinked] case class NonEmpty[+T](index: Int, list: List[T])
      extends DoubleLinked[T]:
    require(list.nonEmpty && index >= 0 && index < length)

    override lazy val length = list.length
    override def toIterable = list

    override def pointAt(index: Int) =
      val idx = 0.max(index).min(length - 1)
      NonEmpty(idx, list)

    override def pointAtNext = pointAt(index + 1)
    override def pointAtPrev = pointAt(index - 1)
    override def pointAtFirst = pointAt(0)
    override def pointAtLast = pointAt(length - 1)

    override def update[S >: T](v: S) =
      NonEmpty(index, list.updated(index, v))

    override def value = Some(list(index))
    override def valueIndex = Some(index)

    override def prev =
      if index == 0 then None else Some(NonEmpty(index - 1, list))

    override def next =
      if index == length - 1 then None
      else Some(NonEmpty(index + 1, list))

    override def first = Some(NonEmpty(0, list))
    override def last = Some(NonEmpty(length - 1, list))

    override def prepend[S >: T](e: S) =
      NonEmpty(index + 1, e +: list)
    override def append[S >: T](e: S) = NonEmpty(index, list :+ e)

    override def drop() =
      if index == 0 && length == 1 then empty
      else if !hasNext then NonEmpty(index - 1, list.slice(0, index))
      else
        NonEmpty(
          index,
          list.slice(0, index) ++ list.slice(index + 1, length)
        )

    override def split() = (slice(0, index), slice(index, length))
    override def slice(fromInclusiveIndex: Int, toExclusiveIndex: Int) =
      val to = 0.max(toExclusiveIndex).min(length)
      val from = 0.max(fromInclusiveIndex).min(to)
      val lst = list.slice(from, to)
      if lst.isEmpty then empty else NonEmpty(0, lst)

    override def insertNext[S >: T](e: DoubleLinked[S]): DoubleLinked[S] =
      if e.isEmpty then this
      else
        NonEmpty(
          index,
          list.slice(0, index + 1) ++
            e.toList ++
            list.slice(index + 1, length)
        )

    override def insertPrev[S >: T](e: DoubleLinked[S]): DoubleLinked[S] =
      if e.isEmpty then this
      else
        NonEmpty(
          index,
          list.slice(0, index) ++
            e.toList ++
            list.slice(index, length)
        )
