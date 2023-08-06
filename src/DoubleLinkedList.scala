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

  // Turns the double-linked list starting at this node into an Iterable
  def toIterable: Iterable[T]

object DoubleLinked:
  object empty extends DoubleLinked[Nothing]:

    def toIterable: Iterable[Nothing] = Iterable.empty

    def value: Option[Nothing] = None

    def prev: Option[DoubleLinked[Nothing]] = None
    def next: Option[DoubleLinked[Nothing]] = None

    def first: Option[DoubleLinked[Nothing]] = None
    def last: Option[DoubleLinked[Nothing]] = None

    def update[S](e: S): DoubleLinked[S] = new NonEmpty(0, List(e))
    def drop(): DoubleLinked[Nothing] = this

    def prepend[S](e: S): DoubleLinked[S] = update(e)
    def append[S](e: S): DoubleLinked[S] = update(e)

  @throws[IllegalArgumentException](
    "if the list is empty or index is out of bounds"
  )
  final private[DoubleLinked] class NonEmpty[+T](index: Int, list: List[T])
      extends DoubleLinked[T]:
    require(list.nonEmpty && index >= 0 && index < list.length)

    def toIterable: Iterable[T] = list.slice(index, list.length)

    def update[S >: T](v: S): DoubleLinked[S] =
      new NonEmpty(index, list.updated(index, v))

    def value: Option[T] = Some(list(index))

    def prev: Option[DoubleLinked[T]] =
      if index == 0 then None else Some(new NonEmpty(index - 1, list))

    def next: Option[DoubleLinked[T]] =
      if index == list.length - 1 then None
      else Some(new NonEmpty(index + 1, list))

    def first: Option[DoubleLinked[T]] = Some(new NonEmpty(0, list))
    def last: Option[DoubleLinked[T]] = Some(
      new NonEmpty(list.length - 1, list)
    )

    def prepend[S >: T](e: S): DoubleLinked[S] =
      new NonEmpty(index + 1, e +: list)
    def append[S >: T](e: S): DoubleLinked[S] = new NonEmpty(index, list :+ e)

    def drop(): DoubleLinked[T] =
      if index == 0 && list.length == 1 then empty
      else if !hasNext then new NonEmpty(index - 1, list.slice(0, index))
      else
        new NonEmpty(
          index,
          list.slice(0, index) ++ list.slice(index + 1, list.length)
        )
