package com.github.vic

// An immutable, double-linked list.
sealed trait DLink[+T] extends DLink.Safe[T]:
  def unsafe: DLink.Unsafe[T]

  def isEmpty = this == DLink.empty
  def hasNext = next.isDefined
  def hasPrev = prev.isDefined

  // Updates the value of the current node.
  def update[S >: T](value: S): DLink[S]

  // Updates the previous node content but returns the current node
  def updatePrev[S >: T](e: S): DLink[S]
  // Updates the next node content but returns the current node
  def updateNext[S >: T](e: S): DLink[S]

  // Adds a new node at the beginning of the list but returns the current node
  def prepend[S >: T](e: S): DLink[S]
  // Adds a new node at the end of the list but returns the current node
  def append[S >: T](e: S): DLink[S]

  // Turns the double-linked list starting at this node into an Iterable
  def toIterable: Iterable[T]

object DLink:
  private[DLink] type Id[+T] = T

  private[DLink] trait W[M[+_], +T]:
    def value: M[T]
    def next: M[DLink[T]]
    def prev: M[DLink[T]]
    def first: M[DLink[T]]
    def last: M[DLink[T]]

  private[DLink] trait Safe[+T] extends W[Option, T]
  private[DLink] trait Unsafe[+T] extends W[Id, T]

  object empty extends DLink[Nothing]:

    def toIterable: Iterable[Nothing] = Iterable.empty

    val unsafe: Unsafe[Nothing] = new:

      @throws[NoSuchElementException]("if the list is empty")
      def value: Nothing = throw new NoSuchElementException("Empty.value")
      def prev: DLink[Nothing] = empty
      def next: DLink[Nothing] = empty

      def first: DLink[Nothing] = empty
      def last: DLink[Nothing] = empty

    def value: Option[Nothing] = None
    def prev: Option[DLink[Nothing]] = None
    def next: Option[DLink[Nothing]] = None
    def first: Option[DLink[Nothing]] = None
    def last: Option[DLink[Nothing]] = None

    def updatePrev[S](e: S): DLink[S] = update(e)
    def updateNext[S](e: S): DLink[S] = update(e)

    def prepend[S](e: S): DLink[S] = update(e)
    def append[S](e: S): DLink[S] = update(e)

    def update[S](e: S): DLink[S] = new NonEmpty(0, List(e))

  @throws[IllegalArgumentException](
    "if the list is empty or index is out of bounds"
  )
  final private[DLink] class NonEmpty[+T](index: Int, list: List[T])
      extends DLink[T]:
    require(list.nonEmpty && index >= 0 && index < list.length)

    def toIterable: Iterable[T] = list.slice(index, list.length)

    def update[S >: T](v: S): DLink[S] =
      new NonEmpty(index, list.updated(index, v))

    val unsafe: Unsafe[T] = new:
      def value: T = list(index)
      def prev: DLink[T] =
        if index == 0 then empty else new NonEmpty(index - 1, list)
      def next: DLink[T] =
        if index == list.length - 1 then empty
        else new NonEmpty(index + 1, list)

      def first: DLink[T] = new NonEmpty(0, list)
      def last: DLink[T] = new NonEmpty(list.length - 1, list)

    def value: Option[T] = Some(list(index))
    def prev: Option[DLink[T]] =
      if index == 0 then None else Some(new NonEmpty(index - 1, list))
    def next: Option[DLink[T]] =
      if index == list.length - 1 then None
      else Some(new NonEmpty(index + 1, list))
    def first: Option[DLink[T]] = Some(new NonEmpty(0, list))
    def last: Option[DLink[T]] = Some(new NonEmpty(list.length - 1, list))

    def updatePrev[S >: T](e: S): DLink[S] = new NonEmpty(
      index,
      list.slice(0, index) :+ e :++ list.slice(index, list.length)
    )
    def updateNext[S >: T](e: S): DLink[S] = new NonEmpty(
      index,
      list.slice(0, index + 1) :+ e :++ list.slice(index + 1, list.length)
    )

    def prepend[S >: T](e: S): DLink[S] = new NonEmpty(index + 1, e +: list)
    def append[S >: T](e: S): DLink[S] = new NonEmpty(index, list :+ e)
