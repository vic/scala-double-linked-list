//> using test.dep "com.lihaoyi::utest:0.8.1"

package io.github.vic.dll.test

import utest._

import io.github.vic.dll.{DoubleLinked => DL}

object DoubleLinkedListTest extends TestSuite:

  val tests = Tests:
    import DL.empty

    test("empty"):

      test("from empty iterable"):
        assert(empty == DL.fromIterable(Nil))

      test("has no value in node"):
        assert(empty.value == None)

      test("has no next node"):
        assert(!empty.hasNext, empty.next == None)

      test("has no prev node"):
        assert(!empty.hasPrev, empty.prev == None)

      test("has no first node"):
        assert(empty.first == None)

      test("has no last node"):
        assert(empty.last == None)

      test("toIterable is empty"):
        assert(empty.toSeq.isEmpty)

      test("drop returns the same empty list"):
        assert(empty == empty.drop())

      test("update transforms into a non-empty list"):
        val nonEmpty = empty.update(42)
        assert(empty != nonEmpty, !nonEmpty.isEmpty)

      test("appending creates a non-empty list"):
        val nonEmpty = empty.append(42)
        assert(empty != nonEmpty)

      test("prepending creates a non-empty list"):
        val nonEmpty = empty.prepend(42)
        assert(empty != nonEmpty)

      test("point at any index is always empty"):
        assert(
          None == empty.valueIndex,
          empty == empty.pointAt(0),
          empty == empty.pointAt(-99),
          empty == empty.pointAt(99)
        )

    test("nonEmpty"):

      test("single item"):
        val a = empty.append(1)

        test("has no next"):
          assert(a.next == None, !a.hasNext)
        test("has no prev"):
          assert(a.prev == None, !a.hasPrev)
        test("contains value"):
          assert(a.value == Some(1))
        test("drop returns empty"):
          assert(a.drop() == empty)
        test("update replaces value"):
          val x = a.update(99)
          assert(x.toList == List(99))
        test("pointing at out of bounds index does not move pointer"):
          assert(
            a.valueIndex == Some(0),
            a == a.pointAt(0),
            a == a.pointAt(-99),
            a == a.pointAt(99)
          )

      test("two items"):
        val b = empty.append(1).append(2).last.get

        test("contains value"):
          assert(
            b.value == Some(2),
            b.hasPrev,
            !b.hasNext,
            b.valueIndex == Some(1)
          )

        test("drop removes element"):
          val a = b.drop()
          assert(a.value == Some(1), !a.hasPrev, !a.hasNext, a.length == 1)

        test("removing first element keeps last"):
          val x = b.first.get.drop()
          assert(x.value == Some(2), x.length == 1)

        test("prepend inserts at the head"):
          val x = b.prepend(0).first.get
          assert(x.toList == List(0, 1, 2))

      test("fromIterable"):
        val x = DL.fromIterable(Seq(1, 2, 3))

        test("toIterable results in same seq"):
          assert(x.to(Seq) == Seq(1, 2, 3))

        test("pointAt is bounds-safe"):
          assert(
            Some(1) == x.pointAt(0).value,
            Some(1) == x.pointAt(-99).value,
            Some(3) == x.pointAt(x.length - 1).value,
            Some(3) == x.pointAt(99).value,
            Some(2) == x.pointAt(1).value
          )

      test("split"):
        test("on an empty list"):
          val (a, b) = empty.split()
          assert(empty == a, b == empty)

        val x = DL.fromIterable(0.to(10)).pointAt(5)
        test("partitions at node position in two sub-lists"):
          val (a, b) = x.split()
          assert(
            a.toList == 0.to(4).toList,
            b.toList == 5.to(10).toList
          )

      test("insert"):
        val x = DL.fromIterable(Seq(0, 1, 2)).pointAt(1)
        val y = DL.fromIterable(Seq(10, 11))

        test("an element as previous"):
          val a = x.insertPrev(99)
          assert(a.toList == List(0, 99, 1, 2))

        test("an element as next"):
          val a = x.insertNext(99)
          assert(a.toList == List(0, 1, 99, 2))

        test("a list as previous"):
          val a = x.insertPrev(y)
          assert(a.toList == List(0, 10, 11, 1, 2))

        test("a list as next"):
          val a = x.insertNext(y)
          assert(a.toList == List(0, 1, 10, 11, 2))
