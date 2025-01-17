// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util.mutable

import wueortho.data.NodeIndex
import wueortho.util.Monoid, Monoid.syntax.*
import wueortho.util.mutable.DisjointSets.*

import scala.collection.mutable

trait DisjointSets[K: AsInt, V: Monoid]:
  def apply(key: K): Option[V] = Option.when(contains(key))(getOrElseThrow(key))
  def mkSet(key: K, value: V): V
  def union(a: K, b: K): V
  def sameSet(a: K, b: K): Boolean
  def contains(key: K): Boolean
  def getOrElseThrow(key: K): V
  def values: Seq[V]

object DisjointSets:
  @FunctionalInterface trait AsInt[T]:
    def asInt(t: T): Int

  object AsInt:
    given AsInt[Int]       = identity
    given AsInt[NodeIndex] = _.toInt

  extension [K: AsInt](k: K) def asInt = summon[AsInt[K]].asInt(k)

  private class Entry[V: Monoid](var pointer: Int, var rank: Int, var value: V)
  private object Entry:
    def nil[V: Monoid] = Entry(-1, -1, Monoid.zero)

  def apply[K: AsInt, V: Monoid] = new DisjointSets[K, V]:
    val entries = mutable.ArrayBuffer.empty[Entry[V]]

    def findSet(i: Int): Int =
      val e = entries(i)
      if e.pointer != i then e.pointer = findSet(e.pointer)
      e.pointer

    override def getOrElseThrow(key: K) = entries(findSet(key.asInt)).value

    override def contains(key: K) = key.asInt >= 0 && key.asInt < entries.length && entries(key.asInt).pointer != -1

    override def sameSet(a: K, b: K) = contains(a) && contains(b) && (findSet(a.asInt) == findSet(b.asInt))

    override def mkSet(key: K, value: V) =
      val i   = key.asInt
      if entries.size <= i then entries ++= Iterator.fill(i - entries.size + 1)(Entry.nil)
      val res = entries(i)
      res.pointer = i
      res.value = value
      res.rank = 0
      value

    def link(a: Int, b: Int) =
      if a == b then entries(a).value
      else
        val (aa, bb) = (entries(a), entries(b))
        if aa.rank > bb.rank then
          bb.pointer = a
          aa.value = aa.value <+> bb.value
          bb.value = Monoid.zero // help the GC
          aa.value
        else
          aa.pointer = b
          bb.value = bb.value <+> aa.value
          aa.value = Monoid.zero // help the GC
          if aa.rank == bb.rank then bb.rank += 1
          bb.value
        end if
    end link

    override def union(a: K, b: K) = link(findSet(a.asInt), findSet(b.asInt))

    override def values = entries.iterator.zipWithIndex.filter((e, i) => e.pointer == i).map(_._1.value).toSeq
end DisjointSets
