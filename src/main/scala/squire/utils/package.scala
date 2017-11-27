package squire

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable.{IndexedSeq => MutableIndexedSeq, Map => MutableMap}

package object utils {

  def toMutableSeq[T](seq: IndexedSeq[T]): MutableIndexedSeq[T] = MutableIndexedSeq[T](seq:_*)
  def toImmutableSeq[T](seq: MutableIndexedSeq[T]): IndexedSeq[T] = IndexedSeq[T](seq:_*)
  def toMutableMap[K, T](map: Map[K, T]): MutableMap[K, T] = MutableMap[K, T](map.toSeq:_*)
  def toImmutableMap[K, T](map: Map[K, T]): ImmutableMap[K, T] = ImmutableMap[K, T](map.toSeq:_*)

}
