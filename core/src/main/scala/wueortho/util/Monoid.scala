// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import scala.annotation.targetName

trait Monoid[T]:
  def zero: T
  def apply(a: T, b: T): T

object Monoid:
  def zero[T: Monoid] = summon[Monoid[T]].zero

  given Monoid[Unit] = new Monoid[Unit]:
    override def zero                    = ()
    override def apply(a: Unit, b: Unit) = ()

  object syntax:
    extension [V: Monoid](v: V) @targetName("addAll") def <+>(o: V) = summon[Monoid[V]](v, o)
end Monoid
