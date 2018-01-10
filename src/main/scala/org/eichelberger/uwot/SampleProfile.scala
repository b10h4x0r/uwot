package org.eichelberger.uwot

import scala.collection.mutable.{HashMap => MutableHashMap}

class SampleProfile(size: Int) {
  def this(samples: Seq[String]) = {
    this(samples.size)
    samples.foreach(s => add(s))
  }

  val data = new MutableHashMap[String, Double]().withDefaultValue(0.0)

  val delta: Double = 1.0 / size.toDouble

  def keySet = data.keySet

  def add(sample: String, weight: Double = 1.0): Unit = {
    data.update(sample, (weight * delta) + data(sample))
  }

  def get(sample: String): Double = data(sample)

  def byKey: Seq[(String, Double)] = {
    data.toSeq.sortWith(
      (a: (String, Double), b: (String, Double)) =>
        Math.signum(a._1.compareTo(b._1)) match {
          case -1 => true
          case  0 => a._2 < b._2
          case  1 => false
        }
    )
  }

  def byValue: Seq[(String, Double)] = {
    data.toSeq.sortWith(
      (a: (String, Double), b: (String, Double)) =>
        Math.signum(a._2.compareTo(b._2)) match {
          case -1 => true
          case  0 => a._1 < b._1
          case  1 => false
        }
      )
  }

  // defined on [0.0, 1.0]; should be symmetric
  def similarityTo(other: SampleProfile): Double = {
    var same: Double = 0.0
    var diff: Double = 0.0
    var a: Double = 0.0
    var b: Double = 0.0

    val keys = keySet ++ other.keySet
    keys.foreach { key =>
      a = get(key)
      b = other.get(key)
      same += Math.min(a, b)
      diff += Math.abs(a - b)
    }

    if ((same + diff) > 0.0) same / (same + diff)
    else 1.0  // only possible for two empty profiles, in which case they're equal
  }
}


