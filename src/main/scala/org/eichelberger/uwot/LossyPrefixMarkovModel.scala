package org.eichelberger.uwot

import com.typesafe.scalalogging.LazyLogging
import org.eichelberger.uwot.Const.{BOS, EOS, ROOT, rws}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}
import scala.util.Try

object Const {
  val BOS: String = "__BOS__"
  val EOS: String = "__EOS__"
  val ROOT: String = "__ROOT__"

  def rws[T](kvs: Map[T, Double]): T = {
    val x = kvs.values.sum * Math.random()
    var y = 0.0
    for (kv <- kvs) {
      y += kv._2
      if (y >= x) return kv._1
    }
    kvs.last._1
  }
}

case class Data(raw: String) {
  private val subs: Seq[String] =
    Seq(BOS) ++ raw.split("").toSeq ++ Seq(EOS)
  def subsequences(size: Int, step: Int): Iterator[Seq[String]] =
    subs.sliding(size, step)
}

//trait LossyPrefixMarkovModel[+T] {
//  def isMutable: Boolean
//  def depth: Int
//  def data: String
//  def weight: Double
//  def children[U >: T]: MutableMap[String, U]
//
//}

class MutableLPMM(val data: String = ROOT, var weight: Double = 1.0, val depth: Int = 3) extends LazyLogging {
  def isMutable: Boolean = true

  private val children: MutableMap[String, MutableLPMM] = new MutableHashMap[String, MutableLPMM]()

  def clear(): Unit = {
    children.clear()
  }

  override def toString: String = toString("")

  def toString(leader: String, hasMoreSiblings: Boolean = false): String = {
    val sb = new StringBuffer()

    // self
    sb.append(leader)
    sb.append(data)
    sb.append(" (")
    sb.append(f"$weight%1.2f")
    sb.append(")")
    sb.append("\n")

    // next leader
    val nextPart = if (hasMoreSiblings) "|   " else "    "
    val nextLeader =
      if (leader.length > 1) leader.substring(0, leader.length - 4) + nextPart
      else leader

    sb.append(nextLeader)
    if (children.nonEmpty) sb.append("|")
    sb.append("\n")

    // children
    if (children.nonEmpty) {
      val n = children.size
      children.toList.zipWithIndex.map {
        case ((key, value), index) =>
          val isLast = index == (n - 1)
          val prefix = if (isLast) "\\-- " else "+-- "
          sb.append(value.toString(nextLeader + prefix, !isLast))
      }
    }

    sb.toString
  }

  def add(s: String): MutableLPMM = {
    val data = Data(s)
    data.subsequences(depth, 1).foreach(subseq => {
      accumulate(subseq)
    })
    this
  }

  @tailrec
  private def accumulate(subseq: Seq[String]): Unit = {
    // dummy check
    if (subseq.isEmpty) return

    val head = subseq.head
    val tail = subseq.tail

    var child: MutableLPMM = null
    if (children.contains(head)) {
      // existing child
      child = children(head)
      child.weight += 1.0
    } else {
      // new child
      child = new MutableLPMM(head, 1.0)
      children.put(head, child)
    }

    // recurse
    child.accumulate(tail)
  }

  private def nextElement(soFar: Seq[MutableLPMM]): MutableLPMM = {
    // TODO:  use the sequence so far

    // build the list for RWS
    val kvs = children.map {
      case (_, nextChild) => (nextChild, nextChild.weight)
    }.toMap

    // return the probabilistic child
    rws(kvs)
  }

  private def matchPrefix(sequence: Seq[MutableLPMM]): Try[MutableLPMM] = Try {
    val subseq = sequence.takeRight(depth - 1)
    var node = this
    subseq.foreach { subnode => {
      if (!node.children.contains(subnode.data))
        throw new Exception(s"Cannot find child:  $subnode")
      node = node.children(subnode.data)
    }}

    node
  }

  def sample(): String = {
    // first node must be BOS
    var node = children.getOrElse(BOS, throw new Exception("Tree does not contain BOS marker"))
    var s = ""

    // sanity check
    if (node.children.isEmpty) return ""

    var sequence = new mutable.ListBuffer[MutableLPMM]()
    sequence.append(node)

    // fetch subsequent nodes
    while (node.data != EOS) {
      // accumulate this contribution
      if (node.data != BOS) s = s + node.data

      //println(s"sample node:  '${node.data}', sample '$s', sequence '${sequence.map(_.data).mkString("[",",","]")}'")

      // re-seek your place in the tree
      node = matchPrefix(sequence).get

      // select the next child probabilistically
      node = node.nextElement(sequence)
      sequence.append(node)
    }

    s
  }

  def profile(sampleSize: Int = 100): SampleProfile = {
    val returnMap = new SampleProfile(sampleSize)

    var i = 0
    var s: String = null
    while (i < sampleSize) {
      s = sample()
      returnMap.add(s)
      i += 1
    }

    returnMap
  }
}

//class ImmutableLPMM() extends LossyPrefixMarkovModel {
//  def isMutable: Boolean = false
//}
