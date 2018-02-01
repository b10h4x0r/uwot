package org.eichelberger.uwot

import breeze.stats.distributions.Gaussian
import com.typesafe.scalalogging.LazyLogging
import org.eichelberger.uwot.Const.{BOS, EOS, ROOT, rws}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
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

case class Aggregates(aggMap: HashMap[String, Aggregate]) {
  def this() = this(HashMap.empty[String, Aggregate])
  def update(id: String, x: Double, weight: Double = 1.0): Aggregates = Aggregates(aggMap.get(id) match {
    case Some(agg) => aggMap.updated(id, agg.update(x, weight))
    case None      => aggMap + (id -> new Aggregate(id, x))
  })
  def update(agg: Aggregate): Aggregates = Aggregates(aggMap.get(agg.id) match {
    case Some(oldAgg) => aggMap.updated(agg.id, oldAgg.update(agg))
    case None      => aggMap + (agg.id -> agg)
  })
  def replace(agg: Aggregate): Aggregates = Aggregates(
    aggMap.updated(agg.id, agg)
  )
  def score(node: MutableLPMM): Double = {
    // degenerate case:  we don't care about aggregates
    if (!Data.UseAggregates) return node.weight

    // simple case:  unbiased blend of aggregate scores
    val allKeys = node.aggregates.aggMap.keySet ++ aggMap.keySet
    allKeys.map(key =>
      (aggMap.get(key), node.aggregates.aggMap.get(key)) match {
        // a:  sequence so far; b:  candidate next node
        case (Some(a), optB) => a.score(optB)
        case (optA, Some(b)) => b.score(optA)
        case _               => throw new Exception(s"Aggregate-collections both missing key '$key'")
      }
    ).sum * node.weight
  }
}

case class RichToken(token: String, aggs: Aggregates) {
  def this() = this("dummy", new Aggregates())
}

object Data {
  val UseAggregates: Boolean = true
  val MinProbability: Double = 1e-3
  val MinSampleSize: Double = 5.0
}

case class Data(raw: String) {
  private val subs: Seq[String] =
    Seq(BOS) ++ raw.split("").toSeq ++ Seq(EOS)

  val n = subs.length
  val enriched: Seq[RichToken] = subs.zipWithIndex.foldRight(Seq[RichToken]())((tokenIndex, acc) => tokenIndex match {
    case (token, index) =>
      // TODO:  refactor to use the node-sequence aggregation
      val lenLeft = n - index - 1
      val isVowel = if (token.toUpperCase.matches("A|E|I|O|U|Y")) 1.0 else 0.0
      val aggs: Aggregates = acc.headOption match {
        case Some(rt) => rt.aggs
        case None     => new Aggregates()
      }
      Seq(RichToken(
        token,
        aggs
          .replace(new Aggregate("length", lenLeft))
          .update("vowels", isVowel)
      )) ++ acc
  })

  def subsequences(size: Int, step: Int): Iterator[Seq[RichToken]] =
    enriched.sliding(size, step)
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

  var aggregates = Aggregates(HashMap.empty[String, Aggregate])

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
    val tab = nextLeader + (if (children.nonEmpty) "|" else " ")

    // aggregates
    aggregates.aggMap.map {
      case (key, agg) =>
        sb.append(tab)
        sb.append("  ")
        sb.append(agg.toString)
        sb.append("\n")
    }

    // vertical spacer
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
  private def accumulate(subseq: Seq[RichToken]): Unit = {
    // dummy check
    if (subseq.isEmpty) return

    val head = subseq.head
    val tail = subseq.tail

    var child: MutableLPMM = null
    if (children.contains(head.token)) {
      // existing child
      child = children(head.token)
      child.weight += 1.0
    } else {
      // new child
      child = new MutableLPMM(head.token, 1.0)
      children.put(head.token, child)
    }

    // update aggregates
    head.aggs.aggMap.foreach {
      case (_, agg) =>
        // TODO:  debug!
        val a = child.aggregates
        val b = child.aggregates.update(agg)
        println(s"Updating '${head.token}':  aggregate $a -> $b")

        child.aggregates = child.aggregates.update(agg)
    }

    // recurse
    child.accumulate(tail)
  }

  private def nextElement(soFar: Seq[MutableLPMM], aggsSoFar: Aggregates): MutableLPMM = {
    // TODO:  use the sequence so far?

    // build the list for RWS
    val kvs = children.map {
      case (_, nextChild) =>
        val weight = aggsSoFar.score(nextChild)
        (nextChild, weight)
    }.toMap

    // TODO:  debug!
    println(s"\nnextElement(${soFar.map(_.data).mkString("(",",",")")}...\n${kvs.map(t => t._1.data + " -> " + t._2.toString).mkString("(","\n  ",")")}")

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

  def aggregateNode(token: String, aggs: Aggregates): Aggregates = {
    aggs.replace(new Aggregate(  // length
      "length",
      aggs.aggMap.get("length").map(_.max).getOrElse(0.0) + 1.0
    ))
    .update(new Aggregate(  // fraction of vowels
      "vowel",
      if (token.toUpperCase.matches("A|E|I|O|U|Y")) 1.0 else 0.0
    ))
  }

  def sample(): String = {
    // first node must be BOS
    var node = children.getOrElse(BOS, throw new Exception("Tree does not contain BOS marker"))
    var s = ""

    // sanity check
    if (node.children.isEmpty) return ""

    // aggregate properties pertaining to the string being built
    var seqAggs = new Aggregates()

    var sequence = new mutable.ListBuffer[MutableLPMM]()
    sequence.append(node)

    // fetch subsequent nodes
    while (node.data != EOS) {
      // accumulate this contribution
      if (node.data != BOS) s = s + node.data
      seqAggs = aggregateNode(node.data, seqAggs)

      // re-seek your place in the tree
      node = matchPrefix(sequence).get

      // select the next child probabilistically
      node = node.nextElement(sequence, seqAggs)
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

  def similarityTo(other: MutableLPMM, sampleSize: Int = 100): Double =
    if (other != null) profile(sampleSize).similarityTo(other.profile(sampleSize))
    else if (children.isEmpty) 1.0
    else 0.0
}

//class ImmutableLPMM() extends LossyPrefixMarkovModel {
//  def isMutable: Boolean = false
//}
