package org.eichelberger.uwot

import breeze.stats.distributions.Gaussian

case class Aggregate(id: String, n: Double, min: Double, max: Double, sum: Double, mean: Double, variance: Double) {

  def this(id: String, x: Double) = this(id, x, x, x, x, x, 0.0)

  def this(id: String, x: Double, weight: Double) = this(id, weight, x, x, x, x, 0.0)

  private def M2: Double = variance * n

  lazy val sufficientStatistic: Gaussian.SufficientStatistic =
    Gaussian.SufficientStatistic(n, mean, M2)

  lazy val stddev: Double = Math.sqrt(variance)

  override def toString: String =
    f"$id%s:[$min%1.2f,$sum%1.2f/$n%1.2f=$mean%1.2f,$max%1.2f::$variance%1.2f]"

  def update(x: Double, weight: Double = 1.0): Aggregate = {
    // guard
    if (x == 0.0 && sufficientStatistic.mean == 0.0 && sum == 0.0) return this

    val nextSuffStat = sufficientStatistic.+(Gaussian.SufficientStatistic(1, x, 0.0))
    println(s"nSS1:  $sufficientStatistic -> $nextSuffStat")

    Aggregate(
      id,
      n + weight,
      Math.min(min, x),
      Math.max(max, x),
      sum + x,
      nextSuffStat.mean,
      nextSuffStat.variance
    )
  }

  def update(agg: Aggregate): Aggregate = {
    // guard
    if (agg.sum == 0.0 && sufficientStatistic.mean == 0.0 && sum == 0.0) return this

    agg match {
      case a: Aggregate =>
        val nextSuffStat = sufficientStatistic.+(agg.sufficientStatistic)
        println(s"nSS2:  $sufficientStatistic -> $nextSuffStat")
        Aggregate(
          a.id,
          n + a.n,
          Math.min(min, a.min),
          Math.max(max, a.max),
          sum + a.sum,
          nextSuffStat.mean,
          nextSuffStat.variance
        )
      case _ => throw new Exception("Invalid class parameter")
    }
  }

  def score(optOther: Option[Aggregate]): Double =
    optOther.map(score).getOrElse(Data.MinProbability)

  def score(other: Aggregate): Double = {
    val a = mean
    val b = other.mean

    if (other.n > Data.MinSampleSize && n >= Data.MinSampleSize) {
      // TODO:  DEBUG!
      print(s"score Gaussian:  a $a, b $b, score ")
      println(
        Gaussian(mean, stddev).probability(Math.min(a, b), Math.max(a, b))
      )

      // simple Gaussian comparison (whether or not that's reasonable for these unknown distributions)
      Gaussian(mean, stddev).probability(Math.min(a, b), Math.max(a, b))
    } else {
      // you have too few samples, so assume a uniform distribution

      // bit of gymnastics to handle case where one (or both) means are negatove
      val delta = Math.abs(a - b)
      val rho = Math.min(Math.abs(a), Math.abs(b))

      // TODO:  DEBUG!
      print(s"score uniform:  a $a, b $b, rho $rho, delta $delta, score ")
      println(
      if (delta + rho < Data.MinProbability) Data.MinProbability
      else rho / (delta + rho))

      if (delta + rho < Data.MinProbability) Data.MinProbability
      else rho / (delta + rho)
    }
  }
}

