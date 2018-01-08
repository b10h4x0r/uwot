package org.eichelberger.uwot

import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SampleProfileTest extends Specification with LazyLogging {
  "asdf" >> {
    "work on a singleton" >> {
      val s1 = new SampleProfile(10)
      s1.add("foo", 2.0)
      s1.add("bar", 7.0)
      s1.add("baz", 1.0)
      s1 must not beNull;

      s1.similarityTo(s1) must closeTo(1.0, 1e-6)
    }

    "work against an empty" >> {
      val s1 = new SampleProfile(10)
      s1.add("foo", 2.0)
      s1.add("bar", 7.0)
      s1.add("baz", 1.0)
      s1 must not beNull;
      s1.similarityTo(s1) must closeTo(1.0, 1e-6)

      val s2 = new SampleProfile(30)
      s2.similarityTo(s2) must closeTo(1.0, 1e-6)

      s1.similarityTo(s2) must closeTo(0.0, 1e-6)
      s2.similarityTo(s1) must closeTo(0.0, 1e-6)
    }

    "match manual results" >> {
      val s1 = new SampleProfile(10)
      s1.add("foo", 2.0)
      s1.add("bar", 7.0)
      s1.add("baz", 1.0)
      s1 must not beNull;
      s1.similarityTo(s1) must closeTo(1.0, 1e-6)

      val s2 = new SampleProfile(30)
      s2.add("bar", 9.0)
      s2.add("baz", 9.0)
      s2.add("qux", 12.0)
      s2 must not beNull;
      s2.similarityTo(s2) must closeTo(1.0, 1e-6)

      s1.similarityTo(s2) must closeTo(0.25, 1e-6)
      s2.similarityTo(s1) must closeTo(0.25, 1e-6)
    }
  }
}
