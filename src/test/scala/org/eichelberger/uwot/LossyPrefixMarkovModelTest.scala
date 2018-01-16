package org.eichelberger.uwot

import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LossyPrefixMarkovModelTest extends Specification with LazyLogging {
  val elements = Seq[String](
    "Hydrogen", "Helium", "Lithium", "Beryllium", "Boron", "Carbon", "Nitrogen",
    "Oxygen", "Fluorine", "Neon", "Sodium", "Magnesium", "Aluminum", "Silicon",
    "Phosphorus", "Sulfur", "Chlorine", "Argon", "Potassium", "Calcium",
    "Scandium", "Titanium", "Vanadium", "Chromium", "Manganese", "Iron",
    "Cobalt", "Nickel", "Copper", "Zinc", "Gallium", "Germanium", "Arsenic",
    "Selenium", "Bromine",  "Krypton", "Rubidium", "Strontium", "Yttrium",
    "Zirconium", "Niobium", "Molybdenum", "Technetium", "Ruthenium", "Rhodium",
    "Palladium", "Silver", "Cadmium", "Indium", "Tin", "Antimony", "Tellurium",
    "Iodine", "Xenon", "Cesium", "Barium", "Lanthanum", "Cerium", "Praseodymium",
    "Neodymium", "Promethium", "Samarium", "Europium", "Gadolinium", "Terbium",
    "Dysprosium", "Holmium", "Erbium", "Thulium", "Ytterbium", "Lutetium",
    "Hafnium", "Tantalum", "Tungsten", "Rhenium", "Osmium", "Iridium", "Platinum",
    "Gold", "Mercury", "Thallium", "Lead", "Bismuth", "Polonium", "Astatine",
    "Radon", "Francium", "Radium", "Actinium", "Thorium", "Protactinium",
    "Uranium", "Neptunium", "Plutonium", "Americium", "Curium", "Berkelium",
    "Californium", "Einsteinium", "Fermium", "Mendelevium", "Nobelium", "Lawrencium",
    "Rutherfordium", "Dubnium", "Seaborgium", "Bohrium", "Hassium", "Meitnerium",
    "Darmstadtium", "Roentgenium", "Copernicium", "Nihonium", "Flerovium",
    "Moscovium", "Livermorium", "Tennessine", "Oganesson"
  ).map(_.toLowerCase)

  "asdf" >> {
    "zxcv" >> {
      val xm = new MutableLPMM(depth = 4)
      xm must not beNull;

      xm.add("foo")
      logger.debug(s"XM(foo):\n$xm")

      xm.add("boom")
      logger.debug(s"XM(foo,boom):\n$xm")

      xm.add("form")
      logger.debug(s"XM(foo,boom,form):\n$xm")

      xm.clear()
      logger.debug(s"XM(*cleared*):\n$xm")

      elements.foreach(xm.add)
      val profile = xm.profile(10000)
      logger.debug("XM(*elements*).profile by SAMPLE:")
      profile.byKey.take(10).foreach {
        case (key, value) =>
          logger.debug(f"  $value%1.3f  $key%s")
      }
      logger.debug("XM(*elements*).profile by WEIGHT:")
      profile.byValue.takeRight(10).reverse.foreach {
        case (key, value) =>
          logger.debug(f"  $value%1.3f  $key%s")
      }

      // dummy value
      1 must equalTo(1)
    }

//    "self-similarity by depth study" >> {
//      val trueProfile = new SampleProfile(elements)
//
//      for (depth <- Seq(1, 2, 4, 8); numSamples <- Seq(10, 100, 1000, 10000, 100000)) {
//        val xm = new MutableLPMM(depth = depth)
//        elements.foreach(xm.add)
//        val sim = xm.similarityTo(xm, numSamples)
//        logger.debug(f"XM(*elements*) depth $depth%d self-sim with $numSamples%d samples:  $sim%1.3f")
//        val xmProfile = xm.profile(numSamples)
//        val trueSim = xmProfile.similarityTo(trueProfile)
//        logger.debug(f"XM(*elements*) depth $depth%d true-sim with $numSamples%d samples:  $trueSim%1.3f")
//      }
//
//      // dummy value
//      1 must equalTo(1)
//    }
  }
}
