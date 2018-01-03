import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import com.typesafe.scalalogging.LazyLogging

import scala.collection.SortedSet

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
  )

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

      elements.foreach(e => xm.add(e.toLowerCase()))
      val profile = xm.profile(10000)
      val keys = profile.keySet.toList.sorted
      logger.debug("XM(*elements*).profile:")
      keys.take(10).foreach { key => {
        logger.debug(f"  ${profile(key)}%1.3f  $key%s")
      }}

      // dummy value
      1 must equalTo(1)
    }
  }
}
