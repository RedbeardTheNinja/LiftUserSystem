

package blank {
package snippet {

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import util._
import common._
import java.util.Date
import blank.lib._
import Helpers._
import Red.lib.DependencyFactory
import Red.model.User

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  def howdy(in: NodeSeq): NodeSeq =
  Helpers.bind("b", in, "time" -> date.map(d => Text(d.toString)))


  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy(in: NodeSeq): NodeSeq = Helpers.bind("b", in, "time" -> date.toString)
   */
}

}
}