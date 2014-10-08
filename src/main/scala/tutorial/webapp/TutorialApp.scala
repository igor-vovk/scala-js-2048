package tutorial.webapp

import scala.scalajs.js

import org.scalajs.dom

object TutorialApp extends js.JSApp {

  override def main(): Unit = {
    appendPar(dom.document.body, "Hello World")
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = dom.document.createElement("p")
    val textNode = dom.document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

}
