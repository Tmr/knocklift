package code.snippet

import xml._
import net.liftweb._
import http._
import js._
import js.JE._
import JsCmds._
import JsExp._
import json._
import JsonAST._
import util.Helpers._

/**
 * Created with IntelliJ IDEA.
 * User: bobrus
 * Date: 22.02.13
 * Time: 13:52
 * To change this template use File | Settings | File Templates.
 */

case class Human(val name:String, age:Int)

class LKO {
  implicit val formats = DefaultFormats

  val injectDataFuncName = "kl.setData"

  val population =
    Human("Adam", 450) ::
    Human("Eva", 350)  ::
    Nil

  def injectData(in:NodeSeq) = {
    <head_merge>
      <script type="text/javascript" src="/classpath/fobo/knockout.js"></script>
      <script type="text/javascript" src="/js/knocklift.js"></script>
      {Script(Call(injectDataFuncName, Extraction.decompose(population)))}
    </head_merge>
  }

}
