package code.snippet

import net.liftweb._
import util._
import Helpers._
import http._
import js._
import js.JE._
import js.jquery.JqJE
import js.jquery.JqJsCmds.{FadeOut, FadeIn}
import js.JsCmds._
import xml._
import js._

/**
 * Created with IntelliJ IDEA.
 * User: bobrus
 * Date: 25.02.13
 * Time: 12:00
 * To change this template use File | Settings | File Templates.
 */

//class Human(var name:String, var age:Int)

object AEditable {

  case class Field(viewFunc: CssSel, editFunc: CssSel)

  val population =
    new Human("Adam", 930) ::
    new Human("Eva", 450)  ::
    Nil

  object dataVar extends SessionVar(population)

  def table(ns:NodeSeq) =
    <form class="lift:form.ajax">{ns}</form>

  def editableList = {
//    "*" #> population.map(h =>
//      "@name *" #> h.name &
//      "@age *" #> h.age
//    )
    "*" #> dataVar.get.map(h =>
      editable(
        "@controls *",
        () => S.notice(Text("Value is updated")),
        Field("@name *" #> h.name, "@name *" #> SHtml.text(h.name, h.name = _)),
        Field("@age *" #> h.age, "@age *" #> SHtml.number(h.age, h.age = _, 0, 1000))
      ) _
    )
  }

  def editable(controlsBind: String, onSubmit:() => JsCmd, fields:Field*)(ns:NodeSeq): NodeSeq = {

    val nodeId = nextFuncName

    var makeForm: CssSel = null // dumb forward declaration
    var rowTransform: CssSel = null // dumb forward declaration

    def onCancel() = {
      renderRow
    }

    def renderRow =
      Call("lift_show_edit_links") &
      Replace(nodeId, rowTransform(ns))

    rowTransform = viewRow(fields) &
      controlsBind #> SHtml.a(() => {
        Call("lift_hide_edit_links") &
          (JqJE.JqId(nodeId) ~> JsFunc("fadeOut", 500, AnonFunc(
            JsHideId(nodeId) &
            Replace(nodeId, makeForm(ns)) &
            JqJE.JqId(nodeId) ~> JsFunc("fadeIn", 500)
          )))
          //FadeOut(nodeId, 0, 500 millis) &


      }, Text("Edit"), ("class" -> "liftedit_link_class_")) &
      "tr [id]" #> nodeId

    makeForm = {
      editRow(fields) &
        controlsBind #>
          Group(Seq(SHtml.ajaxSubmit("Submit", () => {
            Call("lift_show_edit_links") &
            Replace(nodeId, rowTransform(ns)) &
            onSubmit()
          }), SHtml.a(onCancel _, Text("Cancel")))) &
        "tr [id]" #> nodeId & "tr [style]" #> "display:none"
    }


    <head_merge>
      <script type="text/javascript">
        function lift_hide_edit_links() {{jQuery(".liftedit_link_class_").hide()}}
        function lift_show_edit_links() {{jQuery(".liftedit_link_class_").show()}}
      </script>
    </head_merge> ++
    rowTransform(ns)
  }

  def viewRow(fields: Seq[Field]) =
    fields.map(_.viewFunc) reduceLeft {(fs, f) =>  fs & f}

  def editRow(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}
}

// fieldFunc = displayFunc,