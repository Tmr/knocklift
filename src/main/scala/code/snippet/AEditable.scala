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
import common._

/**
 * Created with IntelliJ IDEA.
 * User: bobrus
 * Date: 25.02.13
 * Time: 12:00
 * To change this template use File | Settings | File Templates.
 */

//class Human(var name:String, var age:Int)

object BootstrapMsgs {

  def wrapMsgsJsCmd(notice: Box[NoticeType.Value], id: String): Box[JsCmd] =
    notice.map(n => {
      val noticeNodeClass = n match {
        case NoticeType.Notice => "alert alert-info"
        case NoticeType.Warning => "alert alert-block"
        case NoticeType.Error => "alert alert-error"
      }
      val wrapHtml = encJs(<div class={noticeNodeClass}></div>.toString())
      val closeButton = encJs(<button type="button" class="close" data-dismiss="alert">&times;</button>.toString())
      JsRaw("jQuery('#%s').wrap(%s).parent().prepend(%s)".format(id, wrapHtml, closeButton))

    }


//      AnonFunc(
//        JqJE.JqId(id) ~> JqJE.JqHtml(
//          JsCrVar("h", encJs(<div class="alert alert-block">{{0}}</div>.text)) &
//          JsVar("h") ~> JsFunc("format", JqJE.JqId(id) ~> JqJE.JqHtml())
//        )
//      )
    )


  def render(in:NodeSeq) = {
    val msgs = S.eval(<lift:msgs></lift:msgs>).openOr(NodeSeq.Empty)
    if (S.getAllNotices.length > 0) {
      <div class="alert alert-block">
        <button type="button" class="close" data-dismiss="alert">&times;</button>
        {msgs}
      </div>
    } else msgs
  }
}

object AEditable {

  val editLinkCSSClass = "liftedit_link_class_"
  val hideLinksFnName  = "lift_hide_edit_links_"
  val showLinksFnName  = "lift_show_edit_links_"


  val ajaxEditLinkNodes = Text("Edit")
  val ajaxEditLinkAttributes:List[net.liftweb.http.SHtml.ElemAttr] = List(("class" -> "btn"))
  val ajaxSubmitInputText = "Ok"
  val ajaxSubmitInputAttributes = List(("class" -> "btn btn-danger"))
  val ajaxCancelLinkNodes = Text("Cancel")
  val ajaxCancelLinkAttributes = List(("class" -> "btn"))

  val editModeClass = "info"

  case class Field(viewFunc: CssSel, editFunc: CssSel)

  val population =
    new Human("Adam", 930) ::
    new Human("Eva", 450)  ::
    Nil

  object dataVar extends SessionVar(population)

  def table(ns:NodeSeq) =
  //    <head_merge>
  //      <script type="text/javascript">
  //        function lift_hide_edit_links() {{jQuery("liftedit_link_class_").hide()}}
  //        function lift_show_edit_links() {{jQuery(".liftedit_link_class_").show()}}
  //      </script>
  //    </head_merge> ++
    <head_merge>
      {Script(
      Function(hideLinksFnName, Nil, JqJE.Jq("."+editLinkCSSClass) ~> JsFunc("fadeOut", 100)) &
        Function(showLinksFnName, Nil, JqJE.Jq("."+editLinkCSSClass) ~> JsFunc("fadeIn", 100))
    )}
    </head_merge> ++
    SHtml.ajaxForm(ns)

  def editableList = {
    LiftRules.noticesEffects.request.set(Vendor(BootstrapMsgs.wrapMsgsJsCmd _))
    "*" #> dataVar.get.map(h =>
      editable(
        "@edit", "@ok", "@cancel",
        () => {
          S.notice(Text("Value is updated"))
          S.notice(Text("Value is updated 2"))
          S.warning(Text("Value is updated"))
          S.error(Text("Value is updated"))
        },
        Field("@name *" #> h.name, "@name *" #> SHtml.text(h.name, h.name = _)),
        Field("@age *" #> h.age, "@age *" #> SHtml.number(h.age, h.age = _, 0, 1000)),
        Field("@was-expelled *" #> (if (h.wasExpelled) "Yes" else "No"),
              "@was-expelled *" #> SHtml.checkbox(h.wasExpelled, h.wasExpelled = _)),
        Field("@in-paradise *" #> (if (h.inParadise) "Yes" else "No"),
              "@in-paradise *" #> (if (h.inParadise) "Yes" else "No"))
      ) _
    )
  }

  def smoothReplace(nodeId:String, ns:NodeSeq) =
    JqJE.JqId(nodeId) ~> JsFunc("fadeOut", 100,
      AnonFunc(
        JsHideId(nodeId) &
        Replace(nodeId, ns) &
        JqJE.JqId(nodeId) ~> JsFunc("fadeIn", 100)))


  def editable(editLinkSelector:String, okButtonSelecor: String, cancelButtonSelecor: String,
               onSubmit:() => JsCmd, fields:Field*)(ns:NodeSeq): NodeSeq = {

    val nodeId = nextFuncName

    var makeForm: CssSel = null // dumb forward declaration
    var rowTransform: CssSel = null // dumb forward declaration

    def renderRow() =
      Call(showLinksFnName) &
      smoothReplace(nodeId, rowTransform(ns))

    val addEditClassName = "a [class+]" #> editLinkCSSClass

    rowTransform = viewRow(fields) &
      editLinkSelector #>
        {
          addEditClassName(
            SHtml.a(
              () => Call(hideLinksFnName) & smoothReplace(nodeId, makeForm(ns)),
              ajaxEditLinkNodes,
              ajaxEditLinkAttributes: _*)
          )
        } &
      "tr [id]" #> nodeId &
      okButtonSelecor #> NodeSeq.Empty &
      cancelButtonSelecor #> NodeSeq.Empty

    makeForm = {
      editRow(fields) &
        okButtonSelecor #>
            ajaxSubmitInputAttributes.foldLeft(
              SHtml.ajaxSubmit(ajaxSubmitInputText, () => {
                renderRow & onSubmit()
              }))(_ % _) &
        cancelButtonSelecor #> ajaxCancelLinkAttributes.foldLeft(SHtml.a(renderRow _, ajaxCancelLinkNodes))(_ % _) &
        "tr [id]" #> nodeId &
        "tr [style]" #> "display:none" &
        editLinkSelector #> NodeSeq.Empty
    }
    rowTransform(ns)
  }

  def viewRow(fields: Seq[Field]) =
    fields.map(_.viewFunc) reduceLeft {(fs, f) =>  fs & f}

  def editRow(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}
}