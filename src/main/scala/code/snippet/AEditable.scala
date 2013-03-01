package code.snippet

import net.liftweb._
import util._
import Helpers._
import http._
import js._
import js.JE._
import js.jquery.{JqJsCmds, JqJE}
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
//      """
//        | function(){
//        | if (jQuery('#%s').attr(""))
//        | jQuery('#%s').wrap(%s).parent().prepend(%s)
//        | }()
//      """.stripMargin
      //JsRaw(("jQuery('#%s').wrap(%s).parent().prepend(%s)").format(id, wrapHtml, closeButton))})
      JsRaw(("jQuery('#%s').addClass('%s').prepend(%s)").format(id, noticeNodeClass, closeButton))})

  def render(in:NodeSeq) = {
    LiftRules.noticesEffects.request.set(Vendor(BootstrapMsgs.wrapMsgsJsCmd _))
    NodeSeq.Empty
//    val msgs = S.eval(<lift:msgs></lift:msgs>).openOr(NodeSeq.Empty)
//    if (S.getAllNotices.length > 0) {
//      <div class="alert alert-block">
//        <button type="button" class="close" data-dismiss="alert">&times;</button>
//        {msgs}
//      </div>
//    } else msgs
  }
}

object FadeOutNotices {
  def render(ns:NodeSeq): NodeSeq = {
    LiftRules.noticesAutoFadeOut.request.set( (notices: NoticeType.Value) => {
      notices match {
        case NoticeType.Notice => Full((2 seconds, 2 seconds))
        case _ => Empty
      }
    }
    )
    NodeSeq.Empty
  }
}

object NoticeTest {
  def render(ns:NodeSeq):NodeSeq = {
    SHtml.a(() => {S.notice("Ajax notice"); S.warning("Ajax notice")}, Text("Ajax notice"))
  }
}

object AEditable {

  val editLinkCSSClass = "lift_edit_link_class_"
  val hideLinksFnName  = "lift_hide_edit_links_"
  val showLinksFnName  = "lift_show_edit_links_"

  val newRecordMarkerId = "lift_new_record_marker_id_"

  val ajaxEditLinkNodes = Text("Edit")
  val ajaxEditLinkAttributes:List[net.liftweb.http.SHtml.ElemAttr] = List(("class" -> "btn"))
  val ajaxSubmitInputText = "Ok"
  val ajaxSubmitInputAttributes = List(("class" -> "btn btn-danger"))
  val ajaxCancelLinkNodes = Text("Cancel")
  val ajaxCancelLinkAttributes = List(("class" -> "btn"))

  val addButtonAttributes:List[net.liftweb.http.SHtml.ElemAttr] = List(("class" -> "btn"))

  val editModeClass = "info"

  case class Field(viewFunc: CssSel, editFunc: CssSel)

  val population =
    new Human("Adam", 930) ::
    new Human("Eva", 450)  ::
    Nil

  object dataVar extends SessionVar(population)
  object editForm extends RequestVar[Box[MemoizeTransform]](Empty)

  def table(ns:NodeSeq) =
      <head_merge>
        <script type="text/javascript">
          function {hideLinksFnName}() {{jQuery(".{editLinkCSSClass}").fadeOut(100)}}
          function {showLinksFnName}() {{jQuery(".{editLinkCSSClass}").fadeIn(100)}}
        </script>
      </head_merge> ++
//    <head_merge>
//      {Script(
//      Function(hideLinksFnName, Nil, JqJE.Jq("."+editLinkCSSClass) ~> JsFunc("fadeOut", 100)) &
//        Function(showLinksFnName, Nil, JqJE.Jq("."+editLinkCSSClass) ~> JsFunc("fadeIn", 100))
//    )}
//    </head_merge> ++
    SHtml.ajaxForm(ns)

  def editableList = {
    "*" #> dataVar.get.map(h => {
      editableRow(h)
    })
  }

  def editableRow(h:Human) =
    editable(
      "@edit", "@ok", "@cancel",
      () => S.notice(Text("Value was updated")),
      fieldsFor(h): _*
    )

  def fieldsFor(h:Human) =
    List(Field("@name *" #> h.name, "@name *" #> SHtml.text(h.name, h.name = _)),
         Field("@age *" #> h.age, "@age *" #> SHtml.number(h.age, h.age = _, 0, 1000)),
         Field("@was-expelled *" #> (if (h.wasExpelled) "Yes" else "No"),
               "@was-expelled *" #> SHtml.checkbox(h.wasExpelled, h.wasExpelled = _)),
         Field("@in-paradise *" #> (if (h.inParadise) "Yes" else "No"),
               "@in-paradise *" #> (if (h.inParadise) "Yes" else "No")))

  val createNew =
    makeAddForm {
      val h = new Human("Homunculus", 0)
      addRecordForm(
        "@add", "@ok", "@cancel",
        () => {dataVar.set(dataVar.get ++ List(h));S.notice(Text("Value was added"))},
        editableRow(h)()
        fieldsFor(h): _*
      )
    }

  def addButton(ns:NodeSeq) = {
    val a = SHtml.a(() => {
      editForm.map(form => {
        hideControls &
        JsRaw("jQuery('#%s').before(%s)".format(newRecordMarkerId, encJs(form.applyAgain().toString()))):JsCmd
      }).openOr(Noop)
    }, ns)
    addEditClassName(addButtonAttributes.foldLeft(a)(_ % _))
  }

  def makeAddForm(createNewFrom: => (NodeSeq => NodeSeq)):NodeSeq => NodeSeq = {
    (ns) => {
      editForm.set(Full(SHtml.memoize{createNewFrom}))  // we do not evaluate createNewFrom here (it's so lazy and cool)
      editForm.get.foreach(_.apply(ns)) // createNewFrom is evaluated here and in each editForm.get.applyAgain() call
      <tr id={newRecordMarkerId}></tr>
    }
  }

  def smoothReplace(nodeId:String, ns:NodeSeq) =
    JqJE.JqId(nodeId) ~> JsFunc("fadeOut", 100,
      AnonFunc(
        JsHideId(nodeId) &
        Replace(nodeId, ns) &
        JqJE.JqId(nodeId) ~> JsFunc("fadeIn", 100)))


  def addRecordForm(addLinkSelector:String, okButtonSelector: String,
                    cancelButtonSelector: String, onSubmit:() => JsCmd, afterAddNodes:NodeSeq, fields:Field*) = {
    val nodeId = nextFuncName
    def removeRowFn =
      JqJE.JqId(nodeId) ~> JqJE.JqRemove() & showControls
    makeForm(nodeId, addLinkSelector, okButtonSelector,
      cancelButtonSelector, onSubmit, Full(removeRowFn _), fields: _*) _
  }

  def makeRowView(nodeId:String, editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
                   onSubmit: () => JsCmd, fields:Field*)(ns:NodeSeq):NodeSeq = {
    val fn = viewRowFn(fields) &
      editLinkSelector #>
        {
          addEditClassName(
            SHtml.a(
              () => hideControls &
                    smoothReplace(nodeId, makeForm(nodeId, editLinkSelector, okButtonSelector, cancelButtonSelector,
                    onSubmit, Empty, fields: _*)(ns)),
              ajaxEditLinkNodes,
              ajaxEditLinkAttributes: _*)
          )
        } &
      "tr [id]" #> nodeId &
      okButtonSelector #> NodeSeq.Empty &
      cancelButtonSelector #> NodeSeq.Empty
    fn(ns)
  }

  def makeForm(nodeId:String, editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
               onSubmit: () => JsCmd, onCancel:Box[() => JsCmd], fields:Field*)(ns:NodeSeq): NodeSeq = {
    val renderViewRow = () =>
        showControls &
        smoothReplace(nodeId, makeRowView(nodeId, editLinkSelector, okButtonSelector, cancelButtonSelector,
          onSubmit, fields: _*)(ns))

    val fn = editRowFn(fields) &
      okButtonSelector #>
        ajaxSubmitInputAttributes.foldLeft(
          SHtml.ajaxSubmit(ajaxSubmitInputText, () => {
            renderViewRow() & onSubmit()
          }))(_ % _) &
      cancelButtonSelector #> ajaxCancelLinkAttributes.foldLeft(SHtml.a(onCancel.openOr(renderViewRow), ajaxCancelLinkNodes))(_ % _) &
      "tr [id]" #> nodeId &
      editLinkSelector #> NodeSeq.Empty
    fn(ns)
  }

  def editable(editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
               onSubmit: () => JsCmd, fields:Field*):NodeSeq => NodeSeq =
    makeRowView(nextFuncName, editLinkSelector, okButtonSelector, cancelButtonSelector,
      onSubmit, fields: _*)

  val addEditClassName = "a [class+]" #> editLinkCSSClass

  val showControls:JsCmd =
    Call(showLinksFnName)

  val hideControls:JsCmd =
    Call(hideLinksFnName)

  def viewRowFn(fields: Seq[Field]) =
    fields.map(_.viewFunc) reduceLeft {(fs, f) =>  fs & f}

  def editRowFn(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}

  def addRowFn(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}
}