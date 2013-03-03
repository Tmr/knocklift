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

  val noticeAlertClass = "alert alert-info"
  val warningAlertClass = "alert alert-block"
  val errorAlertClass = "alert alert-error"

  def wrapMsgsJsCmd(notice: Box[NoticeType.Value], id: String): Box[JsCmd] =
    notice.map(n => {
      val noticeNodeClass = n match {
        case NoticeType.Notice => noticeAlertClass
        case NoticeType.Warning => warningAlertClass
        case NoticeType.Error => errorAlertClass
      }
      //val wrapHtml = encJs(<div class={noticeNodeClass}></div>.toString())
      val closeButton = encJs(<button type="button" class="close" data-dismiss="alert">&times;</button>.toString())
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

trait AjaxEditableTable {

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

  object editForm extends RequestVar[Box[MemoizeTransform]](Empty)
  object editFormTemplate extends RequestVar[Box[NodeSeq]](Empty)

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
    (captureEditTemplateForm _) andThen
      rowSelector
  }

  def rowSelector: NodeSeq => NodeSeq

  def captureEditTemplateForm(ns:NodeSeq):NodeSeq = {
    editFormTemplate.set(Full(ns))
    ns
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

  def addRecordForm(addLinkSelector:String, okButtonSelector: String,
                    cancelButtonSelector: String, onSubmit:() => Box[JsCmd], renderErrorFn: (Failure, NodeSeq) => NodeSeq,
                    afterAddNodes: => NodeSeq, fields:List[Field])(ns:NodeSeq) = {
    val nodeId = nextFuncName
    var fn:NodeSeq => NodeSeq = null // dumb forward declaration
    def removeRowFn() =
      JqJE.JqId(nodeId) ~> JqJE.JqRemove() & showControls
    def wrappedOnSubmitFn: Box[JsCmd] =
      onSubmit() match {
        case Full(js) => Full(showControls & smoothReplace(nodeId, afterAddNodes) & js)
        case Empty => Full(showControls & smoothReplace(nodeId, afterAddNodes))
        case fail @ Failure(_,_,_) => {
          val re = renderErrorFn(fail, fn(ns))
          Full(smoothReplace(nodeId, re))
        }
      }

    fn = makeForm(nodeId, addLinkSelector, okButtonSelector,
      cancelButtonSelector, wrappedOnSubmitFn _, renderErrorFn, renderAfterSubmit = false, Full(removeRowFn _), fields) _
    val r = fn(ns)
    r
  }

  def makeForm(nodeId:String, editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
               onSubmit: () => Box[JsCmd], renderErrorFn: (Failure, NodeSeq) => NodeSeq, renderAfterSubmit: Boolean,
               onCancel:Box[() => JsCmd], fields:List[Field])(ns:NodeSeq): NodeSeq = {
    def renderViewRow:JsCmd =
      smoothReplace(nodeId, makeRowView(nodeId, editLinkSelector, okButtonSelector,
        cancelButtonSelector, onSubmit, renderErrorFn, onCancel, fields)(ns))

    var fn:NodeSeq => NodeSeq = null

    fn = editRowFn(fields) &
      okButtonSelector #>
        ajaxSubmitInputAttributes.foldLeft(
          SHtml.ajaxSubmit(ajaxSubmitInputText, () => {
            if (renderAfterSubmit)
              onSubmit() match {
                case Full(js) => showControls & renderViewRow & js
                case Empty => showControls & renderViewRow
                // If we have Failure (for example validation errors chain) we render row (renderAfterSubmit is true, else error rendering should be in onSubmit)
                // and send it to renderErrorFn function along with fail to possibly decorate error in rendered nodes
                case fail @ Failure(_,_,_) =>
                    smoothReplace(nodeId, renderErrorFn(fail, fn(ns)))
              }
            else onSubmit().openOr(Noop)
          }))(_ % _) &
      cancelButtonSelector #> ajaxCancelLinkAttributes.foldLeft(SHtml.a(onCancel.openOr(() => showControls & renderViewRow), ajaxCancelLinkNodes))(_ % _) &
      "tr [id]" #> nodeId &
      editLinkSelector #> NodeSeq.Empty
    val r = fn(ns)
    r
  }


  def makeRowView(nodeId:String, editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
                   onSubmit: () => Box[JsCmd], renderErrorFn: (Failure, NodeSeq) => NodeSeq, onCancel:Box[() => JsCmd], fields:List[Field])(ns:NodeSeq):NodeSeq = {
    val fn = viewRowFn(fields) &
      editLinkSelector #>
        {
          addEditClassName(
            SHtml.a(
              () => hideControls &
                    smoothReplace(nodeId, makeForm(nodeId, editLinkSelector, okButtonSelector, cancelButtonSelector,
                    onSubmit, renderErrorFn, renderAfterSubmit = true, onCancel, fields)(ns)),
              ajaxEditLinkNodes,
              ajaxEditLinkAttributes: _*)
          )
        } &
      "tr [id]" #> nodeId &
      okButtonSelector #> NodeSeq.Empty &
      cancelButtonSelector #> NodeSeq.Empty
    fn(ns)
  }

  def editable(editLinkSelector:String, okButtonSelector: String, cancelButtonSelector: String,
               onSubmit: () => Box[JsCmd], renderErrorFn: (Failure, NodeSeq) => NodeSeq, userOnCancel:Box[() => JsCmd], fields:List[Field])(ns:NodeSeq):NodeSeq = {
    val nodeId = nextFuncName
    def onCancel:JsCmd =
      userOnCancel.openOr(() => Noop)() &
      showControls &
      smoothReplace(nodeId, makeRowView(nodeId, editLinkSelector, okButtonSelector,
        cancelButtonSelector, onSubmit, renderErrorFn, Full(onCancel _), fields)(ns))

    makeRowView(nodeId, editLinkSelector, okButtonSelector, cancelButtonSelector,
      onSubmit, renderErrorFn, Full(onCancel _), fields) (ns)
  }

  val addEditClassName = "a [class+]" #> editLinkCSSClass

  def showControls:JsCmd =
    Call(showLinksFnName)

  def hideControls:JsCmd =
    Call(hideLinksFnName)

  def viewRowFn(fields: Seq[Field]) =
    fields.map(_.viewFunc) reduceLeft {(fs, f) =>  fs & f}

  def editRowFn(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}

  def addRowFn(fields: Seq[Field])=
    fields.map(_.editFunc) reduceLeft {(fs, f) =>  fs & f}


  def smoothReplace(nodeId:String, ns:NodeSeq) =
    JqJE.JqId(nodeId) ~> JsFunc("fadeOut", 100,
      AnonFunc(
        JsHideId(nodeId) &
          Replace(nodeId, ns) &
          JqJE.JqId(nodeId) ~> JsFunc("fadeIn", 100)))
}