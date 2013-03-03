package code.snippet

import net.liftweb._
import common.Full
import common.Full
import http._
import js.JsCmds._
import js.JsCmds.Run
import squerylrecord.RecordTypeMode._
import util._
import Helpers._
import common._
import js._
import xml._
import code.model.{MySchema, Country}

/**
 * Created with IntelliJ IDEA.
 * User: Tmt
 * Date: 02.03.13
 * Time: 16:36
 * To change this template use File | Settings | File Templates.
 */
class ValHolder[T](var v:T){}
object ValHolder {
  def apply[T](v:T) = new ValHolder(v)
}

object CountriesTable extends AjaxEditableTable {

  def rowSelector =
    "*" #> Country.getAllList().map(c => {
      editableRow(c)
    })

  def decorateError(failure: Failure, ns: NodeSeq): NodeSeq = {
    failure.failureChain.foreach(x => S.error(x.msg))
    val f =
      "tr [class]" #> "error this-is-popover" &
       "tr [title]" #> failure.failureChain.map(x => <div>{x.msg}</div>).mkString &
       "tr [data-html]" #>  "true"
    f(ns) ++ Script(Run("$('.tooltip').remove(); $('.this-is-popover').tooltip()"))
  }

  def editableRow(country:Country) = {
    var c = ValHolder(country)
    editable(
      "@edit", "@ok", "@cancel",
      () =>
        withValidation(c.v){
          MySchema.country.update(c.v)
          S.notice(Text("%s was updated".format(c.v.name.get)))
        },
      decorateError,
      Full(() => {
        c.v = from(MySchema.country)(cn => where(cn.id === c.v.id) select(cn)).headOption.getOrElse(Country.createRecord)
        Noop
      }),
        fieldsFor(c)
    ) _
  }

  def withValidation(c:Country)(cont: => JsCmd): Box[JsCmd] = {
    var faults:Box[JsCmd] = c.validate.foldLeft(Empty:Box[JsCmd])(_ ?~! _.msg.text)
    if (c.name.get.length < 2) faults = faults ?~! "Name is too short"
    if (c.seoUrl.get.length < 2) faults = faults ?~! "seoUrl is too small"
    faults match {
      case x:Failure => x
      case _ => Full(cont)
    }
  }

  def fieldsFor(c:ValHolder[Country]) ={
    List(Field("@name *" #> c.v.name, "@name *" #> SHtml.text(c.v.name.value, c.v.name.set(_))),
      Field("@seo-url *" #> c.v.seoUrl, "@seo-url *" #> SHtml.text(c.v.seoUrl.value, c.v.seoUrl.set(_))))
  }

  val createNew =
    makeAddForm {
      val c = Country.createRecord
      addRecordForm(
        "@add", "@ok", "@cancel",
        () => {
          withValidation(c){
            MySchema.country.insert(c)
            S.notice(Text("%s saved".format(c.name.get)))
        }},
        decorateError,
        editableRow(c)(editFormTemplate.get.openOr(NodeSeq.Empty)),
        fieldsFor(ValHolder(c))
      ) _
    }

  override def showControls =
    super.showControls & Run("$('.tooltip').remove();")


  override def hideControls =
    super.hideControls & Run("$('.tooltip').remove();")
}
