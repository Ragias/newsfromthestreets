package org.newsfromthestreets.snippet
import net.liftweb._
import http._
import util._
import common._
import Helpers._
import org.newsfromthestreets.model._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JE._

class DetectiveModeSnippet {
  def twoMoves(isDetective: Boolean): JsCmd = {
    if (isDetective) {
      SetHtml("labelMode", <span id="labelMode"> You are a Detective , press submit button to stop </span>) &
        SetHtml("detectiveMap", <div id="map_canvas" style="width: 500px; height: 300px"></div>) &
        SHtml.ajaxInvoke(() => {
          JsRaw("initialize()")
        }) &
        Noop
     
    } else {
       SetHtml("labelMode", <span id="labelMode"> You are not a Detective , press submit button to start </span>) &
        SetHtml("detectiveMap", <span id="detectiveMap"></span>) &
        Noop
    }
  }
  def detectiveButton(): JsCmd = {
    var out: NodeSeq = <span id="detectiveMode"></span>
    var start: Box[Boolean] = Empty
    User.currentUser.map {
      u =>
        Detective.findByUser(u) match {
          case Full(d) => {
            start = Full(d.mode.is)
            out =
              <span id="detectiveMode">{
                SHtml.ajaxButton(Text("Submit"), () => {
                  Detective.setMode(d, !d.mode.is)
                  Reload
                })
              } </span>

          }
          case _ => {

            out = <span id="detectiveMode">{
              SHtml.ajaxButton(Text("Be Detective"), () => {
                Detective.addUser(u)
                Reload
              })
            } </span>
          }
        }

    }

    start match {
      case Full(d) => twoMoves(d) & SetHtml("detectiveMode", out)
      case _ => SetHtml("detectiveMode", out)
    }

  }
  def render(in: NodeSeq): NodeSeq = {
    Script(OnLoad(detectiveButton()))
  }
}


class SearchGroupSnippet extends LiftScreen{
  val name = text("Name the group", "")
  val description = textarea("Description of the group","")
  
  def finish(){
    User.currentUser.map{
      u => 
            SearchGroup.add(u,name,description)

    }
  }
  
}