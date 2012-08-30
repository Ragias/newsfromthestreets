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
                  d.setMode(!d.mode.is)
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

class AddSearchGroupSnippet {

  var qParam = S.param("q")

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    var name = ""
    var description = ""
    User.currentUser.map {
      u =>
        Detective.findByUser(u).map {
          d =>
            if (qParam.getOrElse("") == "add") {
              out = ("name=name" #> SHtml.text(name, name = _, "id" -> "the_name") &
                "name=description" #> SHtml.textarea(description, description = _, "id" -> "the_description") &
                "type=submit" #> SHtml.onSubmitUnit(() => {
                  SearchGroup.add(d, name, description).map {
                    group =>
                      S.redirectTo("/searchgroup?q=show&id=" + group.id.is.toString)
                  }

                }))(in)
            }
        }
    }

    out
  }
}

class ShowSearchGroup {

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    if (S.param("q").getOrElse("") == "show") {
      S.param("id").map {
        id =>
          SearchGroup.find(id).map {
            group =>
              out = <div id="showGroup">
                      <span id="groupName"> { group.name.is }</span>
                      <span id="groupDescription">{ group.description.is } </span>
                      <span id="numberOfDetectives">{ group.num.is } </span>
                      <span id="admin">{
                        group.admin_id.obj.map {
                          d =>
                            d.user_id.obj.map {
                              u =>
                                u.username.is
                            }.getOrElse("")
                        }.getOrElse("")
                      } </span>
                    </div>
          }
      }
    }
    out
  }
}
class ListOfSearchGroupYouAreAdmin {
  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    for {
      u <- User.currentUser
      d <- u.getDetective
    } yield {
      out = <ul id="listOfSearchGroupYouAreAdmin">
              {
                SearchGroup.findByAdmin(d).map {
                  group =>
                    <span id="groupName"> { group.name.is }</span> ++
                      <span id="groupDescription">{ group.description.is } </span> ++
                      <span id="numberOfDetectives">{ group.num.is } </span> ++
                      SHtml.ajaxButton(Text("Delete"), () => {
                        group.delete()
                        Reload
                      })
                }
              }
            </ul>
    }
    out
  }
}

class ListOfSearchGroupYouAreNotIn {

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    for {
      u <- User.currentUser
      d <- u.getDetective
    } yield {
      out = <ul id="listOfSearchGroupYouAreNotIn">{

        SearchGroup.findByDetectiveNotIn(d).map {
          sg =>
            <li>
              <span id="name">{ sg.name.is } </span><br/>
              <span id="admin">{ sg.getAdminUsername() } </span>
              <span id="num"> { sg.num.is } </span>
              {
                <span id="addMe"> {

                  SHtml.ajaxButton(Text("Add me"), () => {
                    DetectiveInGroup.add(d, sg, false)
                    Reload
                  })

                }</span>

              }
            </li>
        }
      }</ul>
    }
    out
  }
}

class ListOfDetectiveInGroup {
  def render(in: NodeSeq): NodeSeq = {
    var out = <span></span>
    User.currentUser.map {
      user =>
        user.getDetective.map {
          detective =>
            out = <ul id="listOfDetectiveInGroups">{
              DetectiveInGroup.findByDetective(detective).map {
                ding =>
                  <li>
                    {
                      ding.searchgroup_id.obj.map {
                        sg =>

                          <span id="name">{ sg.name.is } </span> ++
                            <span id="admin">{ sg.getAdminUsername() } </span> ++
                            <span id="num"> { sg.num.is } </span>

                      }.getOrElse(<span></span>) ++
                        <span id="request"> {
                          if (ding.request.is) {
                            SHtml.ajaxButton(Text("Your request is proceeding do you want to cancel "), () => {
                              ding.delete()
                              Reload
                            })
                          } else {
                            SHtml.ajaxButton(Text("Get out!"), () => {
                              ding.delete()
                              Reload
                            })
                          }
                        }</span>

                    }
                  </li>
              }
            }</ul>
        }
    }
    out
  }
}

class AdminSearchGroup extends StatefulSnippet with Loggable {
  var id = S.param("id").getOrElse("")
  var q = S.param("q").getOrElse("")
  var name = ""
  var description = ""
  var searchGroup = SearchGroup.find(id)

  def listOfAddedDetectives(sg: SearchGroup): NodeSeq = {

    <ul id="listOfAddedDetectives">{
      DetectiveInGroup.findBySearchGroupAndRequest(sg, true).map {
        dig =>
          if (dig.detective_id.is != sg.admin_id.is) {
            <li>
              <span id="username">{ dig.getDetectiveUserName() } </span>
              {
                SHtml.ajaxButton(Text("Block"), () => {
                  dig.blockTheUser()
                  Reload
                })
              }
            </li>
          }

      }
    }</ul>

  }

  def listOfRequests(sg: SearchGroup): NodeSeq = {

    <ul id="listOfRequests">{
      DetectiveInGroup.findBySearchGroupAndRequest(sg, false).map {
        dig =>
          <li>
            <span id="username">{ dig.getDetectiveUserName() } </span>
            {
              SHtml.ajaxButton(Text("Accept"), () => {
                dig.acceptRequest()
                Reload
              })
            }
          </li>

      }
    }</ul>

  }

  def dispatch = { case "render" => render }

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    if (q == "edit") {
      for {
        user <- User.currentUser
        detective <- user.getDetective
        sg <- searchGroup
      } yield {
        if (sg.admin_id.toString() == detective.id.is.toString()) {
          name = sg.name.is
          description = sg.description.is
          out = listOfAddedDetectives(sg) ++
            <br/> ++
            listOfRequests(sg) ++
            <br/> ++
            ("name=name" #> SHtml.text(name, name = _, "id" -> "the_name") &
              "name=description" #> SHtml.textarea(description, description = _, "id" -> "the_description") &
              "type=submit" #> SHtml.onSubmitUnit(() => {
                SearchGroup.edit(sg, name, description)
                S.redirectTo("/searchgroup?q=show&id=" + sg.id.is.toString)

              }))(in)
        }
      }
    }

    out
  }

}

