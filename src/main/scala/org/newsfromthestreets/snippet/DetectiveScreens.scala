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

class SelectSearchGroup extends StatefulSnippet {
  var id = ""
  var message = ""
  def listMessages(): NodeSeq = {
    var xhtml: NodeSeq = <span></span>

    for {
      sg <- SearchGroup.find(id)
    } yield {
      xhtml = <ul id="listOfMessages">
                {
                  sg.showMessagesByNumberOfResults(10).map {
                    message =>
                      <li>
                        {
                          <span> { message.getUsername() } </span>
                          <span> { message.message.is } </span>
                        }
                      </li>
                  }
                }
              </ul>

    }

    xhtml
  }
  def dispatch = {
    case "render" => render
  }
  def showMessages(): JsCmd = {
    SetHtml("listOfMessages", listMessages)
  }

  def sendMessages: JsCmd = {
    var out: NodeSeq = <span></span>
    for {
      user <- User.currentUser
      detective <- user.getDetective
      sg <- SearchGroup.find(id)
      dig <- DetectiveInGroup.findByDetectiveAndGroup(detective, sg)
    } yield {
      println("REEEEEEEEEEEE!")
      if (dig.request.is == true) {
        out = SHtml.ajaxText("", s => {
          message = s
        }, "id" -> "message") ++ SHtml.ajaxButton(Text("Send"), () => {
          SearchGroupMessage.add(sg, detective, message)
          showMessages() &
            SetValById("message", "")
        })
      }
    }

    SetHtml("sendMessages", out)

  }

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    for {
      user <- User.currentUser
      detective <- user.getDetective
    } yield {
      if (detective.mode.is == true) {
        val ls = DetectiveInGroup.findByDetective(detective).map {
          dig =>
            (dig.searchgroup_id.toString, dig.getGroupName())
        }.toSeq ++ Seq(("None", "None"))

        out = ("name=searchGroup" #> SHtml.ajaxSelect(ls, Full("None"), s => {
          if (s == "None") {
            DetectiveInGroup.findByDetective(detective).foreach(_.changeMode(false))
          } else {
            SearchGroup.find(s).map {
              sg =>
                DetectiveInGroup.findByDetectiveAndGroup(detective, sg).map {
                  dig =>
                    dig.changeModeToTrue
                    id = dig.searchgroup_id.toString()
                }
            }

          }

          showMessages &
            sendMessages
        }))(in)
      }
    }
    out
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
                      <div id="map_canvas" style="width: 300px; height: 300px"></div>
                      {
                        <span id="groupName"> { group.name.is }</span>
                        <br/>
                        <span id="groupDescription">{ group.description.is } </span>
                        <span id="numberOfDetectives">{ group.num.is } </span>
                        <span id="admin">{
                          group.getAdminUsername()
                        } </span> ++ {
                          for {
                            user <- User.currentUser
                            d <- user.getDetective
                          } yield {
                            DetectiveInGroup.findByDetectiveAndGroup(d, group).map {
                              ding =>
                                <span id="request"> {
                                  if (ding.request.is == true && ding.blocked.is == false) {
                                    SHtml.ajaxButton(Text("Get out!"), () => {
                                      ding.delete()
                                      Reload
                                    })
                                  } else if (ding.request.is == false && ding.blocked.is == false) {

                                    SHtml.ajaxButton(Text("Your request is proceeding do you want to cancel ?"), () => {
                                      ding.delete()
                                      Reload
                                    })
                                  } else {
                                    <span>You have been blocked</span>
                                  }
                                }</span>

                            }.getOrElse {
                              SHtml.ajaxButton(Text("Add me"), () => {
                                DetectiveInGroup.add(d, group, false)
                                Reload
                              })
                            }
                          }
                        }
                      }
                    </div>
          }
      }
    }
    out
  }
}
class ListOfSearchGroupYouAreAdmin {
  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = NodeSeq.Empty
    for {
      u <- User.currentUser
      d <- u.getDetective
    } yield {
      out = <ul id="listOfSearchGroupYouAreAdmin">
              {
                SearchGroup.findByAdmin(d).map {
                  group =>
                    <li>{
                      <a id="groupName" href={ "/searchgroup?q=show&id=" + group.id.is.toString() }>{
                        group.name.is
                      } </a> ++ <br/> ++
                        <span id="groupDescription">{ group.description.is } </span> ++
                        <span id="numberOfDetectives">{ group.num.is } </span> ++
                        SHtml.ajaxButton(Text("Edit"), () => {
                          S.redirectTo("/searchgroup?q=edit&id=" + group.id.is.toString())
                          Reload
                        }) ++
                        SHtml.ajaxButton(Text("Delete"), () => {
                          group.delete()
                          Reload
                        })
                    }</li>
                }
              }
            </ul>
    }
    out
  }
}

class ListOfSearchGroupYouAreNotIn {

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = NodeSeq.Empty
    for {
      u <- User.currentUser
      d <- u.getDetective
    } yield {
      out = <ul id="listOfSearchGroupYouAreNotIn">{

        SearchGroup.findByDetectiveNotIn(d).map {
          sg =>
            <li>
              <a id="name" href={ "/searchgroup?q=show&id=" + sg.id.is.toString() }>{ sg.name.is } </a><br/>
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

class ListOfDetectiveInGroup extends Loggable {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    User.currentUser.map {
      user =>
        user.getDetective.map {
          detective =>
            out = <ul id="listOfDetectiveInGroups">{
              DetectiveInGroup.findByDetective(detective).filter {
                din => din.isDetectiveAdmin == false
              }.map {
                ding =>
                  <li>
                    {
                      ding.searchgroup_id.obj.map {
                        sg =>

                          <a id="name" href={ "/searchgroup?q=show&id=" + sg.id.is.toString() }>{ sg.name.is } </a> ++
                            <br/> ++
                            <span id="admin">{ sg.getAdminUsername() } </span> ++
                            <span id="num"> { sg.num.is } </span>

                      }.getOrElse(<span></span>) ++
                        <span id="request"> {
                          if (ding.request.is == true && ding.blocked.is == false) {
                            SHtml.ajaxButton(Text("Get out!"), () => {
                              ding.delete()
                              Reload
                            })
                          } else if (ding.request.is == false && ding.blocked.is == false) {

                            SHtml.ajaxButton(Text("Your request is proceeding do you want to cancel ?"), () => {
                              ding.delete()
                              Reload
                            })
                          } else {
                            <span>You have been blocked</span>
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
                }) ++
                  SHtml.ajaxButton(Text("Delete"), () => {
                    dig.delete()
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
              if (dig.blocked.is) {
                SHtml.ajaxButton(Text("Unblock"), () => {
                  dig.unblockTheUser()
                  Reload
                })
              } else {
                SHtml.ajaxButton(Text("Accept"), () => {
                  dig.acceptRequest()
                  Reload
                })
              }
            }
          </li>

      }
    }</ul>

  }

  def dispatch = { case "render" => render }

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = NodeSeq.Empty
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
                sg.edit(name, description)
                S.redirectTo("/searchgroup?q=show&id=" + sg.id.is.toString)

              }))(in)
        }
      }
    }

    out
  }

}

class SearchGroupMessenger extends StatefulSnippet {
  private var message = ""
  private var id = S.param("id").getOrElse("")
  private var num = S.param("num").getOrElse("")
  private var q = S.param("q").getOrElse("")
  def dispatch = {
    case "render" => render
  }

  def listMessages: NodeSeq = {
    var xhtml: NodeSeq = NodeSeq.Empty
    if (q == "show") {
      for {
        sg <- SearchGroup.find(id)
      } yield {
        xhtml = <ul id="listOfMessages">
                  {
                    sg.showMessagesByNumberOfResults(asInt(num).getOrElse(10)).map {
                      message =>
                        <li>
                          {
                            <span> { message.getUsername() } </span>
                            <span> { message.message.is } </span>
                          }
                        </li>
                    }
                  }
                </ul>
      }
    }

    xhtml
  }

  def showMessages(): JsCmd = {
    SetHtml("listOfMessages", listMessages)
  }

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    if (q == "show") {
      for {
        user <- User.currentUser
        detective <- user.getDetective
        sg <- SearchGroup.find(id)
        dig <- DetectiveInGroup.findByDetectiveAndGroup(detective, sg)
      } yield {
        if (dig.request.is == true) {
          out = (
            "#message" #> SHtml.ajaxText("", s => {
              message = s
            }) &
            "#send" #> SHtml.ajaxButton(Text("Send"), () => {
              SearchGroupMessage.add(sg, detective, message)
              showMessages() &
                SetValById("message", "")
            }))(in)
        }
      }
    }
    <div>
      { Script(OnLoad(showMessages)) ++ out }
    </div>
  }
}

class ShowActiveGroups {
  def render = <ul id="listOfActiveGroups">
                 {
		  			SearchGroup.findActiveGroups().map{
		  			  sg =>  <li>{
		  			     <a id="name" href={ "/searchgroup?q=show&id=" + sg.id.is.toString() }>{ sg.name.is } </a> ++
                            <br/> ++
                            <span id="admin">{ sg.getAdminUsername() } </span> ++
                            <span id="num"> { sg.num.is } </span>
		  			  }</li>
		  			}
                 }
               </ul>
}

