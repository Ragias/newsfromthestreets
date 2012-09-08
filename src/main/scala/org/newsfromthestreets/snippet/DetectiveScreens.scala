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
import org.newsfromthestreets.comet._
import com.fmpwizard.cometactor.pertab.namedactor.InsertNamedComet
import com.fmpwizard.cometactor.pertab.namedactor.CometListerner
import net.liftweb.actor.LiftActor



class SelectSearchGroup extends StatefulSnippet {
  var id = ""
  var message = ""
  def listMessages(): NodeSeq = {
    var xhtml: NodeSeq = <span></span>

    for {
      sg <- SearchGroup.find(id)
    } yield {
      xhtml = <ul id="listOfGroupMessages">
              </ul>

    }

    xhtml
  }
  def dispatch = {
    case "render" => render
  }
  def showMessages(): JsCmd = {
    SetHtml("listOfGroupMessages", listMessages)
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

        out = ("name=searchGroup" #>
          SHtml.ajaxSelect(ls, Full("None"), s => {
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
          })).apply(in)
      }
    }
    out
  }
}

class AddButtonSearchGroup {
  def render(in: NodeSeq): NodeSeq = {
    SHtml.ajaxButton(Text("Add group"), () => {
      S.redirectTo("/searchgroup?q=add")
    })
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
              out = (
                "#groupName *" #> group.name.is &
                "#groupName [href]" #> { "/searchgroup?q=show&id=" + group.id.is.toString() } &
                "#groupDescription *" #> group.description.is &
                "#numberOfDetectives *" #> group.num.is &
                "#admin *" #> group.getAdminUsername())(in)
              for {
                user <- User.currentUser
                d <- user.getDetective
              } yield {
                DetectiveInGroup.findByDetectiveAndGroup(d, group).map {
                  ding =>

                    if (ding.request.is == true && ding.blocked.is == false) {
                      out = out ++ ("#request" #> SHtml.ajaxButton(Text("Get out!"), () => {
                        ding.delete()
                        Reload
                      })).apply(in)
                    } else if (ding.request.is == false && ding.blocked.is == false) {

                      out = out ++ ("#request" #> SHtml.ajaxButton(Text("Your request is proceeding do you want to cancel ?"), () => {
                        ding.delete()
                        Reload
                      })).apply(in)
                    } else {
                      out = out ++ <span>You have been blocked</span>
                    }

                }.getOrElse {
                  out = out ++ ("#request" #>
                    SHtml.ajaxButton(Text("Add me"), () => {
                      DetectiveInGroup.add(d, group, false)
                      Reload
                    })).apply(in)
                }
              }
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
      out = ("#listOfSearchGroupYouAreAdmin" #> SearchGroup.findByAdmin(d).map {
        group =>
          ("li" #> {
            ("#groupName *" #> group.name.is &
              "#groupName [href]" #> { "/searchgroup?q=show&id=" + group.id.is.toString() } &
              "#groupDescription *" #> group.description.is &
              "#numberOfDetectives *" #> group.num.is)(in) ++
              SHtml.ajaxButton(Text("Edit"), () => {
                S.redirectTo("/searchgroup?q=edit&id=" + group.id.is.toString())
                Reload
              }) ++
              SHtml.ajaxButton(Text("Delete"), () => {
                group.delete()
                Reload
              })

          }).apply(in)
      }).apply(in)
    }
    out
  }
}

class ShowActiveGroups {
  def render(in: NodeSeq): NodeSeq = ("#listOfActiveGroups *" #>
    {
      SearchGroup.findActiveGroups().map {
        sg =>
          ("li" #>
            {
              ("#groupName *" #> sg.name.is &
                "#groupName [href]" #> { "/searchgroup?q=show&id=" + sg.id.is.toString() } &
                "#groupDescription *" #> sg.description.is &
                "#numberOfDetectives *" #> sg.num.is)(in)
            }).apply(in)
      }
    }).apply(in)
}

class ListOfGroupsUserIsNotMember {

  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = NodeSeq.Empty
    for {
      u <- User.currentUser
    } yield {
      out = ("#listOfGroupsUserIsNotMember *" #> {
        u.getDetective.map {
          d =>
            SearchGroup.findByDetectiveAndRequest(d,false).map {
              sg =>
                ("li" #>
                  {
                    ("#groupName *" #> sg.name.is &
                      "#groupName [href]" #> { "/searchgroup?q=show&id=" + sg.id.is.toString() } &
                      "#groupDescription *" #> sg.description.is &
                      "#numberOfDetectives *" #> sg.num.is)(in) ++
                      SHtml.ajaxButton(Text("Add me"), () => {
                        DetectiveInGroup.add(d, sg, false)
                        Reload
                      }, "id" -> "addMe")

                  }).apply(in)
            }
        }.getOrElse {
          SearchGroup.findAll.map {
            sg =>
              ("li" #>
                {
                  ("#groupName *" #> sg.name.is &
                    "#groupName [href]" #> { "/searchgroup?q=show&id=" + sg.id.is.toString() } &
                    "#groupDescription *" #> sg.description.is &
                    "#numberOfDetectives *" #> sg.num.is)(in)
                }).apply(in)
          }
        }
      }).apply(in)
    }
    out
  }
}

class ListOfGroupsUserIsMember{
  def render(in:NodeSeq) ={
     var out: NodeSeq = NodeSeq.Empty
    for {
      u <- User.currentUser
    } yield {
      out = ("#listOfGroupsUserIsMember *" #> {
        u.getDetective.map {
          d =>
            SearchGroup.findByDetectiveAndRequest(d,true).map {
              sg =>
                ("li" #>
                  {
                    ("#groupName *" #> sg.name.is &
                      "#groupName [href]" #> { "/searchgroup?q=show&id=" + sg.id.is.toString() } &
                      "#groupDescription *" #> sg.description.is &
                      "#admin *" #> sg.getAdminUsername() &
                      "#numberOfDetectives *" #> sg.num.is).apply(in) 

                  }).apply(in)
            }
        }.getOrElse{NodeSeq.Empty}
      }).apply(in)
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
            out = ("#listOfDetectiveInGroups *" #>
              DetectiveInGroup.findByDetective(detective).filter {
                din => din.isDetectiveAdmin == false
              }.map {
                ding =>
                  ("li" #>

                    ding.searchgroup_id.obj.map {
                      sg =>

                        ("#groupName *" #> sg.name.is &
                          "#groupName [href]" #> { "/searchgroup?q=show&id=" + sg.id.is.toString() } &
                          "#groupDescription *" #> sg.description.is &
                          "#numberOfDetectives *" #> sg.num.is &
                          {
                            if (ding.request.is == true && ding.blocked.is == false) {
                              "#request" #> SHtml.ajaxButton(Text("Get out!"), () => {
                                ding.delete()
                                Reload
                              })
                            } else if (ding.request.is == false && ding.blocked.is == false) {
                              "#request" #>
                                SHtml.ajaxButton(Text("Your request is proceeding do you want to cancel ?"), () => {
                                  ding.delete()
                                  Reload
                                })
                            } else {
                              "#request" #> <span>You have been blocked</span>
                            }
                          }).apply(in)

                    }.getOrElse(<span></span>)).apply(in)
              }).apply(in)

        }
    }
    out
  }
}

class ListOfSearchGroupMembers {
  def render(in: NodeSeq): NodeSeq = {
    val id = S.param("id").getOrElse("")
    val q = S.param("q").getOrElse("")
    var out = NodeSeq.Empty
    if (q == "edit") {
      for {
        sg <- SearchGroup.find(id)
      } yield {
        out = ("#listOfSearchGroupMembers *" #>
          DetectiveInGroup.findBySearchGroupAndRequest(sg, true).map {
            dig =>
              if (dig.detective_id.is != sg.admin_id.is) {
                ("li" #>
                  {
                    ("#username *" #> dig.getDetectiveUserName()).apply(in) ++
                      SHtml.ajaxButton(Text("Block"), () => {
                        dig.blockTheUser()
                        Reload
                      }) ++
                      SHtml.ajaxButton(Text("Delete"), () => {
                        dig.delete()
                        Reload
                      })
                  }).apply(in)
              } else {
                NodeSeq.Empty
              }

          } & ClearClearable).apply(in)
      }
    }
    out
  }
}

class ListOfRequests {
  def render(in: NodeSeq): NodeSeq = {
    val id = S.param("id").getOrElse("")
    val q = S.param("q").getOrElse("")
    var out = NodeSeq.Empty
    if (q == "edit") {
      for {
        sg <- SearchGroup.find(id)
      } yield {
        out = ("#listOfRequests *" #>
          {
            DetectiveInGroup.findBySearchGroupAndRequest(sg, false).map {
              dig =>
                ("li" #>
                  {
                    ("#username *" #> dig.getDetectiveUserName()).apply(in) ++
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
                  }).apply(in)

            }
          } & ClearClearable).apply(in)
      }
    }
    out
  }
}

class AdminSearchGroup {

  def render(in: NodeSeq): NodeSeq = {
    var id = S.param("id").getOrElse("")
    var q = S.param("q").getOrElse("")
    var name = ""
    var description = ""
    var searchGroup = SearchGroup.find(id)
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
          out =
            ("name=name" #> SHtml.text(name, name = _, "id" -> "the_name") &
              "name=description" #> SHtml.textarea(description, description = _, "id" -> "the_description") &
              "type=submit" #> SHtml.onSubmitUnit(() => {
                sg.edit(name, description)
                S.redirectTo("/searchgroup?q=show&id=" + sg.id.is.toString)

              })).apply(in)
        }
      }
    }

    out
  }

}

class SearchGroupMessengerComet extends InsertNamedComet with Loggable {
  override lazy val name = S.param("id") openOr ("")
  override lazy val cometClass = "SearchGroupChat"

}

class SearchGroupMessenger extends Loggable {

  def render(in: NodeSeq): NodeSeq = {
    var id = S.param("id").getOrElse("")
    val num = S.param("num").getOrElse("")
    val q = S.param("q").getOrElse("")
    var message = ""

    CometListerner.listenerFor(Full(id)) match {
                case a: LiftActor => {
                  logger.info("We send the id=" + id)
                  a ! Messages(id,10)
                
                }
                case _ => logger.info("No actor to send an update")
              }

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
              CometListerner.listenerFor(Full(sg.id.toString)) match {
                case a: LiftActor => {
                  logger.info("We send the id=" + sg.id.toString)
                  a ! Messages(sg.id.toString,10)
                
                }
                case _ => logger.info("No actor to send an update")
              }

              SetValById("message", "")
            })).apply(in)
        }
      }
    }
    out
  }
}





