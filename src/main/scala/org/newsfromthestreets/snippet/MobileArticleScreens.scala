package org.newsfromthestreets.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import http._
import util._
import common._
import Helpers._
import org.newsfromthestreets.model._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import java.text.SimpleDateFormat
import com.fmpwizard.cometactor.pertab.namedactor.InsertNamedComet
import com.fmpwizard.cometactor.pertab.namedactor.CometListerner
import net.liftweb.actor.LiftActor
import org.newsfromthestreets.comet.Chat

class ListOfNews extends DispatchSnippet {
  def dispatch = {
    case "render" => render

  }

  def render(in: NodeSeq): NodeSeq = {
    var fmt = new SimpleDateFormat("dd-MM-yy");
    <div>{

      (
        "name=date" #> SHtml.ajaxSelect(ArticleDate.findAll.map { ad =>
          val dateStr = fmt.format(ad.date.get.getTime())
          (dateStr, dateStr)
        } ++ List(("All", "All")), Full("All"), s => {
          Noop
        }) &
        "name=category" #> SHtml.ajaxSelect(ArticleCategory.findAll.map {
          ac => (ac.name.is, ac.name.is)
        } ++ List(("All", "All")), Full("All"), s => {
          Noop
        }))(in)
    }</div>
  }
}
class AddNewButton {
  def render: NodeSeq = {
    User.currentUser.map {
      u =>
        SHtml.ajaxButton(Text("Add article"), () => {
          S.redirectTo("/addArticle")
        })

    }.getOrElse(NodeSeq.Empty)
  }
}

class AddNew extends StatefulSnippet {
  private var title = ""
  private var article = ""
  private var category = ""
  private var latStr: String = ""
  private var lngStr: String = ""
  private var article_id = ""
  def add() {

    User.currentUser.map {
      user =>

        if (latStr.isEmpty || lngStr.isEmpty) {
          S.error("location", "Choose a location from the google map")

        } else if (title.isEmpty || article.isEmpty()) {
          S.error("article", "Add the title AND the article")

        } else {
          for {
            lat <- asDouble(latStr)
            lng <- asDouble(lngStr)
          } yield {
            Article.add(user, title, article, category, lat, lng)
            S.redirectTo("/index")
          }

        }
    }.getOrElse {
      S.redirectTo("/index")

    }
  }

  def dispatch = { case "render" => render }
  def render(in: NodeSeq): NodeSeq = {
    val categoryList = ArticleCategory.findAll.map(ac => (ac.name.is, ac.name.is))
    val param = S.param("q")
    var out: NodeSeq = <span></span>

    User.currentUser.map {
      user =>

        out = ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
          "name=category" #> SHtml.select(categoryList, Empty, category = _, "id" -> "the_category") &
          "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
          "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
          "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
          "type=submit" #> SHtml.onSubmitUnit(
            add))(in) ++ SHtml.ajaxButton(Text("Cancel"), () => S.redirectTo("/index")) //it little confusing but is just a function that gets the in

    }
    out
  }

}

class EditNew extends StatefulSnippet {
  private var title = ""
  private var article = ""
  private var category = ""
  private var latStr: String = "0.0"
  private var lngStr: String = "0.0"
  private var article_id = ""
  def edit() {
    User.currentUser.map {
      user =>

        if (latStr.isEmpty || lngStr.isEmpty) {
          S.error("location", "Choose a location from the google map")

        } else if (title.isEmpty || article.isEmpty()) {
          S.error("article", "Add the title AND the article")

        } else {
          for {
            lat <- asDouble(latStr)
            lng <- asDouble(lngStr)
          } yield {
            Article.find(article_id).map(_.edit(user, title, article, category, lat, lng))
            S.redirectTo("/index")
          }

        }
    }.getOrElse {
      S.redirectTo("/index")

    }
  }
  def dispatch = { case "render" => render }

  def render(in: NodeSeq): NodeSeq = {
    val categoryList = ArticleCategory.findAll.map(ac => (ac.name.is, ac.name.is))
    val param = S.param("q")
    var out: NodeSeq = <span></span>

    User.currentUser.map {
      user =>
        if (param.getOrElse("") == "edit") {
          article_id = S.param("id").getOrElse("")
          Article.find(article_id).map {
            a =>
              User.currentId.map {
                uid =>
                  if (uid.toStringMongod() == a.user_id.is.toStringMongod()) {
                    title = a.title.is
                    article = a.article.is
                    category = a.category_id.obj.map(_.name.is).getOrElse("")
                    latStr = a.geolatlng.get.lat.toString()
                    lngStr = a.geolatlng.get.long.toString()
                    out = ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
                      "name=category" #> SHtml.select(categoryList, Full(category), category = _, "id" -> "the_category") &
                      "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
                      "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
                      "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
                      "type=submit" #> SHtml.onSubmitUnit(
                        edit))(in) ++ SHtml.ajaxButton(Text("Cancel"), () => S.redirectTo("/index"))
                  }
              }
          }

        }
    }
    out
  }
}

class ShowNew {
  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = NodeSeq.Empty
    val q = S.param("q").getOrElse("")
    val id = S.param("id").getOrElse("")
    if (q == "show") {
      Article.find(id).map {
        a =>

          out = ("#title" #> <h3>{ a.title.is }</h3> &
            "#username" #> <span>{ a.getUsername() }</span> &
            "#date" #> <p>{ a.getExactDateInString() }</p> &
            "#article" #> <article>{ a.article.is } </article>)(in)
          if (User.currentUser.isEmpty) {
            out +:= <ul id="listOfComments"></ul>
          }
      }
    }
    out
  }
}

class ListOfMyNews {
  def render = {
    var out: NodeSeq = NodeSeq.Empty
    var limit = asInt(S.param("limit").getOrElse("")).getOrElse(10)
    User.currentUser.map {
      user =>
        val ls = Article.findByUser(user)

        out = SHtml.ajaxButton(Text("Add Article"), () => {
          S.redirectTo("/addArticle")
        }) ++ <ul id="listOfMyNews" data-role="listview" data-theme="b">
                {
                  ls.take(limit).map {
                    a =>
                      <li>
                        {
                          <a href={ "/article?q=show&id=" + a.id.is.toString() }>{ a.title.is }</a> ++
                            SHtml.ajaxButton(Text("Edit"), () => {
                              S.redirectTo("/article?q=edit&id=" + a.id.is.toString())
                            }) ++
                            SHtml.ajaxButton(Text("Delete"), () => {
                              a.delete()
                              Reload
                            })

                        }
                      </li>
                  }

                }
              </ul>
        if (ls.size > limit) {
          out = out ++ SHtml.ajaxButton(Text("show more"), () => {
            limit += 10
            S.redirectTo("/listofarticles?limit=" + limit)
          })
        }
    }

    out
  }
}

class ArticleChatComet extends InsertNamedComet with Loggable {
  override lazy val name = S.param("id") openOr ("")
  override lazy val cometClass = "ArticleChat"

}

class CommentNew extends Loggable {
  def render(in: NodeSeq): NodeSeq = {
    var message = ""
    var xhtml: NodeSeq = NodeSeq.Empty
    val aid = S.param("id").getOrElse("")
    val q = S.param("q")

    if (q == "show") {
      CometListerner.listenerFor(Full(aid)) match {
        case a: LiftActor => {
          logger.info("We send the id=" + aid)
          a ! Chat(aid, 10)

        }
        case _ => logger.info("No actor to send an update")
      }
      for {

        user <- User.currentUser
        article <- Article.find(aid)
      } yield {

        xhtml = ("#message" #> SHtml.ajaxTextarea("", s => {
          message = s
        }) &
          "#send" #> SHtml.ajaxButton(Text("Send"), () => {
            CommentArticle.add(user, article, message)
            CometListerner.listenerFor(Full(aid)) match {
              case a: LiftActor => {
                logger.info("We send the id=" + aid)
                a ! Chat(aid, 10)

              }
              case _ => logger.info("No actor to send an update")
            }
            SetValById("message", "")

          }))(in)
      }
    }

    xhtml
  }
}


