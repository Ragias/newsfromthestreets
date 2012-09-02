package org.newsfromthestreets.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import http._
import util._
import common._
import Helpers._
import org.newsfromthestreets.model._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.text.SimpleDateFormat

class ShowArticle {
  val param = S.param("id")
  def render(in: NodeSeq): NodeSeq = {
    var out: NodeSeq = <span></span>
    for {
      param <- S.param("id")
      q <- S.param("q")
      article <- Article.find(param)
      user <- article.user_id.obj
    } yield {
      if (q == "show") {
        out = <div class="showArticle">
                <span class="title"> { article.title.is }</span><br/>
                <span class="article"> { article.article.is }</span><br/>
                <span class="username"> { user.name.is } </span><br/>
                <span class="date"> {
                  article.id.getTime().date.toString()
                } </span><br/>
                <span class="lat"> { article.geolatlng.get.lat.toString() }</span><br/>
                <span class="lng"> { article.geolatlng.get.long.toString() } </span><br/>
              </div>
        User.currentUser.map {
          u =>
            out = <a id="addArticle" href="/article?q=add"> Add Article</a> ++ out
        }

      }

    }
    if (S.param("q").isEmpty) {
      User.currentUser.map {
        u =>
          out = <a id="addArticle" href="/article?q=add"> Add Article</a> ++ out
      }
    }

    out
  }
}

class AddArticle extends StatefulSnippet {
  private var title = ""
  private var article = ""
  private var category = ""
  private var latStr: String = "0.0"
  private var lngStr: String = "0.0"
  private var article_id = ""
  def add() {
    for {
      user <- User.currentUser
      lat <- asDouble(latStr)
      lng <- asDouble(lngStr)
    } yield {
      Article.add(user, title, article, category, lat, lng)
      S.notice("The article is added")
    }
  }

  def edit() {
    for {
      user <- User.currentUser
      lat <- asDouble(latStr)
      lng <- asDouble(lngStr)
    } yield {

      Article.find(article_id).map(_.edit(user, title, article, category, lat, lng))
      S.notice("The article is edited")
    }
  }

  def dispatch = { case "render" => render }
  def render(in: NodeSeq): NodeSeq = {
    val categoryList = ArticleCategory.findAll.map(ac => (ac.name.is,ac.name.is))
    val param = S.param("q")
    var out: NodeSeq = <span></span>

    User.currentUser.map {
      user =>
        if (param.getOrElse("") == "add") {
          out = ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
            "name=category" #> SHtml.select(categoryList,Empty, category=_ , "id" -> "the_category") &
            "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
            "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
            "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
            "type=submit" #> SHtml.onSubmitUnit(add))(in) //it little confusing but is just a function that gets the in
        } else if (param.getOrElse("") == "edit") {
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
                      "name=category" #> SHtml.select(categoryList,Full(category), category=_ , "id" -> "the_category") &
                      "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
                      "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
                      "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
                      "type=submit" #> SHtml.onSubmitUnit(edit))(in)
                  }
              }
          }

        }
    }
    out
  }

}

class ListOfArticles extends StatefulSnippet {
  private var date: Box[ArticleDate] = Empty
  private var category: Box[ArticleCategory] = Empty
  def dispatch = {
    case "render" => render

  }

  def showList(): JsCmd = {

    SetHtml("listOfArticles", <ul id="listOfArticles">
                                {

                                  Article.listByCategoryDateLocation(date, category, Empty, Empty).map {
                                    a =>
                                      <li>
                                        {
                                          <span> { a.title.is }</span>
                                          <br/>
                                          <span>{
                                            a.category_id.obj match {
                                              case Full(x) => x.name
                                              case _ => ""
                                            }
                                          }</span><br/>
                                          <span> { a.id.getTime().date.toString() }</span><br/>
                                          <span> { a.article.is }</span><br/>
                                          <span> { a.user_id.obj.get.name.is }</span><br/>
                                          <a href={ "/article?q=show&id=" + a.id.toString() }> more </a>
                                          <span>{
                                            var editXml: NodeSeq = <span></span>
                                            User.currentId.map {
                                              uid =>
                                                if (uid.toStringMongod() == a.user_id.is.toStringMongod()) {
                                                  editXml = <a href={ "/article?q=edit&id=" + a.id.toString() }> edit </a>
                                                }
                                            }
                                            editXml
                                          }</span><br/>
                                        }
                                      </li>
                                  }
                                }
                              </ul>)
  }

  def render(in: NodeSeq): NodeSeq = {
    var fmt = new SimpleDateFormat("dd-MM-yy");
    <div>{
      Script(OnLoad(showList)) ++
        ("name=date" #> SHtml.ajaxSelect(ArticleDate.findAll.map { ad =>
          val dateStr = fmt.format(ad.date.get.getTime())
          (dateStr, dateStr)
        } ++ List(("All", "All")), Full("All"), s => {
          if (s.equals("All")) {
            date = Empty
          } else {

            date = ArticleDate.findByDateString(s)
          }
          showList()
        }) &
          "name=category" #> SHtml.ajaxSelect(ArticleCategory.findAll.map {
            ac => (ac.name.is, ac.name.is)
          } ++ List(("All", "All")), Full("All"), s => {
            if (s.equals("All")) {
              category = Empty
            } else {
              category = ArticleCategory.findByName(s)
            }
            showList()
          }))(in)
    }</div>
  }
}

class CommentArticleSnippet extends StatefulSnippet {
  private var message = ""
  private var articleId = S.param("id")
  private var num = S.param("num")
  private var qparam = S.param("q")

  def dispatch = {
    case "render" => render
    case "listMessages" => listMessages
  }
  def listMessages(in: NodeSeq): NodeSeq = {
    var xhtml: NodeSeq = <span></span>
    qparam.map {
      q =>
        if (q == "show") {
          for {
            aid <- articleId
            article <- Article.find(aid)
          } yield {
            xhtml = <ul id="listOfMessages">
                      {
                        CommentArticle.showByNumberOfResults(article, asInt(num.getOrElse("10")).getOrElse(10)).map {
                          comment =>
                            <li>
                              {
                                <span> { comment.user_id.obj.get.name.is } </span>
                                <span> { comment.message.is } </span>
                              }
                            </li>
                        }
                      }
                    </ul>
          }
        }
    }
    xhtml
  }

  def render(in: NodeSeq): NodeSeq = {
    var xhtml: NodeSeq = <span></span>
    qparam.map {
      q =>
        if (q == "show") {
          for {
            aid <- articleId
            user <- User.currentUser
            article <- Article.find(aid)
          } yield {

            xhtml = ("#message" #> SHtml.ajaxTextarea("", s => {
              message = s
            }) &
              "#send" #> SHtml.ajaxButton(Text("Send"), () => {
                CommentArticle.add(user, article, message)
                showMessages() &
                  SetValById("message", "")

              }))(in)
          }
        }
    }
    <div>
      { Script(OnLoad(showMessages)) ++ xhtml }
    </div>

  }

  def showMessages(): JsCmd = {
    SetHtml("listOfMessages",
      {
        var xhtml: NodeSeq = <span></span>
        for {
          aid <- articleId
          article <- Article.find(aid)
        } yield {
          xhtml = <ul id="listOfMessages">
                    {
                      CommentArticle.showByNumberOfResults(article, asInt(num.getOrElse("10")).getOrElse(10)).map {
                        comment =>
                          <li>
                            {
                              <span> { comment.user_id.obj.get.name.is } </span>
                              <span> { comment.message.is } </span>
                            }
                          </li>
                      }
                    }
                  </ul>
        }
        xhtml
      })
  }
}