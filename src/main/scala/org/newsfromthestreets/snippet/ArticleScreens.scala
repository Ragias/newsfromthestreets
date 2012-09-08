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
        out = (
                "#title" #> { article.title.is } &
                "#article" #> { article.article.is } &
               "#username" #> { user.name.is } &
                "#date" #> {
                  article.id.is.getTime().date.toString()
                } &
                "#lat" #> { article.geolatlng.get.lat.toString() } &
                "#lng" #> { article.geolatlng.get.long.toString() } 
              )(in)
        User.currentUser.map {
          u =>
            out = SHtml.ajaxButton(Text("Add Article"), ()=>{
              S.redirectTo("/article?q=add")
            }) ++ out
        }

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


