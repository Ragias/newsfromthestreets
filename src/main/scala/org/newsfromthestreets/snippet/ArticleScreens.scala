package org.newsfromthestreets.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import http._
import util._
import common._
import Helpers._
import org.newsfromthestreets.model._

class ShowArticle {
  val param = S.param("id")
  def render(in: NodeSeq): NodeSeq = {
    var xthml: NodeSeq = <span>Nothing</span>
    for {
      param <- S.param("id")
      article <- Article.find(param)
      user <- article.user_id.obj
    } yield {
      xthml = <div class="showArticle">
                <span class="title"> { article.title.is }</span>
                <span class="article"> { article.article.is }</span>
                <span class="username"> { user.name.is } </span>
                <span class="date"> { article.date.is.toString() } </span>
                <span class="lat"> { article.lat.is.toString() }</span>
                <span class="lng"> { article.lng.is.toString() } </span>
              </div>

    }

    xthml
  }
}

class AddArticle extends StatefulSnippet {
  private var title = ""
  private var article = ""
  private var latStr: String = "0.0"
  private var lngStr: String = "0.0"

  def process() {
    for {
      user <- User.currentUser
      lat <- asDouble(latStr).?~!("It does not work")
      lng <- asDouble(lngStr) ?~ ("lalala")
    } yield {
      Article.add(user, title, article, lat, lng)
      S.notice("The article is added")
    }
  }
  def dispatch = { case "render" => render }
  def render = {
    "name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
      "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
      "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
      "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }

}