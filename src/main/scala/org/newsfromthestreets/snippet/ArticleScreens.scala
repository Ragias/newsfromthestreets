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
      q <- S.param("q")
      article <- Article.find(param)
      user <- article.user_id.obj
    } yield {
      if(q == "show"){
        xthml = <div class="showArticle">
                <span class="title"> { article.title.is }</span><br/>
                <span class="article"> { article.article.is }</span><br/>
                <span class="username"> { user.name.is } </span><br/>
                <span class="date"> { article.date.is.toString() } </span><br/>
                <span class="lat"> { article.lat.is.toString() }</span><br/>
                <span class="lng"> { article.lng.is.toString() } </span><br/>
              </div>
      }

    }

    xthml
  }
}

class AddArticle extends StatefulSnippet {
  private var title = ""
  private var article = ""
  private var latStr: String = "0.0"
  private var lngStr: String = "0.0"
  private var article_id = ""
  def add() {
    for {
      user <- User.currentUser
      lat <- asDouble(latStr)
      lng <- asDouble(lngStr)
    } yield {
      Article.add(user, title, article, lat, lng)
      S.notice("The article is added")
    }
  }
  
  def edit(){
    for {
      user <- User.currentUser
      lat <- asDouble(latStr)
      lng <- asDouble(lngStr)
    } yield {
      Article.edit( article_id ,user, title, article, lat, lng)
      S.notice("The article is edited")
    }
  }
  
  def dispatch = { case "render" => render }
  def render(in:NodeSeq):NodeSeq = {
     val param = S.param("q")
     if(param.getOrElse("") == "add"){
      ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
      "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
      "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
      "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
      "type=submit" #> SHtml.onSubmitUnit(add))(in) //it little confusing but is just a function that gets the in
     }else if(param.getOrElse("") == "edit"){
       article_id  = S.param("id").getOrElse("") 
      Article.find( article_id ).map{
        a =>
          title = a.title.is
          article = a.article.is
          latStr = a.lat.is.toString()
          lngStr = a.lng.is.toString()
          
      }
      ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
      "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
      "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
      "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
      "type=submit" #> SHtml.onSubmitUnit(edit))(in)
     }else{
       <span></span>
     }
  }

}