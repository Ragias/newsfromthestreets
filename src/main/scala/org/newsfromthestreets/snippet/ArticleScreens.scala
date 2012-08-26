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
    var xthml: NodeSeq = <span>Nothing</span>
    for {
      param <- S.param("id")
      q <- S.param("q")
      article <- Article.find(param)
      user <- article.user_id.obj
    } yield {
      if (q == "show") {
        xthml = <div class="showArticle">
                  <span class="title"> { article.title.is }</span><br/>
                  <span class="article"> { article.article.is }</span><br/>
                  <span class="username"> { user.name.is } </span><br/>
                  <span class="date"> { article.date_id.obj.get.date.is.getTime().toString() } </span><br/>
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
      Article.add(user, title, article,category, lat, lng)
      S.notice("The article is added")
    }
  }

  def edit() {
    for {
      user <- User.currentUser
      lat <- asDouble(latStr)
      lng <- asDouble(lngStr)
    } yield {
      Article.edit(article_id, user, title, article, lat, lng)
      S.notice("The article is edited")
    }
  }

  def dispatch = { case "render" => render }
  def render(in: NodeSeq): NodeSeq = {
    val param = S.param("q")
    if (param.getOrElse("") == "add") {
      ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
        "name=category" #> SHtml.text(category, category = _, "id" -> "the_category") &
        "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
        "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
        "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
        "type=submit" #> SHtml.onSubmitUnit(add))(in) //it little confusing but is just a function that gets the in
    } else if (param.getOrElse("") == "edit") {
      article_id = S.param("id").getOrElse("")
      Article.find(article_id).map {
        a =>
          title = a.title.is
          article = a.article.is
          latStr = a.lat.is.toString()
          lngStr = a.lng.is.toString()

      }
      ("name=title" #> SHtml.text(title, title = _, "id" -> "the_title") &
        "name=category" #> SHtml.text(category, category = _, "id" -> "the_category") &
        "name=article" #> SHtml.textarea(article, article = _, "id" -> "the_article") &
        "name=lat" #> SHtml.text(latStr, latStr = _, "id" -> "the_lat") &
        "name=lng" #> SHtml.text(lngStr, lngStr = _, "id" -> "the_lng") &
        "type=submit" #> SHtml.onSubmitUnit(edit))(in)
    } else {
      <span></span>
    }
  }

}

class ListOfArticles extends StatefulSnippet {
  private var date: Box[String] = Empty
  private var category:Box[String] = Empty
  def dispatch = { 
    case "render" => render
    
    }
  
  def showList():JsCmd = {
   SetHtml("listOfArticles", <ul id="listOfArticles">
                           {
	   							
                             Article.listByCategoryAndDate(category,date).map {
                               a =>
                                 <li>
                                   {
                                     <span> { a.title.is }</span>
                                     <br/>
                                     <span>{a.category_id.obj.get.name}</span> <br/>
                                     <span> { a.date_id.is.getTime().toString() }</span><br/>
                                     <span> { a.article.is }</span><br/>
                                     <span> { a.user_id.obj.get.name.is }</span><br/>
                                     <a href={ "/article?q=show&id=" + a.id.toString() }> more </a><br/>
                                   }
                                 </li>
                             }
                           }
                         </ul>)
  }
  
  def render(in:NodeSeq) :NodeSeq= {
     var fmt = new SimpleDateFormat("dd-MM-yy");
     
       ("name=date" #> SHtml.ajaxSelect(ArticleDate.findAll.map{ad=> 
          val dateStr = fmt.format(ad.date.get.getTime())
         (dateStr ,    dateStr)
         }++List(("All","All")), Full("All"),s=> {
          if(s.equals("All")){
            date=Empty
          }else{
           
            date=Full(s)
          }
           
         }) &
        "name=category" #> SHtml.ajaxSelect(ArticleCategory.findAll.map{
          ac => (ac.name.is , ac.name.is)
        }++List(("All","All")), Full("All"), s => {
          if(s.equals("All")){
            category=Empty
          }else{
            category=Full(s) 
          }
           
          })&
        "name=submit" #> SHtml.ajaxButton(Text("Press me"), ()=> showList())  )(in)
  }
}