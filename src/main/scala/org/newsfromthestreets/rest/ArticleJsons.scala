package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import org.newsfromthestreets.model._
import net.liftweb.common._

object ArticleJsons extends RestHelper {
  serve {
    case "api" :: "newsfromthestreets" :: "articles" :: category :: date :: _ JsonGet _ => {
      val ad: Box[String] = if (date == "All") Empty else Full(date)
      val ac: Box[String] = if (category == "All") Empty else Full(category)

      JArray(Article.listByCategoryAndDate(ac, ad).map {
        a => ("title" -> a.title.is) ~ ("lat" -> a.lat.is) ~ ("lng" -> a.lng.is)
      })

    }

    case "api" :: "newsfromthestreets" :: "article" :: _ JsonPost json -> _ => {
      User.currentUser.map {
        u =>
          Article.createFromJson(u, json)
          OkResponse()
      }
      ForbiddenResponse("No access for you")

    }
    
    case "api" :: "example":: _ JsonGet _ => {
      var resp:JObject =  ("user" -> "none") 
      User.currentUser.map {
        u =>
         resp = ("user" -> u.name.is)
      }
      resp
    }
  
  }
}