package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.util.Helpers._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import org.newsfromthestreets.model._
import net.liftweb.common._

object ArticleJsons extends RestHelper with Loggable {
  serve {
    case "api" :: "newsfromthestreets" :: "articles" :: category :: date :: _ JsonGet req => {
      val articleDate: Box[ArticleDate] = if (date.toLowerCase() == "all") Empty else ArticleDate.findByDateString(date)
      val articleCategory: Box[ArticleCategory] = if (category.toLowerCase() == "all") Empty else ArticleCategory.findByName(category)
      val lat = asDouble(req.param("lat").getOrElse(""))
      val lng = asDouble(req.param("lng").getOrElse(""))

      

      JArray(Article.listByCategoryDateLocation(articleDate,articleCategory,lat,lng).map {
        a => a.asJValue
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

    case "api" :: "example" :: _ JsonGet _ => {
      var resp: JObject = ("user" -> "none")
      User.currentUser.map {
        u =>
          resp = ("user" -> u.name.is)
      }
      resp
    }

  }
}