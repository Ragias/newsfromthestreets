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
      val limit = asInt(req.param("limit").getOrElse("")).getOrElse(5)
      JArray(
        Article.listByCategoryDateLocation(articleDate, articleCategory, lat, lng, 0.5).takeRight(limit).map {
          a =>
            ("id" -> a.id.is.toString()) ~
              ("title" -> a.title.is) ~
              ("category" -> a.getCategoryName()) ~
              ("date" -> a.getExactDateInString()) ~
              ("username" -> a.getUsername()) ~
              ("icon" -> a.getIcon()) ~
              ("latlng" -> ("lat" -> a.geolatlng.get.lat) ~ ("long" -> a.geolatlng.get.long))
        })

    }
    case "api" :: "newsfromthestreets" :: "article" :: id :: _ JsonGet req => {
      for {
        // find the item, and if it’s not found,
        // return a nice message for the 404
        a <- Article.find(id) ?~ "Article Not Found"
      } yield {
        ("id" -> a.id.is.toString()) ~
          ("title" -> a.title.is) ~
          ("category" -> a.getCategoryName()) ~
          ("date" -> a.getExactDateInString()) ~
          ("username" -> a.getUsername()) ~
          ("icon" -> a.getIcon()) ~
          ("latlng" -> ("lat" -> a.geolatlng.get.lat) ~ ("long" -> a.geolatlng.get.long))
      }

    }

  }
}