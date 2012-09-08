package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.util.Helpers._
import net.liftweb.json.JsonDSL._
import org.newsfromthestreets.model._
import net.liftweb.common._
import net.liftweb.json.JsonAST._

object DetectiveJsons extends RestHelper with Loggable {
  serve {
    case "api" ::  "detective" :: _ JsonPut json -> _ => {
      User.currentUser.map {
        u =>
          Detective.addUser(u).map {
            d =>
              d.setMode(true)
              if (d.mode.is) {

                if (json.values.isInstanceOf[Map[String, Double]]) {
                  d.setLocationByJson(json.values.asInstanceOf[Map[String, Map[String, Double]]])
                  logger.info("The DetectiveJsons put " + u.username.is + " location")
                } else {
                  logger.error("The DetectiveJsons " + u.username.is + "   has problems with json : " + json.values.toString())
                }
              }
          }
      }
      OkResponse()
    }

    case "api" ::  "detectivesetmode" :: _ JsonPut json -> _ => {
      val modeOpt = json.\("mode").extractOpt[Boolean]
      for {
        mode <- modeOpt
        user <- User.currentUser
        d <- Detective.findByUser(user)
      } yield {
        d.setMode(mode)
        logger.info("The DetectiveJsons set mode for " + user.username.is )
      }
      OkResponse()
    }

    case "api" :: "detectives" :: _ JsonGet _ => {
      JArray(Detective.findByMode(true).map {

        d => d.asJValue
      })
    }

  }
}