package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.json.JsonDSL._
import org.newsfromthestreets.model._
import net.liftweb.common._
import net.liftweb.json.JsonAST._

object DetectiveJsons extends RestHelper with Loggable {
  serve {
    case "api" :: "newsfromthestreets" :: "detective" :: _ JsonPut json -> _ => {
      User.currentUser.map {
        u =>
          Detective.findByUser(u).map {
            d =>

              if (d.mode.is) {
                
                if (json.values.isInstanceOf[Map[String, Double]]) {
                  d.setLocationByJson(json.values.asInstanceOf[Map[String, Map[String,Double]]])
                  logger.info("The Detective " + u.username.is + " Jsons json is : " + json.values.toString())
                } else {
                  logger.error("The Detective " + u.username.is + " Jsons  has problems with json : " + json.values.toString())
                }
              }
          }
      }
      OkResponse()
    }

    case "api" :: "newsfromthestreets" :: "detectives" :: _ JsonGet _ => {
      JArray(Detective.findByMode(true).map {

        d => d.asJValue
      })
    }

    case "api" :: "newsfromthestreets" :: "searchgroup" :: searchgroup_id :: _ JsonGet _ => {
      var ls: JArray = JArray(List())
      if (searchgroup_id != "None") {
        for {
          user <- User.currentUser
          detective <- user.getDetective
          group <- SearchGroup.find(searchgroup_id)
        } yield {
          ls = JArray(DetectiveInGroup.findBySearchGroupAndMode(group, true).filter {
            dig =>
              !dig.detective_id.obj.isEmpty && dig.request.is == true && dig.blocked.is == false
          }.map {
            dig =>
              dig.detective_id.obj.get.asJValue
          })

        }
      } else {
        ls = JArray(Detective.findByMode(true).map {

          d => d.asJValue
        })
      }
      ls
    }

  }
}