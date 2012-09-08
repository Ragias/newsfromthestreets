package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.util.Helpers._
import net.liftweb.json.JsonDSL._
import org.newsfromthestreets.model._
import net.liftweb.common._
import net.liftweb.json.JsonAST._

object GroupJsons extends RestHelper with Loggable {
  serve {
    case "api" :: "ingroup" :: "setmode"  :: _ JsonPut json -> _ => {
      val groupIdOpt = json.\("groupId").extractOpt[String]
      val modeOpt = json.\("mode").extractOpt[Boolean]
      for {
        gid <- groupIdOpt
        mode <- modeOpt
        user <- User.currentUser
        d <- Detective.findByUser(user)
        sg <- SearchGroup.find(gid)
      } yield {

        d.setMode(true)
        DetectiveInGroup.findByDetectiveAndGroup(d, sg).filter {
          dig => dig.request.is == true
        }.map {
          dig =>
              
              dig.changeMode(mode)
              dig.changeModeToFalseForAllTheOthersExceptThis

            
            logger.info("The mode is changed between detective_id=" + d.id.is.toString + " and searchgroup_id=" + sg.id.is.toString)
        }.getOrElse {
          logger.error("The mode is changed between detective_id=" + d.id.is.toString + " and searchgroup_id=" + sg.id.is.toString)
        }
      }
      OkResponse()

    }

    case "api" :: "searchgroup" :: searchgroup_id :: _ JsonGet _ => {
      var ls: JArray = JArray(List())
      if (searchgroup_id != "None") {
        for {
          group <- SearchGroup.find(searchgroup_id)
        } yield {

          ls = JArray(DetectiveInGroup.findBySearchGroupAndMode(group, true).filter {
            dig =>
              !dig.detective_id.obj.isEmpty && dig.request.is == true && dig.blocked.is == false
          }.map {
            dig =>
              val d = dig.detective_id.obj.get
              ("id" -> d.id.is.toString()) ~
                ("username" -> d.getUserName()) ~
                ("latlng" -> ("lat" -> d.geolatlng.get.lat) ~ ("long" -> d.geolatlng.get.long))
          })

        }
      } else {
        ls = JArray(Detective.findByMode(true).map {

          d =>
            ("id" -> d.id.is.toString()) ~
              ("username" -> d.getUserName()) ~
              ("latlng" -> ("lat" -> d.geolatlng.get.lat) ~ ("long" -> d.geolatlng.get.long))
        })
      }
      ls
    }
  }
}