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
                  Detective.getLocationByJson(d, json.values.asInstanceOf[Map[String, Double]])
                  logger.info("The Detective " + u.username.is + " Jsons json is : " + json.values.toString())
                } else {
                  logger.error("The Detective " + u.username.is + " Jsons  has problems with json : " + json.values.toString())
                }
              } 
          }
      }
      OkResponse()
    }
    
    case "api" :: "newsfromthestreets" :: "detectives" :: mode :: _ JsonGet _ => {
      var m = false
      if(mode == "true"){
        m = true
      }
      JArray(Detective.findByMode(m).map{
        
        d => d.asJValue
      })
    } 
    
    case "api" :: "newsfromthestreets" :: "specificdective" :: user_id :: _ JsonGet _ => {
      (for{ u <- User.find(user_id)
           d <- Detective.findByUser(u)} 
          yield{
             d.asJValue
          })
      
    }
  }
}