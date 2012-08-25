package org.newsfromthestreets.rest
import net.liftweb.http._
import rest._
import net.liftweb.json._
import org.newsfromthestreets.model._

object ArticleJsons extends RestHelper{
  serve{
     case "api" ::"newsfromthestreets" :: "articles" :: date :: _ JsonGet _ => {  
       if(date == "all"){
         JArray(Article.findAll.map{
           a => a.asJValue
         })
         
       }else{
         JArray(Article.listByDate(date).map(a=> a.asJValue))
       }
     
     }
     
   case "api" :: "user" :: _ JsonPost json -> _ => {
     User.currentUser.map{
       u => 
         Article.createFromJson(u,json)
          OkResponse()
     }
     ForbiddenResponse("No access for you")

    
   
   }
     
  }
}