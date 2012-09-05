package org.newsfromthestreets.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import http._
import util._
import common._
import Helpers._
object Mobile  {

  def mobile(in:NodeSeq):NodeSeq = {
    if(isMobile) in else NodeSeq.Empty
  }

  def desktop(in:NodeSeq):NodeSeq = {
    if(!isMobile) in else NodeSeq.Empty
  }
    

  def isMobile: Boolean = S.request.map {
    req =>
      req.userAgent.map {
        ua =>
          //true 
          ua.toLowerCase().contains("iphone") ||
           ua.toLowerCase().contains("android")
      }.getOrElse(false)
  }.getOrElse(false) 

  //todo cange ismobile
  def render = if (isMobile) "#mobile ^^" #> true else "#desktop ^^" #> true
 
}

class TemplateSelector extends StatefulSnippet {

  def dispatch = {
    case "select" => select
  }
    
  def select = {
    "#main [class]" #> someLogicForSelectingTemplate()
  }
  
  /*Demonstrating with random selection */
  def someLogicForSelectingTemplate() = {
    if(Mobile.isMobile){
      "lift:surround?with=mobile;at=main"
    }else{
      "lift:surround?with=desktop;at=main"
    }
  }
}


