package bootstrap.liftweb

import net.liftweb._
import http.{ LiftRules, NotFoundAsTemplate, ParsePath, Html5Properties, S, Req }
import sitemap.{ SiteMap, Menu, Loc }
import util.{ NamedPF }
import net.liftweb._
import common.{ Full }
import org.newsfromthestreets.model._
import _root_.net.liftweb.sitemap.Loc._
import org.newsfromthestreets.lib._
import net.liftmodules.mongoauth.MongoAuth
import org.newsfromthestreets.rest._

object Preperation {
  def prepareDetectives {
    DetectiveInGroup.findAll.foreach(_.changeMode(false))
    Detective.findAll.foreach(_.setMode(false))
  }
  def addCategories {
    val categories = List("Traffic","Crimes", "Politics", "Sport", "Business", "Arts", "Science", "Technology", "Health", "Fashion", "Opinion")
    categories.foreach {
      c => ArticleCategory.findOrAdd(c)
    }

  }
}
class Boot {
  def boot {

    MongoConfig.init()
    MongoAuth.authUserMeta.default.set(User)
    MongoAuth.siteName.default.set("admin")
    MongoAuth.loginTokenAfterUrl.default.set(Site.password.url)
    MongoAuth.systemEmail.default.set(SystemUser.user.email.is)
    MongoAuth.systemUsername.default.set(SystemUser.user.name.is)
    // where to search snippet
    LiftRules.addToPackages("org.newsfromthestreets")
    
    
    
    Preperation.prepareDetectives
    Preperation.addCategories

    // build sitemap
    LiftRules.setSiteMap(Site.siteMap)

    LiftRules.uriNotFound.prepend(NamedPF("404handler") {
      case (req, failure) => NotFoundAsTemplate(
        ParsePath(List("exceptions", "404"), "html", false, false))
    })

    LiftRules.dispatch.append(ArticleJsons)
    LiftRules.dispatch.append(DetectiveJsons)

    // set character encoding
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.isLoggedIn)

    //Show the spinny image when an Ajax call starts
    //LiftRules.ajaxStart =
    //  Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    //LiftRules.ajaxEnd =
    //  Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
    // set the default htmlProperties
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))
  }
}