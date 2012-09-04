package org.newsfromthestreets.lib

import net.liftweb._
import common._
import http.S
import sitemap._
import sitemap.Loc._
import net.liftmodules.mongoauth.Locs
import org.newsfromthestreets.model._

case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object MenuGroups {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup = LocGroup("topbar")
  val SigningGroup = LocGroup("signing")
}
object Site extends Locs {
  import MenuGroups._
  val home = MenuLoc(Menu("Home") / "index")

  val loginToken = MenuLoc(buildLoginTokenMenu)
  val logout = MenuLoc(buildLogoutMenu)
  private val profileParamMenu = Menu.param[User]("User", "Profile",
    User.findByUsername _,
    _.username.is) / "user" >> Loc.CalcValue(() => User.currentUser)
    
 
                                   
  lazy val profileLoc = profileParamMenu.toLoc
  val password = MenuLoc(Menu.i("Password") / "settings" / "password" >> RequireLoggedIn >> SettingsGroup >>Hidden)
  val account = MenuLoc(Menu.i("Account") / "settings" / "account" >> SettingsGroup >> RequireLoggedIn>>Hidden)
  val editProfile = MenuLoc(Menu("EditProfile", "Profile") / "settings" / "profile" >> SettingsGroup >> RequireLoggedIn>>Hidden)
  val settings = MenuLoc(Menu("Settings") / "settings" >> RequireLoggedIn >>Hidden)
  
  val login = MenuLoc(Menu("Login") / "login" >> RequireNotLoggedIn >> SigningGroup)
  val register = MenuLoc(Menu("Register") / "register" >> RequireNotLoggedIn >> SigningGroup)
  
  val article = MenuLoc(Menu("Article")/"article" >> Hidden )
  val addArticle = MenuLoc(Menu("Add Article")/"addArticle" >> RequireLoggedIn >>Hidden)
  val listOfArticles = MenuLoc(Menu("List of my Articles")/"listofarticles" >> RequireLoggedIn)
  
  
  
  val detective = MenuLoc(Menu("Detective")/"detective">> RequireLoggedIn)
  val searchgroup = MenuLoc(Menu("Search Group")/"searchgroup">> RequireLoggedIn)
  val listOfSearchGroups = MenuLoc(Menu("List of Search Groups")/"listofsearchgroups">> RequireLoggedIn)
  
  private def menu = List(home.menu,
    login.menu,
    register.menu,
    //loginToken.menu,
    logout.menu,
    profileParamMenu,
    password.menu,
    account.menu,
    settings.menu,
    editProfile.menu,
    article.menu ,
    addArticle.menu,
    listOfArticles.menu
    //detective.menu,
    //searchgroup.menu,
    //listOfSearchGroups.menu
    )

  def siteMap = SiteMap(menu: _*)
}