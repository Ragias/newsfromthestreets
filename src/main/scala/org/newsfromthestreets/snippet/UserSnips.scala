package  org.newsfromthestreets.snippet
import scala.xml._

import net.liftweb._
import common._
import http.{DispatchSnippet, S, SHtml, StatefulSnippet}
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._
import net.liftmodules.mongoauth.model.ExtSession
import  org.newsfromthestreets.model._
import  org.newsfromthestreets.lib._

object UserLogin extends Loggable {

  def render = {
    // form vars
    var password = ""
    var hasPassword = true
    var remember = User.loginCredentials.is.isRememberMe

   

    def doSubmit(): JsCmd = {
      S.param("email").map(e => {
        val email = e.toLowerCase.trim
        // save the email and remember entered in the session var
        User.loginCredentials(LoginCredentials(email, remember))

        if (hasPassword && email.length > 0 && password.length > 0) {
          User.findByEmail(email) match {
            case Full(user) if (user.password.isMatch(password)) =>
              logger.debug("pwd matched")
              User.logUserIn(user, true)
              if (remember) User.createExtSession(user.id.is)
              else ExtSession.deleteExtCookie()
              RedirectTo(Site.home.url)
            case _ =>
              S.error("Invalid credentials")
              Noop
          }
        }
        else if (hasPassword && email.length <= 0 && password.length > 0) {
          S.error("id_email_err", "Please enter an email")
          Noop
        }
        else if (hasPassword && password.length <= 0 && email.length > 0) {
          S.error("id_password_err", "Please enter a password")
          Noop
        }
        else if (hasPassword) {
          S.error("id_email_err", "Please enter an email")
          S.error("id_password_err", "Please enter a password")
          Noop
        }
        else if (email.length > 0) {
          // see if email exists in the database
          User.findByEmail(email) match {
            case Full(user) =>
              User.sendLoginToken(user)
              User.loginCredentials.remove()
              S.notice("An email has been sent to you with instructions for accessing your account")
              Noop
            case _ =>
              RedirectTo(Site.register.url)
          }
        }
        else {
          S.error("id_email_err", "Please enter an email address")
          Noop
        }
      }) openOr {
        S.error("id_email_err", "Please enter an email address")
        Noop
      }
    }

    def cancel() = S.seeOther(Site.home.url); Noop

    "#id_email [value]" #> User.loginCredentials.is.email &
    "#id_password" #> SHtml.password(password, password = _) &
    "name=remember" #> SHtml.checkbox(remember, remember = _) &
    "#id_submit" #> SHtml.hidden(doSubmit)
  }
}

sealed trait UserSnippet extends AppHelpers with Loggable {

  protected def user: Box[User]

  protected def serve(snip: User => NodeSeq): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)
    }): NodeSeq

  protected def serve(html: NodeSeq)(snip: User => CssSel): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)(html)
    }): NodeSeq

  def header(xhtml: NodeSeq): NodeSeq = serve { user =>
    <div id="user-header">
      <h3>{name(xhtml)}</h3>
    </div>
  }



  def username(xhtml: NodeSeq): NodeSeq = serve { user =>
    Text(user.username.is)
  }

  def name(xhtml: NodeSeq): NodeSeq = serve { user =>
    if (user.name.is.length > 0)
      Text("%s (%s)".format(user.name.is, user.username.is))
    else
      Text(user.username.is)
  }

  def title(xhtml: NodeSeq): NodeSeq = serve { user =>
    <lift:head>
      <title lift="Menu.title">{"$name$: %*% - "+user.username.is}</title>
    </lift:head>
  }
}

object CurrentUser extends UserSnippet {
  protected def user = User.currentUser
}

object ProfileLocUser extends UserSnippet {

  protected def user = Site.profileLoc.currentValue

  import java.text.SimpleDateFormat

  val df = new SimpleDateFormat("MMM d, yyyy")

  def profile(html: NodeSeq): NodeSeq = serve(html) { user =>
    val editLink: NodeSeq =
      if (User.currentUser.filter(_.id.is == user.id.is).isDefined)
        SHtml.ajaxButton(Text("Settings"),()=>{
          S.redirectTo("/settings")
        })
      else
        NodeSeq.Empty

    "#id_name *" #> <h3>{user.name.is}</h3> &
    "#id_location *" #> user.location.is &
    "#id_whencreated" #> df.format(user.whenCreated.toDate).toString &
    "#id_bio *" #> user.bio.is &
    "#id_editlink *" #> editLink
  }
}