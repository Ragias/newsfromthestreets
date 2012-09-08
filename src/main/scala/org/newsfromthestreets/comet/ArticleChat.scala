package org.newsfromthestreets.comet
import net.liftweb._
import http._
import util._
import Helpers._
import com.fmpwizard.cometactor.pertab.namedactor.{ NamedCometActor, CometListerner }
import net.liftweb.common._
import org.newsfromthestreets.model._
import scala.xml.NodeSeq


case class Chat(id: String, limit: Int)

class ArticleChat extends NamedCometActor with Loggable {
  
  private var limit = 10
  private var articleId = ""

  override def lowPriority: PartialFunction[Any, Unit] = {
   

    case Chat(id, lim) => {
      logger.info("We received the id=" + id)
      articleId = id
      limit = lim
      reRender();
    }
    case _ => error("The message for ArticleChat is not correct")
  }
  def render = {
    var out = NodeSeq.Empty
    for {
      a <- Article.find(articleId)
    } yield {
      out = <ul id="articleComments"> {
        CommentArticle.showByArticleAndLimit(a,limit).map {
          x => <li> { x.getUsername() + " : " + x.message.is } </li>
        }
      }</ul>

    }
    out
  }

}