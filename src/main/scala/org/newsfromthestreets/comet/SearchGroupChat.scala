package org.newsfromthestreets.comet

import net.liftweb._
import http._
import util._
import Helpers._
import com.fmpwizard.cometactor.pertab.namedactor.{ NamedCometActor, CometListerner }
import net.liftweb.common._
import org.newsfromthestreets.model._
import scala.xml.NodeSeq


case class Messages(groupId: String, limit: Int)

class SearchGroupChat extends NamedCometActor with Loggable {
  
  private var limit = 10
  private var groupId = ""

  override def lowPriority: PartialFunction[Any, Unit] = {
   

    case Messages(id, lim) => {
      logger.info("We received the id=" + id)
      groupId = id
      limit = lim
      reRender();
    }
    case _ => error("The message for SearchGrouChat is not correct")
  }
  def render = {
    var out = NodeSeq.Empty
    for {
      sg <- SearchGroup.find(groupId)
    } yield {
      out = <ul id="groupMessages"> {
        SearchGroupMessage.showByGroupAndLimit(sg,limit).map {
          x => <li> { x.getUsername() + " : " + x.message.is } </li>
        }
      }</ul>

    }
    out
  }

}