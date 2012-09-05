package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JValue
import com.foursquare.rogue.LatLong

class Detective extends MongoRecord[Detective] with ObjectIdPk[Detective] with Loggable {
  def meta = Detective
  object user_id extends ObjectIdRefField(this, User)
  object mode extends BooleanField(this)
 
  object geolatlng extends MongoCaseClassField[Detective, LatLong](this) { override def name = "latlng" }

  def getUserName(): String = {
    this.user_id.obj.map {
      u => u.username.is
    }.getOrElse {
      logger.error("Detective id=" + this.id.toString() + " does not have a correct user_id")
      ""
    }
  }

  def setLocation(lat: Double, lng: Double) {
    this.geolatlng(LatLong(lat,lng)).update
  }

  def setLocationByJson(json: JObject) {
    Detective.update(("_id" -> this.id.is),
      (("user_id" -> this.user_id.is)
        ~ ("mode" -> this.mode.is)
        ~ json))
  }

  def setMode(mode: Boolean) {
    this.mode(mode).update
    if (mode == false) {
      DetectiveInGroup.findByDetective(this).map {
        dig => dig.changeMode(false)
      }
    }
  }

  def delete() = {
    SearchGroup.findByAdmin(this).map {
      group =>
        group.delete
    }
    DetectiveInGroup.findByDetective(this).map {
      dg => dg.delete_!
    }
    SearchGroupMessage.findByDetective(this).map {
      sgm => sgm.delete_!
    }
    this.delete_!
  }

}

object Detective extends Detective with MongoMetaRecord[Detective] with Loggable {
  val geoIdx = Detective.index(_.geolatlng, TwoD)

  def add(user: User, lat: Double, lng: Double): Box[Detective] = {
    findByUser(user) match {
      case Full(d) => Full(d)
      case _ => {
        Detective.createRecord
          .user_id(user.id.is)
          .geolatlng(LatLong(lat,lng))
          .mode(true)
          .saveTheRecord()
      }
    }
  }
  def addUser(user: User) {
    Detective.createRecord
      .user_id(user.id.is)
      .geolatlng(LatLong(0,0))
      .mode(false)
      .saveTheRecord()
  }

  def findByUser(user: User): Box[Detective] = {
    Detective.where(_.user_id eqs user.id.is).fetch().headOption
  }

  def findByMode(mode: Boolean): List[Detective] = {
    Detective.where(_.mode eqs mode).fetch()
  }

}


class SearchGroup extends MongoRecord[SearchGroup] with ObjectIdPk[SearchGroup] with Loggable {
  def meta = SearchGroup
  object name extends StringField(this, 160)
  object description extends StringField(this, 160)
  object num extends IntField(this)
  object admin_id extends ObjectIdRefField(this, Detective)
  def getAdminUsername(): String = {
    this.admin_id.obj.map {
      a => a.getUserName()
    }.getOrElse {
      logger.error("SearchGroup id=" + this.id.is.toString() + " does not have an admin_id")
      ""
    }
  }

  def delete() {
    DetectiveInGroup.findBySearchGroup(this).map {
      dg => dg.delete_!
    }
    SearchGroupMessage.findBySearchGroup(this).map {
      sgm => sgm.delete_!
    }
    this.delete_!
  }
  def edit(name: String, description: String) {
    this.name(name).description(description).update
  }
  def editNum() {

    this.num(DetectiveInGroup.where(_.searchgroup_id eqs this.id.is).and(_.request eqs true).fetch().length).update
  }

  def showMessagesByNumberOfResults(num: Int): List[SearchGroupMessage] = {
    SearchGroupMessage.where(_.searchgroup_id eqs this.id.is).orderDesc(_.id).fetch(num).reverse
  }
  
  def getActiveUsernames():List[String]={
    DetectiveInGroup.where(_.searchgroup_id eqs this.id.is).fetch().filter(_.mode.is == true).map{
      dig => dig.getDetectiveUserName()
    }
  }
}

object SearchGroup extends SearchGroup with MongoMetaRecord[SearchGroup] with Loggable {
  def add(detective: Detective, name: String, description: String): Box[SearchGroup] = {

    val group = SearchGroup.createRecord
      .name(name)
      .description(description)
      .num(1)
      .admin_id(detective.id.is)
      .saveTheRecord()
    group.foreach(DetectiveInGroup.add(detective, _, true))
    group
  }

  def findByName(name: String) = {
    SearchGroup.where(_.name eqs name).fetch()
  }

  def findByDetectiveNotIn(detective: Detective): List[SearchGroup] = {
    SearchGroup.findAll.filter {
      sg => DetectiveInGroup.findByDetectiveAndGroup(detective, sg).isEmpty
    }
  }

  def findByAdmin(detective: Detective): List[SearchGroup] = {
    SearchGroup.where(_.admin_id eqs detective.id.is).fetch()
  }
  
  def findActiveGroups():List[SearchGroup]={
    SearchGroup.findAll.filter{
      sg => 
        sg.getActiveUsernames().size > 0
    }
  }

}

class DetectiveInGroup extends MongoRecord[DetectiveInGroup] with ObjectIdPk[DetectiveInGroup] with Loggable {
  def meta = DetectiveInGroup
  object searchgroup_id extends ObjectIdRefField(this, SearchGroup)
  object detective_id extends ObjectIdRefField(this, Detective)
  object mode extends BooleanField(this)
  object request extends BooleanField(this)
  object blocked extends BooleanField(this)

  def acceptRequest() {
    this.request(true).update
    searchgroup_id.obj.foreach(_.editNum())
  }

  def getDetectiveUserName(): String = {
    this.detective_id.obj.map(_.getUserName()).getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString + " has a problem with detective_id")
      ""
    }
  }

  def blockTheUser() {
    this.blocked(true).mode(false).request(false).update
    this.searchgroup_id.obj.map {
      sg => sg.editNum()
    }.getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString + " has a problem with searchgroup_id")
    }
  }

  def unblockTheUser() {
    this.blocked(false).mode(false).request(true).update
    this.searchgroup_id.obj.map {
      sg => sg.editNum()
    }.getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString + " has a problem with searchgroup_id")
    }
  }

  def delete() {
    this.searchgroup_id.obj.map {
      sg =>
        if (this.detective_id.is == sg.admin_id.is) {
          sg.delete()
        } else {
          this.delete_!
        }
        sg.editNum()
    }

  }

  def changeMode(mode: Boolean) {
    this.mode(mode).update
  }

  def changeModeToTrue() {
    DetectiveInGroup.where(_.detective_id eqs this.detective_id.is).fetch().foreach {
      dg => dg.changeMode(false)
    }
    this.changeMode(true)
  }

  def isDetectiveAdmin: Boolean = {
    this.searchgroup_id.obj.map {
      sg =>
        sg.admin_id.is.toString() == this.detective_id.is.toString()
    }.getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString() + " does not have proper searchgroup_id")
      false
    }
  }

  def getGroupName() = {
    this.searchgroup_id.obj.map {
      sg => sg.name.is
    }.getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString() + " does not have proper searchgroup_id")
      ""
    }
  }

}

object DetectiveInGroup extends DetectiveInGroup with MongoMetaRecord[DetectiveInGroup] with Loggable {
  def add(detective: Detective, group: SearchGroup, accepted: Boolean) = {
    DetectiveInGroup.findByDetectiveAndGroup(detective, group).map {
      dig => Full(dig)
    }.getOrElse {
      val dig = DetectiveInGroup.createRecord
        .detective_id(detective.id.is)
        .searchgroup_id(group.id.is)
        .mode(false)
        .blocked(false)
        .request(accepted)
        .saveTheRecord()

      dig.foreach(d => group.editNum())
      dig
    }
  }

  def findDetectivesInGroup(group: SearchGroup) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.request eqs true).fetch()
  }

  def findBySearchGroup(group: SearchGroup) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).fetch()
  }
  def findByDetective(detective: Detective): List[DetectiveInGroup] = {
    DetectiveInGroup.where(_.detective_id eqs detective.id.is).fetch()
  }

  def findBySearchGroupAndRequest(group: SearchGroup, request: Boolean) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.request eqs request).fetch()
  }
  def findBySearchGroupAndMode(group: SearchGroup, mode: Boolean) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.mode eqs mode).fetch()
  }
  def findByDetectiveAndGroup(detective: Detective, group: SearchGroup) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.detective_id eqs detective.id.is).fetch().headOption
  }
}

class SearchGroupMessage extends MongoRecord[SearchGroupMessage] with ObjectIdPk[SearchGroupMessage] with Loggable {
  def meta = SearchGroupMessage
  object searchgroup_id extends ObjectIdRefField(this, SearchGroup)
  object message extends StringField(this, 160)
  object detective_id extends ObjectIdRefField(this, Detective)

  def getUsername(): String = {
    detective_id.obj.map {
      d => d.getUserName()
    }.getOrElse {
      logger.error("SearchGroupMessage id=" + this.id.is.toString + " does not have a proper detective_id")
      ""
    }
  }

}

object SearchGroupMessage extends SearchGroupMessage with MongoMetaRecord[SearchGroupMessage] with Loggable {
  def add(group: SearchGroup, detective: Detective, message: String) {
    SearchGroupMessage.createRecord
      .detective_id(detective.id.is)
      .searchgroup_id(group.id.is)
      .message(message)
      .saveTheRecord()
  }
  def findBySearchGroup(group: SearchGroup) = {
    SearchGroupMessage.where(_.searchgroup_id eqs group.id.is).fetch()
  }
  def findByDetective(detective: Detective) = {
    SearchGroupMessage.where(_.detective_id eqs detective.id.is).fetch()
  }
}