package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JValue

class Detective extends MongoRecord[Detective] with ObjectIdPk[Detective] {
  def meta = Detective
  object user_id extends ObjectIdRefField(this, User)
  object mode extends BooleanField(this)
  object lat extends DoubleField(this)
  object lng extends DoubleField(this)
}

object Detective extends Detective with MongoMetaRecord[Detective] with Loggable {
  def add(user: User, lat: Double, lng: Double): Box[Detective] = {
    findByUser(user) match {
      case Full(d) => Full(d)
      case _ => {
        Detective.createRecord
          .user_id(user.id.is)
          .lat(lat)
          .lng(lng)
          .mode(true)
          .saveTheRecord()
      }
    }
  }
  def addUser(user:User){
    Detective.createRecord
          .user_id(user.id.is)
          .lat(0)
          .lng(0)
          .mode(false)
          .saveTheRecord()
  }
  def getLocation(detective: Detective, lat: Double, lng: Double) {
    Detective.update(("_id" -> detective.id.is),
      (("user_id" -> detective.user_id.is)
        ~ ("mode" -> detective.mode.is)
        ~ ("lat" -> lat)
        ~ ("lng" -> lng)))

  }
  def getLocationByJson(detective:Detective, json:Map[String,Double]){
    Detective.update(("_id" -> detective.id.is),
      (("user_id" -> detective.user_id.is)
        ~ ("mode" -> detective.mode.is)
        ~ json))
  }
  def setMode(detective: Detective, is: Boolean) {
    Detective.update(("_id" -> detective.id.is),
      (("user_id" -> detective.user_id.is)
        ~ ("mode" -> is)
        ~ ("lat" -> detective.lat.is)
        ~ ("lng" -> detective.lng.is)))
  }

  def findByUser(user: User): Box[Detective] = {
    Detective.where(_.user_id eqs user.id.is).fetch().headOption
  }
  
  def findByMode(mode:Boolean):List[Detective] ={
    Detective.where(_.mode eqs mode).fetch()
  }
}

class SearchGroup extends MongoRecord[SearchGroup] with ObjectIdPk[SearchGroup] {
  def meta = SearchGroup
  object name extends StringField(this, 160)
  object description extends StringField(this, 160)
  object num extends IntField(this)
  object admin_id extends ObjectIdRefField(this, Detective)
}

object SearchGroup extends SearchGroup with MongoMetaRecord[SearchGroup] with Loggable {
  def add(user:User,name: String, description: String) = {
    SearchGroup.createRecord
      .name(name)
      .description(description)
      .num(1)
      .admin_id(user.id.is)
      .saveTheRecord()
  }
  def edit(group: SearchGroup, name: String, description: String) {
    SearchGroup.update(("_id" -> group.id.is), (("name" -> name) ~ ("description" -> description) ~ ("num" -> group.num.is)))
  }
}

class DetectiveInGroup extends MongoRecord[DetectiveInGroup] with ObjectIdPk[DetectiveInGroup] {
  def meta = DetectiveInGroup
  object searchgroup_id extends ObjectIdRefField(this, SearchGroup)
  object detective_id extends ObjectIdRefField(this, Detective)
}

object DetectiveInGroup extends DetectiveInGroup with MongoMetaRecord[DetectiveInGroup] with Loggable {
  def add(detective: Detective, group: SearchGroup) {
    DetectiveInGroup.createRecord
      .detective_id(detective.id.is)
      .searchgroup_id(group.id.is)
      .saveTheRecord()
  }

}

class SearchGroupMessage extends MongoRecord[SearchGroupMessage] with ObjectIdPk[SearchGroupMessage] {
  def meta = SearchGroupMessage
  object searchgroup_id extends ObjectIdRefField(this, SearchGroup)
  object message extends StringField(this, 160)
  object detective_id extends ObjectIdRefField(this, Detective)
}

object SearchGroupMessage extends SearchGroupMessage with MongoMetaRecord[SearchGroupMessage] with Loggable {
  def add(group: SearchGroup, detective: Detective, message: String) {
    SearchGroupMessage.createRecord
      .detective_id(detective.id.is)
      .searchgroup_id(group.id.is)
      .message(message)
      .saveTheRecord()
  }
}