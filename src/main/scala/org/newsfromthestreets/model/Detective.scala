package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JValue

class Detective extends MongoRecord[Detective] with ObjectIdPk[Detective] with Loggable {
  def meta = Detective
  object user_id extends ObjectIdRefField(this, User)
  object mode extends BooleanField(this)
  object lat extends DoubleField(this)
  object lng extends DoubleField(this)
  def getUserName(): String = {
    this.user_id.obj.map {
      u => u.username.is
    }.getOrElse {
      logger.error("Detective id=" + this.id.toString() + " does not have a correct user_id")
      ""
    }
  }
  
  def setLocation(lat:Double,lng:Double){
    this.lat(lat).lng(lng).update
  }
  
  def setLocationByJson(json:JObject){
     Detective.update(("_id" -> this.id.is),
      (("user_id" -> this.user_id.is)
        ~ ("mode" -> this.mode.is)
        ~ json))
  }
  
  def setMode(mode:Boolean){
    this.mode(mode).update
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
  def addUser(user: User) {
    Detective.createRecord
      .user_id(user.id.is)
      .lat(0)
      .lng(0)
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
  
  def delete(){
    DetectiveInGroup.findBySearchGroup(this).map {
      dg => dg.delete_!
    }
    SearchGroupMessage.findBySearchGroup(this).map {
      sgm => sgm.delete_!
    }
    this.delete_!
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
  def edit(group: SearchGroup, name: String, description: String) {
    SearchGroup.update(("_id" -> group.id.is), (("name" -> name) ~ ("description" -> description) ~ ("num" -> group.num.is)))
  }

  def findByName(name: String) = {
    SearchGroup.where(_.name eqs name).fetch()
  }
  
  def findByDetectiveNotIn(detective:Detective):List[SearchGroup]={
    SearchGroup.findAll.filter{
      sg => DetectiveInGroup.findByDetectiveAndGroup(detective,sg).isEmpty 
    }
  }
  
  def findByAdmin(detective: Detective): List[SearchGroup] = {
    SearchGroup.where(_.admin_id eqs detective.id.is).fetch()
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
  }
  
  def getDetectiveUserName(): String = {
    this.detective_id.obj.map(_.getUserName()).getOrElse {
      logger.error("DetectiveInGroup id=" + this.id.is.toString + " has a problem with detective_id")
      ""
    }
  }
  
  def blockTheUser(){
    this.blocked(true).mode(false).request(false).update
  }
 
  
  def delete(){
    this.searchgroup_id.obj.map{
      sg=>
        if(this.detective_id.is == sg.admin_id.is){
          sg.delete()
        }else{
          this.delete_!
        }
    }
    
  }
  
  def changeMode(mode:Boolean){
    this.mode(mode).update
  }
  
  def changeModeToTrue(){
     DetectiveInGroup.where(_.detective_id eqs this.detective_id.is).fetch().foreach {
      dg => dg.changeMode(false)
    }
    this.changeMode(true)
  }
  
}

object DetectiveInGroup extends DetectiveInGroup with MongoMetaRecord[DetectiveInGroup] with Loggable {
  def add(detective: Detective, group: SearchGroup, accepted: Boolean) {
    DetectiveInGroup.createRecord
      .detective_id(detective.id.is)
      .searchgroup_id(group.id.is)
      .mode(false)
      .blocked(false)
      .request(accepted)
      .saveTheRecord()
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

  def findBySearchGroupAndRequest(group: SearchGroup,request:Boolean) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.request eqs request).fetch()
  }
  def findByDetectiveAndGroup(detective: Detective, group: SearchGroup) = {
    DetectiveInGroup.where(_.searchgroup_id eqs group.id.is).and(_.detective_id eqs detective.id.is).fetch().headOption
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
  def findBySearchGroup(group: SearchGroup) = {
    SearchGroupMessage.where(_.searchgroup_id eqs group.id.is).fetch()
  }
  def findByDetective(detective: Detective) = {
    SearchGroupMessage.where(_.detective_id eqs detective.id.is).fetch()
  }
}