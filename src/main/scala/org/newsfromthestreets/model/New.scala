package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import org.bson.types.ObjectId
import net.liftweb.mongodb.BsonDSL._
import java.util.Date
import java.text.SimpleDateFormat
import org.joda.time.DateTime
import net.liftweb.json.JsonAST.JValue
import net.liftweb.mongodb.JObjectParser
import net.liftweb.json.JsonAST.JObject

class Article extends MongoRecord[Article] with ObjectIdPk[Article] {
  def meta = Article

  object title extends StringField(this, 60)
  object user_id extends ObjectIdRefField(this, User)
  object article extends TextareaField(this, 500)
  object date extends DateTimeField(this)
  object lat extends DoubleField(this)
  object lng extends DoubleField(this)

}

object Article extends Article with MongoMetaRecord[Article] with Loggable {
  def add(user: User, title: String, article: String, lat: Double, lng: Double): Box[Article] = {
     val a= Article.createRecord
      a.user_id(user.id.is)
      a.title(title)
      
      a.article(article)
      a.date(Full(new DateTime))
      a.lat(lat)
      a.lng(lng)
      a.saveTheRecord()
  }
  
  def edit(id: String, user: User, title: String, article: String, lat: Double, lng: Double) {
    Article.update(("_id" -> id), (("user_id" -> user.id.is) ~ ("article" -> article) ~ ("title" -> title) ~ ("lat" -> lat) ~ ("lng" -> lng)))
  }

  def listByDate(date: String): List[Article] = {
    try {
      val formatter = new SimpleDateFormat("dd-MM-yy")
      val d = new DateTime(formatter.parse(date))
      Article.where(_.date after d).fetch()
    } catch {
      case e =>
        logger.error(e.getMessage())
        List()

    }
  }
  
  def createFromJson(user:User,json:JValue)={
    Article.createRecord.user_id(user.id.is).setFieldsFromJValue(json)   
  }
  
}

class CommentNew extends MongoRecord[CommentNew] with ObjectIdPk[CommentNew] {
  def meta = CommentNew

  object article_id extends ObjectIdRefField(this, Article)
  object user_id extends ObjectIdRefField(this, User)
  object message extends TextareaField(this, 500)

}

object CommentNew extends CommentNew with MongoMetaRecord[CommentNew] {
  def add(user: User, article: Article, message: String) = {
    CommentNew.createRecord
      .user_id(user.id.is)
      .article_id(article.id.is)
      .message(message)
      .saveTheRecord()
  }

}




