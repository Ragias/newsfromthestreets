package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import org.bson.types.ObjectId
import net.liftweb.mongodb.BsonDSL._
class Article extends MongoRecord[Article] with ObjectIdPk[Article] {
  def meta = Article

  object title extends StringField(this, 60)
  object user_id extends ObjectIdRefField(this, User)
  object article extends TextareaField(this, 500)
  object date extends DateField(this)
  object lat extends DoubleField(this)
  object lng extends DoubleField(this)

}

object Article extends Article with MongoMetaRecord[Article] {
  def add(user: User, title: String, article: String, lat: Double, lng: Double): Box[Article] = {
    Article.createRecord
      .user_id(user.id.is)
      .title(title)
      .article(article)
      .date(new java.util.Date)
      .lat(lat)
      .lng(lng)
      .saveTheRecord()
  }
  def edit(id:String,user:User,title:String,article:String,lat:Double,lng:Double){
    Article.update(("_id"->id), (("user_id" -> user.id.is) ~ ("article" -> article) ~ ("title" -> title) ~ ("lat" -> lat) ~ ("lng" -> lng)))
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




