package org.newsfromthestreets.model
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common._
import com.foursquare.rogue.Rogue._
import com.foursquare.rogue.IndexedRecord
import net.liftweb.mongodb.BsonDSL._
import java.util.Date
import java.text.SimpleDateFormat
import org.joda.time.DateTime
import org.joda.time.LocalDate
import java.util.Locale
import org.joda.time.format.DateTimeFormat
import net.liftweb.json.JsonAST.JValue
import com.foursquare.rogue.{LatLong,Degrees,IndexModifier}

class ArticleDate extends MongoRecord[ArticleDate] with ObjectIdPk[ArticleDate] {
  def meta = ArticleDate
  object date extends DateTimeField(this)
  def delete(){
    Article.where(_.date_id eqs this.id.is).fetch().foreach(_.delete())
    this.delete_!
  }
}


object ArticleDate extends ArticleDate with MongoMetaRecord[ArticleDate] with Loggable {
  def add() = {
    val ad = ArticleDate.createRecord
    ad.date(Full((new LocalDate).toDateTimeAtStartOfDay()))
    ad.saveTheRecord()
  }
  def findOrAdd(): Box[ArticleDate] = {
    val today = (new LocalDate).toDateTimeAtStartOfDay()
    ArticleDate.where(_.date between (today, today.plusHours(24))).fetch().headOption match {
      case Some(ad) => Full(ad)
      case None => ArticleDate.add()
    }
  }
  
  def findByDateString(dateStr:String)={
     val formatter = new SimpleDateFormat("dd-MM-yy")
     val d = new DateTime(formatter.parse(dateStr))
     ArticleDate.where(_.date between (d,d.plusDays(1))).fetch().headOption
  }
}

class ArticleCategory extends MongoRecord[ArticleCategory] with ObjectIdPk[ArticleCategory] {
  def meta = ArticleCategory
  object name extends StringField(this, 20)
   def delete(){
    Article.where(_.category_id eqs this.id.is).fetch().foreach(_.delete())
    this.delete_!
  }
}

object ArticleCategory extends ArticleCategory with MongoMetaRecord[ArticleCategory] with Loggable {
  def add(name: String) = {
    ArticleCategory.createRecord.name(name).saveTheRecord()
  }
  def findOrAdd(name: String): Box[ArticleCategory] = {
    ArticleCategory.where(_.name eqs name).fetch().headOption match {
      case Some(ac) => Full(ac)
      case None => ArticleCategory.add(name)
    }
  }
  
  def findByName(name:String)={
    ArticleCategory.where(_.name eqs name).fetch().headOption
  }
  
  
}

class Article extends MongoRecord[Article]  with MongoId[Article] with IndexedRecord[Article] {
  def meta = Article

  object title extends StringField(this, 60)
  object user_id extends ObjectIdRefField(this, User)
  object article extends TextareaField(this, 500)
  object date_id extends ObjectIdRefField(this, ArticleDate)
  object category_id extends ObjectIdRefField(this, ArticleCategory)
  object geolatlng extends MongoCaseClassField[Article, LatLong](this) { override def name = "latlng" }
  
  def delete(){
    CommentArticle.where(_.article_id eqs this.id).fetch().foreach(_.delete_!)
    this.delete_!
  }
  
  def edit( user: User, title: String, article: String, category: String, lat: Double, lng: Double) {

    ArticleCategory.findOrAdd(category).map {
      cat =>
        ArticleDate.findOrAdd.map {
          date =>
            this.title(title).article(article).category_id(cat.id.is).date_id(date.id.is).geolatlng(LatLong(lat,lng)).update
        }

    }
  }

}

object Article extends Article with MongoMetaRecord[Article] with Loggable {
  val geoIdx = Article.index(_.geolatlng, TwoD)
  def add(user: User, title: String, article: String, category: String, lat: Double, lng: Double): Box[Article] = {
    val a = Article.createRecord
    a.user_id(user.id.is)
    a.title(title)
    a.article(article)

    val today = (new LocalDate).toDateTimeAtStartOfDay()
    ArticleDate.where(_.date between (today, today.plusDays(1))).fetch().headOption match {
      case Some(ad) => a.date_id(ad.id.is)
      case None => ArticleDate.add().map(ad => a.date_id(ad.id.is)).getOrElse(logger.error("The ArticleDate does not add a new date "))
    }
    ArticleCategory.where(_.name eqs category).fetch().headOption match {
      case Some(ac) => a.category_id(ac.id.is)
      case None => ArticleCategory.add(category).map(ac => a.category_id(ac.id.is)).getOrElse(logger.error("The ArticleCategory does not add a new date "))
    }
    a.geolatlng(LatLong(lat,lng))
    a.saveTheRecord()
  }

  def findByCategory(category:ArticleCategory)={
    Article.where(_.category_id eqs category.id.is).fetch()
  }
  
  def findByCategoryLocation(category:ArticleCategory , lat:Double,long:Double)={
    Article.where(_.category_id eqs category.id.is).and(_.geolatlng near (lat,long,Degrees(0.3))).fetch()
  }
  
  def findByCategoryDateLocation(category:ArticleCategory, date:ArticleDate , lat:Double,long:Double)={
    Article.where(_.category_id eqs category.id.is)
    .and(_.date_id eqs date.id.is)
    .and(_.geolatlng near (lat,long,Degrees(0.3)))
    .fetch()
  }
  
  def findByDate(date:ArticleDate)={
    Article.where(_.date_id eqs date.id.is).fetch()
  }
  def findByDateLocation(date:ArticleDate , lat:Double,long:Double)={
    Article.where(_.date_id eqs date.id.is).and(_.geolatlng near (lat,long,Degrees(0.3)))
    .fetch()
  }
  
  def findByCategoryDate(category:ArticleCategory,date:ArticleDate )={
    Article.where(_.category_id eqs category.id.is)
    .and(_.date_id eqs date.id.is).fetch()
  }
  
  def findByLocation(lat:Double,long:Double)={
    Article.where(_.geolatlng near (lat,long,Degrees(0.3)))
    .fetch()
  }
  
  
  def listByCategoryDateLocation(articleDate: Box[ArticleDate],articleCategory: Box[ArticleCategory]
  ,lat:Box[Double],lng:Box[Double]):List[Article]={
     var articleList: List[Article] = List()
      if (articleDate.isEmpty) {
        if (articleCategory.isEmpty) {
          if (lat.isEmpty || lng.isEmpty) {
            articleList = Article.findAll
          } else {
            articleList = Article.findByLocation(lat.get, lng.get)
          }
        } else {
          if (lat.isEmpty || lng.isEmpty) {
            articleList = Article.findByCategory(articleCategory.get)
          } else {
            articleList = Article.findByCategoryLocation(articleCategory.get, lat.get, lng.get)
          }
        }
      } else {
        if (articleCategory.isEmpty) {
          if (lat.isEmpty || lng.isEmpty) {
            articleList = Article.findByDate(articleDate.get)
          } else {
            articleList = Article.findByDateLocation(articleDate.get, lat.get, lng.get)
          }
        } else {
          if (lat.isEmpty || lng.isEmpty) {
            articleList = Article.findByCategoryDate(articleCategory.get, articleDate.get)
          } else {
            articleList = Article.findByCategoryDateLocation(articleCategory.get, articleDate.get, lat.get, lng.get)
          }
        }
      }
     articleList
  }
  
 

  def createFromJson(user: User, json: JValue) = {
    Article.createRecord.user_id(user.id.is).setFieldsFromJValue(json)
  }
  
  def findNearestArticle(lat:Double,lng:Double,degree:Double)={
    Article.where(_.geolatlng near (lat,lng,Degrees(degree))).fetch()
  }
  

}

class CommentArticle extends MongoRecord[CommentArticle] with ObjectIdPk[CommentArticle] {
  def meta = CommentArticle

  object article_id extends ObjectIdRefField(this, Article)
  object user_id extends ObjectIdRefField(this, User)
  object message extends TextareaField(this, 500)

}

object CommentArticle extends CommentArticle with MongoMetaRecord[CommentArticle] {
  def add(user: User, article: Article, message: String) = {
    CommentArticle.createRecord
      .user_id(user.id.is)
      .article_id(article.id)
      .message(message)
      .saveTheRecord()
  }

  def showByNumberOfResults(art: Article, num: Int): List[CommentArticle] = {
    CommentArticle.where(_.article_id eqs art.id).orderDesc(_.id).fetch(num).reverse
  }

}




