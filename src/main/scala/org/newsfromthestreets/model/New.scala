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
import org.joda.time.LocalDate
import java.util.Locale
import org.joda.time.format.DateTimeFormat

class ArticleDate extends MongoRecord[ArticleDate] with ObjectIdPk[ArticleDate] {
  def meta = ArticleDate
  object date extends DateTimeField(this)

}

object ArticleDate extends ArticleDate with MongoMetaRecord[ArticleDate] with Loggable {
  def add() = {
    val ad = ArticleDate.createRecord
    ad.date(Full((new LocalDate).toDateTimeAtStartOfDay()))
    ad.saveTheRecord()
  }
  def findOrAdd():Box[ArticleDate]={
    val today = (new LocalDate).toDateTimeAtStartOfDay()
    ArticleDate.where(_.date between (today, today.plusDays(1))).fetch().headOption match {
      case Some(ad) => Full(ad)
      case None => ArticleDate.add()
    }
  }
}

class ArticleCategory extends MongoRecord[ArticleCategory] with ObjectIdPk[ArticleCategory] {
  def meta = ArticleCategory
  object name extends StringField(this, 20)
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
}

class Article extends MongoRecord[Article] with ObjectIdPk[Article] {
  def meta = Article

  object title extends StringField(this, 60)
  object user_id extends ObjectIdRefField(this, User)
  object article extends TextareaField(this, 500)
  object date_id extends ObjectIdRefField(this, ArticleDate)
  object category_id extends ObjectIdRefField(this, ArticleCategory)
  object lat extends DoubleField(this)
  object lng extends DoubleField(this)

}

object Article extends Article with MongoMetaRecord[Article] with Loggable {
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
    a.lat(lat)
    a.lng(lng)
    a.saveTheRecord()
  }

  def edit(id: String, user: User, title: String, article: String, category: String, lat: Double, lng: Double) {
   
    ArticleCategory.findOrAdd(category).map {
      cat =>
        ArticleDate.findOrAdd.map{
          date =>
            Article.update(("_id" -> id), (("user_id" -> user.id.is) 
            ~ ("category_id" -> cat.id.is) 
            ~ ("article" -> article) 
            ~ ("title" -> title) 
            ~ ("date_id" -> date.id.is)
            ~ ("lat" -> lat) 
            ~ ("lng" -> lng)))
        }
        

    }
  }

  def listByDate(date: String): List[Article] = {
    if (date != "All") {
      try {
        val formatter = new SimpleDateFormat("dd-MM-yy")
        val d = new DateTime(formatter.parse(date))
        ArticleDate.where(_.date after d).fetch().map {
          ad => Article.where(_.date_id eqs ad.id.is).fetch().head
        }
      } catch {
        case e =>
          logger.error(e.getMessage())
          List()

      }
    } else {
      Article.findAll
    }
  }

  def listByCategory(category: String): List[Article] = {
    ArticleCategory.where(_.name eqs category).fetch().map {
      ac => Article.where(_.category_id eqs ac.id.is).fetch().head
    }
  }

  def listByCategoryAndDate(category: Box[String], date: Box[String]): List[Article] = {
    if (category.isEmpty) {
      if (date.isEmpty) {
        Article.findAll
      } else {
        Article.listByDate(date.get)
      }
    } else {
      if (date.isEmpty) {
        Article.listByCategory(category.get)
      } else {
        var fmt = DateTimeFormat.forPattern("dd-MM-yy");
        var ls : List[Article] = List()
        for {
          dat <- date
          ad <- {
            val d = fmt.parseDateTime(dat)
            ArticleDate.where(_.date between (d, d.plusDays(1))).fetch().headOption
          }
          ac <- ArticleCategory.where(_.name eqs category.get).fetch().headOption
        } yield {
          ls= Article.where(_.date_id eqs ad.id.is)
            .and(_.category_id eqs ac.id.is).fetch()
        } 
        ls
      }
    }
  }

  def createFromJson(user: User, json: JValue) = {
    Article.createRecord.user_id(user.id.is).setFieldsFromJValue(json)
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
      .article_id(article.id.is)
      .message(message)
      .saveTheRecord()
  }

  def showByNumberOfResults(art: Article, num: Int): List[CommentArticle] = {
    CommentArticle.where(_.article_id eqs art.id.is).orderAsc(_.id).fetch(num)
  }

}




