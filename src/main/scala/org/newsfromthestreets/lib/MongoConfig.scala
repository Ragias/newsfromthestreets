package org.newsfromthestreets.lib

import net.liftweb._
import common._
import json._
import mongodb._
import util.Props

object MongoConfig extends Loggable{
	def init(){
	   MongoDB.defineDb(
        DefaultMongoIdentifier,
        MongoAddress(MongoHost(), "newsfromthestreet")
      )
	}
}