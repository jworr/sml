package sml

import java.sql.Connection

import sml.sqlite.{iterableResults, prepareAndQuery}

//for testing
import sml.sqlite.connect

/**
A library for accessing the paraphrase database
*/
object ppdb
{
	/**
	Returns true if the phrase is in the database
	*/
	def hasPhrase(db:Connection, phrase:String): Boolean =
	{
		val sql = "select count(*) from similarity where source = ?"

		//execute the query
		val result = prepareAndQuery(db, sql, phrase.toLowerCase)

		//fetch the first row
		result.next

		//check if any paraphases match
		return result.getInt(1) > 0
	}

	/**
	Returns all similar phrases to the given phrase along with a similarity score
	*/
	def similarPhrases(db:Connection, phrase:String, agiga:Boolean = true): Iterable[(String, Double)] =
	{
		val query = if(agiga) 
			"select target, agiga_sim, pos from similarity where source = ?"
		else 
			"select target, google_sim, pos from similarity where source = ?"

		//excute the query
		val result = prepareAndQuery(db, query, phrase.toLowerCase)

		//build and return the results
		for(row <- result) yield
		{
			(row.getString(1), row.getDouble(2))
		}
	}

	def main(args:Array[String])
	{
		val db = connect("/home/walker/Data/ppdb/small_no_num.db")

		for( result <- similarPhrases(db, "arrested") )
		{
			println(result._1)
		}
	}
}
