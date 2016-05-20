package sml

import java.sql.Connection

import sml.knowledge.{Traverser, Node, TraverserImpl}
import sml.sqlite.{iterableResults, prepareAndQuery}

/**
A library for accessing the paraphrase database
*/
object ppdb
{
	/**
	Returns true if the phrase is in the database
	*/
	def phraseInDB(db:Connection)(phrase:String): Boolean =
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
	def similarPhrases(db:Connection)(phrase:String, agiga:Boolean = true): Iterable[(String, Double)] =
	{
		val query = if(agiga) 
			"select target, agiga_sim, pos from similarity where source = ?"
		else 
			"select target, google_sim, pos from similarity where source = ?"

		val source = phrase.toLowerCase

		//excute the query
		val result = prepareAndQuery(db, query, source)

		//build and return the results
		val sim = for(row <- result) yield
		{
			(row.getString(1), row.getDouble(2))
		}

		return sim ++ List( (source, 1.0) )
	}

	/**
	Return phrases with syntatic usage which are connected to the given phrase
	*/
	def neighborsWithSyn(db:Connection, phrase:String): Iterable[(String, Set[String])] =
	{
		val query = "select target, pos from similarity where source = ?"

		//execute and return the results
		val results = prepareAndQuery(db, query, phrase.toLowerCase).map(r => (r.getString(1), r.getString(2))).groupBy(p => p._1)

		//build groups
		for( (phrase, tuple) <- results ) yield
		{
			(phrase, tuple.map(_._2).toSet)
		}
	}

	/**
	Returns all the syntatic usages for the phrase
	*/
	def syntaticRules(db:Connection, phrase:String):Set[String] =
	{
		val query = "select pos from similarity where source = ?"

		prepareAndQuery(db, query, phrase.toLowerCase).map(_.getString(1)).toSet
	}

	/**
	Returns only those phrases which are unique neighbors of the given phrase
	*/
	def neighbors(db:Connection, phrase:String): Iterable[String] =
	{
		val query = "select distinct target from similarity where source = ?"
	
		//execute the query and return the results
		return prepareAndQuery(db, query, phrase.toLowerCase).map(_.getString(1))
	}

	/**
	Returns the similarity of two phrases base on the overlap in their neighbors
	*/
	def adjSim(db:Connection, left:String, right:String): Double =
	{
		val leftNeig = neighbors(db, left).toSet
		val rightNeig = neighbors(db, right).toSet
		val unionSize = leftNeig.union(rightNeig).size.toDouble
	
		if(unionSize > 0.0)
			leftNeig.intersect(rightNeig).size.toDouble / unionSize
		else
			0.0
	}

	/**
	A phrase in PPDB
	*/
	class PPDBNode(phrase:String, score:Double, val synRules:Set[String]) 
		extends Node(phrase, score)
	
	/**
	Defines a traverser over PPDB
	*/
	abstract class PPDBTraverser(val db:Connection) 
		extends TraverserImpl[PPDBNode]
	{
		def hasPhrase(phrase:String):Boolean = phraseInDB(db)(phrase)
	}

	/**
	Traverses PPDB based on distance from the starting point
	*/
	case class DistTraverser(val limit:Int, conn:Connection) extends PPDBTraverser(conn)
	{
		def init(phrase:String) = Seq(new PPDBNode(phrase,0.0,syntaticRules(conn, phrase)))

		/**
		Returns all nodes one step away from the current node
		*/
		def successors(node:PPDBNode):Iterable[PPDBNode] =
		{
			val newScore = node.score + 1
			
			//get the neighors we are not beyond our limit
			if(newScore <= limit)
				neighborsWithSyn(db, node.phrase).map(s => new PPDBNode(s._1, newScore, s._2))
			else
				List[PPDBNode]()
		}
	}

	/**
	Traverses PPDB based on similarity
	*/
	case class SimTraverser(val limit:Double, conn:Connection) extends PPDBTraverser(conn)
	{
		def init(phrase:String) = Seq(new PPDBNode(phrase, 1.0, syntaticRules(conn, phrase)))

		/**
		Returns all nodes that are within a similarity threshold of the the start
		*/
		def successors(node:PPDBNode):Iterable[PPDBNode] =
		{
			neighborsWithSyn(db, node.phrase).map(s => new PPDBNode(s._1, node.score * adjSim(db, s._1, node.phrase), s._2)).filter(_.score >= limit)
		}
	}
}
