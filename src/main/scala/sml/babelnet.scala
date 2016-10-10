package sml

import java.sql.Connection

import sml.sqlite.{connect, prepareAndQuery, iterableResults, prepareAndExecute}
import sml.knowledge.{Node, TraverserImpl}
import sml.nlp.Token

/**
A module to interact with babelnet
*/
object babelnet
{
	type SynsetId = String

	/**
	Connects to a babelnet database and has useful methods for searching it
	*/
	class BabelnetDB(dbPath:String)
	{
		val conn = connect(dbPath)

		/**
		Returns all the synsets that have the specified lemma within the specified
		top lemmas per synset
		*/
		def synsetSearch(lemma:String, pos:String, limit:Int):Iterable[SynsetId] =
		{
			val results = if(limit == -1)
			{
				val sql = """select l.synset_id 
					from lemma l, synset s
					where l.synset_id = s.id
					and lemma = ? 
					and pos = ?
					and priority <= ?"""

				prepareAndQuery(conn, sql, lemma, pos, limit)
			}
			else
			{
				val sql = """select l.synset_id 
					from lemma l, synset s
					where l.synset_id = s.id
					and lemma = ? 
					and pos = ?"""

				prepareAndQuery(conn, sql, lemma, pos)
			}

			//get all the results
			for(row <- results) yield row.getString("synset_id")
		}

		def synsetSearch(lemma:String, pos:String):Iterable[SynsetId] = synsetSearch(lemma, pos, -1)

		def synsetSearch(phrase:Seq[String], pos:String, limit:Int):Iterable[SynsetId] =
		{
			synsetSearch(toPhraseFromStr(phrase), pos, limit)
		}

		/**
		Returns true if the lemma is in the database
		*/
		def hasPhrase(lemma:String):Boolean = 
		{
			val sql = "select synset_id from lemma where lemma = ?"
			prepareAndQuery(conn, sql, lemma).nonEmpty
		}

		def hasPhrase(span:Seq[String]):Boolean = hasPhrase(toPhraseFromStr(span))

		/**
		Converts a span into a phrase
		*/
		def toPhrase(span:Seq[Token]):String = toPhraseFromStr(span.map(_.word))

		/**
		Returns all the hypernyms associated with the relation id
		*/
		def hypernyms(id:SynsetId, useTransitive:Boolean=false):Iterable[SynsetId] =
		{
			val sql = if(useTransitive)
				"select target from transitive_relation where source = ? and relation_type in (1,40) and (automatic is null or automatic = 0);"
			else
				"select target from relation where source = ? and relation_type in (1,40) and (automatic is null or automatic = 0);"

			//get all the results
			for(row <- prepareAndQuery(conn, sql, id)) yield
			{
				row.getString("target")
			}
		}
	}

	/**
	Converts a phrase into a string
	*/
	def toPhraseFromStr(span:Seq[String]):String = span.mkString("_")

	/**
	A node in the babelnet graph
	*/
	class BabelnetNode(val id:String, dist:Double, val history:List[SynsetId]) extends Node(id, dist)

	/**
	Traverses along the hypernym relationships
	*/
	class HypernymTraverser(val limit:Int, path:String, val blackList:Set[SynsetId]=Set(), val transitive:Boolean=false) extends TraverserImpl[BabelnetNode]
	{
		val db = new BabelnetDB(path)

		/**
		Wraps a paraphrase in a node
		*/
		def init(id:String):Iterable[BabelnetNode] = Seq(new BabelnetNode(id, 0.0, List()))
			
		/**
		Returns true if the id is in the db
		*/
		def hasPhrase(phrase:String):Boolean = db.hasPhrase(phrase)
			
		/**
		Returns a nodes successors
		*/
		def successors(node:BabelnetNode):Iterable[BabelnetNode] =
		{
			val dist = node.score + 1.0

			//if we haven't traversed so far continue looking
			if(dist <= limit)
			{
				val hyp = db.hypernyms(node.id, transitive)

				//TODO remove
				//println(s"${node.id} -> ${hyp.map(h => s"$h:${targetSynsets.getOrElse(h, "")}").mkString(" ")}")

				//if the successors includes something from the blacklist, stop
				if(!hyp.exists(blackList.contains))
					hyp.map(i => new BabelnetNode(i, dist, node.id :: node.history))
				else
					Seq()
			}
			else
				Seq[BabelnetNode]()
		}

		/**
		Look up possible synsets for the given lemma
		*/
		def lemmaSynsets(lemma:String, pos:String, limit:Int):Iterable[SynsetId] =
		{
			db.synsetSearch(lemma, pos, limit)
		}
	}

	/**
	Returns a factory function for hypernym traversers
	*/
	def makeFactory(limit:Int, path:String, blackList:Set[SynsetId]=Set(), transitive:Boolean=false):()=>HypernymTraverser =
	{
		def build():HypernymTraverser = new HypernymTraverser(limit, path, blackList, transitive)

		build
	}

	/**
	 * Returns all the synsets in the database without transative relationships
	 */
	def allSynsets(db:Connection):Iterable[SynsetId] =
	{
		val sql = "select id from synset where id not in (select source from thypernym) limit 100;"

		for(row <- prepareAndQuery(db, sql)) yield
		{
			row.getString("id")
		}
	}

	/**
	 * Inserts a transative relationship into the database
	 */
	def insertTransitveRelation(db:Connection, source:SynsetId, target:SynsetId, relType:Int)
	{
		val sql = "insert into transitive_relation (source, target, relation_type) values (?, ?, ?)"

		prepareAndExecute(db, sql, source, target, relType)
	}

	/**
	Check if the transitive relation still exists
	*/
	def transitiveRelationExists(db:Connection, source:SynsetId, target:SynsetId):Boolean =
	{
		val sql = "select source from transitive_relation where source = ? and target = ?"

		prepareAndQuery(db, sql, source, target).nonEmpty
	}

	//TODO remove
	val targetSynsets = Map("bn:00046516n" -> "person", 
	"bn:00061450n" -> "people",
	"bn:00023235n" -> "country",
	"bn:00023236n" -> "nation",
	"bn:00064607n" -> "profession",
	"bn:11198518n" -> "occupation",
	"bn:00005732n" -> "military",
	"bn:00005704n" -> "weapon",
	"bn:00077301n" -> "unit of time",
	"bn:00055644n" -> "money",
	"bn:00013722n" -> "building",
	"bn:00021286n" -> "company",
	"bn:00059480n" -> "organization",
	"bn:00079675n" -> "vehicle",
	"bn:00005956n" -> "artifact",
	"bn:00058442n" -> "object",
	"bn:00062699n" -> "place"
	)
}
