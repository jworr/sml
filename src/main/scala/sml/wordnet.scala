package sml

import java.io.File
import scala.collection.JavaConverters._

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{IWord,POS,ISynsetID,Pointer,ISynset}

import sml.knowledge.{Traverser, Node, TraverserImpl}

/**
 * A library for interacting with wordnet
 */
object wordnet 
{
	/**
	A connection to the wordnet database
	*/
	class WnConnection(val path:String)
	{
		val connection = 
		{
			val dict = new Dictionary(new File(path))
			dict.open
			dict
		}

		/**
		Returns all the entries for the given string
		*/
		def lookupWord(word:String, posId:Option[POS]=None):Iterable[IWord] =
		{
			val pos = posId match
			{
				case p:Some[POS] => Seq(p.get)
				case _ => Seq(POS.NOUN, POS.VERB, POS.ADVERB, POS.ADJECTIVE)
			}
		
			pos.map(connection.getIndexWord(word.replace(DELIM,WN_DELIM), _)).filter(_ != null).flatMap(_.getWordIDs.asScala.map(connection.getWord))
		}

		def lookupWord(word:Word):Iterable[IWord] = lookupWord(word.word, Some(word.pos))

		/**
		Get all the synonyms of a word
		*/
		def relatedWords(word:Word, relation:RelationFollower):Set[Word] =
		{
			expand(lookupWord(word), relation)
		}	

		/**
		Get all the synonyms of a word from the top k synsets
		*/
		def topRelatedWords(word:Word, k:Int, relation:RelationFollower):Set[Word] =
		{
			expand(lookupWord(word).take(k), relation)
		}

		/**
		Returns the related synsets based on a relationship pointer
		*/
		def relatedSynsets(rel:Pointer):IWord => Iterable[ISynset] =
		{
			def followRelation(word:IWord):Iterable[ISynset] =
			{
				word.getSynset.getRelatedSynsets(rel).asScala.map(connection.getSynset)
			}

			followRelation
		}

		/**
		Expands words via a relationship
		*/
		def expand(words:Iterable[IWord], relation:RelationFollower):Set[Word] =
		{
			words.flatMap(relation.relatedSynsets).flatMap(synsetToWords).toSet
		}	

		/**
		Returns different lexical forms of the word
		*/
		def lexicalForms(word:IWord):Set[IWord] = 
		{
			word.getRelatedWords.asScala.map(connection.getWord).toSet
		}

		override def toString:String = path
	}

	/**
	Traverses wordnet according to a given relation
	*/
	trait RelationFollower
	{
		def relatedSynsets(word:IWord):Iterable[ISynset]
	}

	/**
	Returns the synset related to the word's synonyms
	*/
	object Synonym extends RelationFollower
	{
		def relatedSynsets(word:IWord):Iterable[ISynset] = Seq(word.getSynset)
	}

	/**
	Returns the synsets based on the given relationship pointer	
	*/
	class SynsetRelation(val db:WnConnection, val relationType:Pointer) extends RelationFollower
	{
		def relatedSynsets(word:IWord):Iterable[ISynset] =
		{
			word.getSynset.getRelatedSynsets(relationType).asScala.map(db.connection.getSynset)
		}
	}

	/**
	A collection of relationships
	*/
	class RelationGroup(val relations:Iterable[RelationFollower])
	{
		def relatedSynsets(word:IWord):Iterable[ISynset] =
		{
			relations.flatMap(_.relatedSynsets(word))
		}
	}

	/**
	The Hypernym relation
	*/
	class Hypernym(db:WnConnection) extends SynsetRelation(db, Pointer.HYPERNYM)

	/**
	The Instance-Hypernym relation
	*/
	class InstanceHypernym(db:WnConnection) extends SynsetRelation(db, Pointer.HYPERNYM_INSTANCE)

	/**
	The Hyponym relation
	*/
	class Hyponym(db:WnConnection) extends SynsetRelation(db, Pointer.HYPONYM)

	/**
	The Instance-Hypernym relation
	*/
	class InstanceHyponym(db:WnConnection) extends SynsetRelation(db, Pointer.HYPONYM_INSTANCE)

	/**
	The Holonym relation - a collection to a part
	*/
	class PartHolonym(db:WnConnection) extends SynsetRelation(db, Pointer.HOLONYM_PART)

	/**
	The Holonym relation - a collection to a part
	*/
	class MemberHolonym(db:WnConnection) extends SynsetRelation(db, Pointer.HOLONYM_MEMBER)

	/**
	The Meteronym relation - a part to a collection
	*/
	class SubstanceMeronym(db:WnConnection) extends SynsetRelation(db, Pointer.MERONYM_SUBSTANCE)

	/**
	The Entailment relation, similar to implication
	*/
	class Entailment(db:WnConnection) extends SynsetRelation(db, Pointer.ENTAILMENT)

	/**
	Traverses the synonym relations in PPDB
	*/
	class WnTraverser(val limit:Int, val db:WnConnection, 
		val relation:RelationFollower,
		val topK:Option[Int]=None) extends 
		TraverserImpl[WnNode]
	{
		/**
		Wraps a paraphrase in a node
		*/
		def init( word:String ):Iterable[WnNode] = 
		{
			db.lookupWord(word).map(iwordToWord).map(w => new WnNode(w, 0.0))
		}
	
		def hasPhrase(word:String):Boolean = db.lookupWord(word).nonEmpty

		/**
		Returns a nodes successors
		*/
		def successors(node:WnNode):Iterable[WnNode] = 
		{
			val depth = node.score + 1

			//stop if we hit out depth
			if(depth <= limit)
			{
				val syn = topK match
				{
					case n:Some[Int] => db.topRelatedWords(node.word,n.get, relation)
					case _ => db.relatedWords(node.word, relation)
				}

				//TODO remove
				//println(s"next, depth $depth: ${syn.mkString("; ")}")

				syn.map(n => new WnNode(n, depth))
			}
			else
				Seq()
		}

		override def relatedPhrases(word:String):Set[String] =
		{
			super.relatedPhrases(word).map(_.replace(WN_DELIM,DELIM))
		}
	}

	case class Word(val word:String, val pos:POS)

	/**
	Converets a synset to a collection of words
	*/
	def synsetToWords(syn:ISynset):Iterable[Word] =
	{
		syn.getWords.asScala.map(w => Word(w.getLemma, POS.getPartOfSpeech(syn.getType)))
	}

	def iwordToWord(word:IWord):Word =
	{
		Word(word.getLemma, POS.getPartOfSpeech(word.getSynset.getType))
	}

	/**
	Represents a node in Wordnet
	*/
	class WnNode(val word:Word, dist:Double) extends Node(word.word, dist)
	{
		override def equals(other:Any): Boolean = other match
		{
			case that:WnNode=> that.word == word 
			case _ => false
		}
		
		override def hashCode:Int = word.hashCode 
	}

	val WN_DELIM = "_"
	val DELIM = " "

	/**
	Does synonym lookups
	*/
	def main(args:Array[String])
	{
		val db = new WnConnection(args(0))
		val word = args(1)
		val depth = args(2).toInt

		val trav = new WnTraverser(depth, db, Synonym)

		println("Synonyms:")
		println(trav.relatedPhrases(word).mkString("\n"))

		val trav2 = new WnTraverser(depth, db, new Hypernym(db))

		println("--------Hypernyms---------")
		println(trav2.relatedPhrases(word).mkString("\n"))
	}
}
