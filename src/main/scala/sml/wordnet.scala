package sml

import java.io.File
import scala.collection.JavaConverters._

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{IWord,POS,ISynsetID}

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
		def lookupWord(word:String):Iterable[IWord] =
		{
			val pos = Seq(POS.NOUN, POS.VERB, POS.ADVERB, POS.ADJECTIVE)
		
			pos.map(connection.getIndexWord(word.replace(DELIM,WN_DELIM), _)).filter(_ != null).flatMap(_.getWordIDs.asScala.map(connection.getWord))
		}

		/**
		Get all the synonyms of a word
		*/
		def synonyms(word:IWord):Set[IWord] =
		{
			synsetWords(word, word.getSynset.getRelatedSynsets.asScala)
		}

		/**
		Get all the synonyms of a word from the top k synsets
		*/
		def topKSynonyms(word:IWord, k:Int):Set[IWord] =
		{
			synsetWords(word, word.getSynset.getRelatedSynsets.asScala.take(k))
		}

		/**
		Returns all the words associated with the word and given synsets
		*/
		def synsetWords(word:IWord, synsets:Iterable[ISynsetID]):Set[IWord] =
		{
			//get all the words associated with this words synset and related synsets
			(synsets.map(connection.getSynset) ++ Seq(word.getSynset)).flatMap(_.getWords.asScala).toSet
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
	Traverses the synonym relations in PPDB
	*/
	class WnSynTraverser(val limit:Int, val db:WnConnection, val topK:Option[Int]=None) extends 
		TraverserImpl[WnNode]
	{
		/**
		Wraps a paraphrase in a node
		*/
		def init( word:String ):Iterable[WnNode] = 
		{
			db.lookupWord(word).map(w => new WnNode(w, 0.0))
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
					case n:Some[Int] => db.topKSynonyms(node.wnWord,n.get)
					case _ => db.synonyms(node.wnWord)
				}

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

	/**
	Represents a node in Wordnet
	*/
	class WnNode(val wnWord:IWord, dist:Double) extends Node(wnWord.getLemma, dist)

	val WN_DELIM = "_"
	val DELIM = " "
}
