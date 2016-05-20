package sml

import java.io.File
import scala.collection.JavaConverters._

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{IWord,POS}

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
		val connection = new Dictionary(new File(path))

		/**
		Returns all the entries for the given string
		*/
		def lookupWord(word:String):Iterable[IWord] =
		{
			val pos = Seq(POS.NOUN, POS.VERB, POS.ADVERB, POS.ADJECTIVE)
			
			pos.flatMap(p => connection.getIndexWord(word, p).getWordIDs.asScala.map(connection.getWord))
		}

		/**
		Get all the synonyms of a word
		*/
		def synonyms(word:IWord):Set[IWord] =
		{
			val syn = word.getSynset

			//get all the words associated with this words synset and related synsets
			(syn.getWords.asScala ++ syn.getRelatedSynsets.asScala.flatMap(s => connection.getSynset(s).getWords.asScala)).toSet
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
	class WnSynTraverser(val limit:Int, val db:WnConnection) extends 
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
				(db.synonyms(node.wnWord) ++ db.lexicalForms(node.wnWord)).map(n => new WnNode(n, depth))
			else
				Seq()
		}
	}

	/**
	Represents a node in Wordnet
	*/
	class WnNode(val wnWord:IWord, dist:Double) extends Node(wnWord.getLemma, dist)
}
