package sml.nlp

import scala.math.Ordered
import scala.collection.mutable.ArrayBuffer
import java.io.FileInputStream
import opennlp.tools.chunker.{ChunkerModel, ChunkerME}

/**
Loads the OpenNLP chunker
*/
object chunker
{
	val model = new ChunkerME( new ChunkerModel( new FileInputStream("en-chunker.bin") ) )

	/**
	A grouping over tokens
	*/
	class Chunk(val tokens:Seq[Token], val chunkType:String) extends Seq[Token] with Ordered[Chunk]
	{
		/**
		Returns true if the chunk contains the token
		*/
		def hasToken(token:Token):Boolean = tokens.contains(token)

		/**
		Returns true if the chunk is a noun phrase
		*/
		def isNounPhrase:Boolean = chunkType.contains("NP")

		/**
		Returns true if the chunk is a verb phrase
		*/
		def isVerbPhrase:Boolean = chunkType.contains("VP")

		/**
		Determines if this Chunk matches the given type
		*/
		def matchesType(typeTag:String):Boolean =
		{
			//if the given type is a POS tag, then make sure one of the tokens
			//matches that pos
			if(POS.contains(typeTag))
			{
				tokens.exists(t => t.pos == typeTag)
			}
			//else compare it against the chunk type
			else
			{
				typeTag == chunkType
			}
		}

		/**
		Returns a Range (span) for the chunk
		*/
		def toSpan:Range = Range(this.head.id, this.last.id+1)

		def apply(index:Int):Token = tokens(index)

		def length:Int = tokens.size

		def iterator:Iterator[Token] = tokens.iterator

		override def toString:String =
		{
			s"$chunkType: " + tokens.map(_.word).mkString(" ")
		}

		override def equals(other:Any):Boolean = other match
		{
			case that:Chunk => tokens == that.tokens && chunkType == that.chunkType
			case _ => false
		}

		override def hashCode = tokens.hashCode + (7 * chunkType.hashCode)

		def compare(that:Chunk):Int = tokens.head.id.compare(that.tokens.head.id)
	}

	/**
	Returns chunks over the sentence
	*/
	def chunkSentence(sent:Sentence):Seq[Chunk] =
	{
		//tag the sentence as chunks
		val tags = model.chunk(sent.tokens.map(_.word).toArray, sent.tokens.map(_.pos).toArray)

		var tagged = sent.tokens.zip(tags)
		var current = tagged.head
		val results = new ArrayBuffer[Chunk]()

		//split the tokens into chunks based on their tags
		while(!tagged.isEmpty)
		{
			current = tagged.head
			tagged = tagged.tail
			
			//get all the tokens in the chunk
			val (chunk, rest) = tagged.span(p => p._2.startsWith("I"))
	
			//add the chunk to the results
			results += new Chunk( (Seq(current) ++ chunk).map(_._1).toSeq, tagType(current._2) )
			
			//advance to the result of the tokens
			tagged = rest
		}

		return results
	}

	/**
	Strips the prefix tag if any which determines membership in a chunk
	rather than type e.g. B-NP, I-NP, B-VP
	*/
	def tagType( tag:String ):String = if(tag.contains("-")) tag.split("-")(1) else tag

	/**
	Debug the object
	*/
	def main(args:Array[String])
	{
		val path = "/home/walker/Data/ERE/data/stanford3/cmptxt/AFP_ENG_20100414.0615.cmp.txt.xml"

		val doc = parseDoc(path)

		for(sentence <- doc.sentences)
		{
			println("Sentence: " + sentence.tokens.map(_.word).mkString(" "))
			println(chunkSentence(sentence).mkString("\n"))
		}
	}
}
