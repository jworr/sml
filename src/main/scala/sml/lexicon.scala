package sml

import sml.learn.{Instance,LabeledInstance}

/**
A sparse bag-of-words style collection
*/
object lexicon
{
	/**
	A sparse collection of strings with a binary variable interface
	*/
	class Lexicon[C](source:Seq[Iterable[String]], val domain:Dictionary, labels:Seq[C]) 
		extends Iterable[LabeledInstance[C]]
	{
		val data = source.zip(labels).map(p => domain.makeLabeledInstance(p._1, p._2))

		def iterator:Iterator[LabeledInstance[C]] = data.iterator
	}

	/**
	A mapping between Strings and indexes
	*/
	class Dictionary(val domain:Set[String])
	{
		val wordToIndex = domain.zipWithIndex.toMap

		/**
		Makes a sparse set out of a collection of strings
		*/
		def makeSparse(data:Iterable[String]):Set[Int] =
		{
			data.map(s => wordToIndex.getOrElse(s,-1)).filter(i => i != -1).toSet
		}

		/**
		Creates an instance based on the dictionary's domain
		*/
		def makeInstance(data:Iterable[String]):SparseInstance = new SparseInstance(makeSparse(data), size)

		/**
		Creates a labeled instance based on the dictionary's domain
		*/
		def makeLabeledInstance[C](data:Iterable[String], label:C):LabeledSparseInstance[C] =
		{
			new LabeledSparseInstance(makeSparse(data), size, label)
		}

		def size:Int = domain.size
	}

	/**
	A sparse instance
	*/
	class SparseInstance(val data:Set[Int], val domainSize:Int) extends Instance
	{
		/**
		Returns the feature value
		*/
		def featureAt(index:Int):Double = 
		{
			if(index >= domainSize) throw new FeatureException(s"No feature at $index")

			if(data.contains(index)) 1.0 else 0.0
		}

		/**
		Returns all the features
		*/
		def features:Iterable[Double] = Range(0,domainSize).map(i => featureAt(i))

		def size:Int = domainSize
	}

	/**
	A sparse instance with a label
	*/
	class LabeledSparseInstance[C](data:Set[Int], domainSize:Int, val labelValue:C) 
		extends SparseInstance(data,domainSize) 
		with LabeledInstance[C]
	{
		def label:C = labelValue
	}

	/**
	An execption for missing features
	*/
	case class FeatureException(msg:String) extends Exception(msg)
}
