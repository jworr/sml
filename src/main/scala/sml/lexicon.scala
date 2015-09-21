package sml

import sml.learn.LabeledInstance

/**
A sparse bag-of-words style collection
*/
object lexicon
{
	/**
	A sparse collection of strings with a binary variable interface
	*/
	class Lexicon[C](source:Seq[Iterable[String]], val domain:Set[String], labels:Seq[C]) extends Iterable[LabeledInstance[C]]
	{
		val wordToIndex = domain.zipWithIndex.toMap
		val data = source.zip(labels).map(p => new SparseInstance(bagToSet(p._1, wordToIndex), domain.size, p._2))

		def iterator:Iterator[LabeledInstance[C]] = data.iterator
	}


	/**
	A sparse instance
	*/
	class SparseInstance[C](val data:Set[Int], val domainSize:Int, val labelValue:C) extends LabeledInstance[C]
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

		def label:C = labelValue
	}

	/**
	Reprents a bag of words as a set of indexes
	*/
	def bagToSet(data:Iterable[String], wordMap:Map[String,Int]):Set[Int] =
	{
		data.map(s => wordMap.getOrElse(s,-1)).filter(i => i != -1).toSet
	}

	/**
	An execption for missing features
	*/
	case class FeatureException(msg:String) extends Exception(msg)
}
