package sml

import sml.learn._

/**
A sparse bag-of-words style collection
*/
object lexicon
{
	/**
	A sparse collection of strings with a binary variable interface
	*/
	class Lexicon[C](source:Seq[Iterable[String]], val domain:Dictionary, labels:Seq[C]) 
		extends Iterable[LabeledInstance[Int,C]]
	{
		val data = source.zip(labels).map(p => domain.makeLabeledInstance(p._1, p._2))

		def iterator:Iterator[LabeledInstance[Int,C]] = data.iterator
	}

	/**
	A dense collection of strings with a binary variable interface
	*/
	class DenseLexicon[C](source:Seq[Iterable[String]], val domain:Dictionary, labels:Seq[C]) 
		extends Iterable[LabeledInstance[Int,C]]
	{
		val data = source.zip(labels).map(p => domain.makeLabeledDense(p._1, p._2))

		def iterator:Iterator[LabeledInstance[Int,C]] = data.iterator
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
		Makes an array out of the collection
		*/
		def makeArray(data:Iterable[String]):Array[Double] =
		{
			val indexes = makeSparse(data)

			Range(0,domain.size).map(i => if(indexes.contains(i)) 1.0 else 0.0).toArray
		}

		/**
		Creates an instance based on the dictionary's domain
		*/
		def makeInstance(data:Iterable[String]):Instance[Int] = new SparseInstance(makeSparse(data), size)

		/**
		Creates a labeled instance based on the dictionary's domain
		*/
		def makeLabeledInstance[C](data:Iterable[String], label:C):LabeledInstance[Int,C] =
		{
			new LabeledSparseInstance(makeSparse(data), size, label)
		}

		/**
		Creates a dense instance
		*/
		def makeLabeledDense[C](data:Iterable[String], label:C):LabeledInstance[Int,C] =
		{
			new LabeledVectorInstance(makeArray(data), label)
		}

		/**
		Creates a dense labelled instance
		*/
		def makeDense[C](data:Iterable[String]):Instance[Int] =
		{
			new VectorInstance(makeArray(data))
		}

		def size:Int = domain.size
	}

	/**
	A sparse instance
	*/
	class SparseInstance(val data:Set[Int], val domainSize:Int) extends Instance[Int]
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

		/**
		 * Returns all the features along with the keys
		 */
		def featuresWithKey:Iterable[(Int,Double)] = features.zipWithIndex.map(p => (p._2, p._1))
		/*
		new Iterable[Double]
		{
			def iterator = new Iterator[Double]
			{
				var index = 0
				
				def hasNext:Boolean = index < domainSize
				def next:Double =
				{
					val result = featureAt(index)
					index += 1
					return result
				}
			}
		}*/

		def size:Int = domainSize
	}

	/**
	A sparse instance with a label
	*/
	class LabeledSparseInstance[C](data:Set[Int], domainSize:Int, val labelValue:C) 
		extends SparseInstance(data,domainSize) 
		with LabeledInstance[Int, C]
	{
		def label:C = labelValue
	}

	/**
	An execption for missing features
	*/
	case class FeatureException(msg:String) extends Exception(msg)
}
