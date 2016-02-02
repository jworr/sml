package sml

/**
Provides the interface for machine learning
*/
object learn
{
	/**
	Defines a training instance, assumes all features are of the same type
	*/
	trait Instance[K]
	{
		/**
		Returns the value of the feature at the given index
		*/
		def featureAt(key:K):Double

		/**
		Iterates over all the features
		*/
		def features:Iterable[Double]

		/**
		Iterates over the features with an Index
		*/
		def featuresWithKey:Iterable[(K,Double)]

		/**
		Returns the number of features in the example
		*/
		def size:Int
	}
	
	/**
	Defines a training instance with a label
	*/
	trait LabeledInstance[K,C] extends Instance[K]
	{
		def label:C
	}

	type DenseInstance = Instance[Int]

	type LabeledDenseInstance[C] = LabeledInstance[Int,C]

	/**
	Defines a simple vector instance
	*/
	class VectorInstance(val values:Array[Double]) extends DenseInstance
	{
		def featureAt(index:Int):Double = values(index)

		def features:Iterable[Double] = values

		def featuresWithKey:Iterable[(Int,Double)] = values.zipWithIndex.map(p => (p._2, p._1))

		def size:Int = values.size

		override def toString:String = values.mkString(", ")
	}

	/**
	Defines a simple labeled instance
	*/
	class LabeledVectorInstance[C](override val values:Array[Double], val classLabel:C) extends VectorInstance(values) with LabeledDenseInstance[C]
	{
		override def label = classLabel

		override def toString:String = classLabel + ":" + values.mkString(", ")
	}

	/**
	Defines the behavior of a classifier
	*/
	trait Classifier[K,C]
	{
		def classify(example:Instance[K]):C

		def domain:Set[C]
	}

	/**
	A classifier that is batch trained
	*/
	trait BatchClassifier[K,C] extends Classifier[K,C]
	{
		def batchTrain(examples:Iterable[LabeledInstance[K,C]]):Boolean
	}

	/**
	A classifier that is online trained
	*/
	trait OnlineClassifier[K,C] extends Classifier[K,C]
	{
		def onlineTrain(example:LabeledInstance[K,C])
	}

	/**
	Returns a compact representation of the weight vector
	*/
	def compactStr(weights:Array[Double]):String =
	{
		val limit = 10

		def toStr(items:Seq[(Double,Int)]):String = items.map(p => p._2 + ":" + p._1).mkString(",")

		//show top and bottom k if the length is over the limit
		if(weights.size > limit)
		{
			val inOrder = weights.sorted
			toStr(inOrder.zipWithIndex.slice(0,limit/2)) + "..." + toStr(inOrder.zipWithIndex.slice(weights.size - (limit/2), weights.size))
		}
		else
		{
			weights.mkString(", ")
		}
	}
}
