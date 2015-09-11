package sml

/**
Provides the interface for machine learning
*/
object learn
{
	/**
	Defines a training instance, assumes all features are of the same type
	*/
	trait Instance
	{
		/**
		Returns the value of the feature at the given index
		*/
		def featureAt(index:Int):Double

		/**
		Iterates over all the features
		*/
		def features:Iterable[Double]

		/**
		Iterates over the features with an Index
		*/
		def featuresWithIndex:Iterable[(Double,Int)] = features.zipWithIndex

		/**
		Returns the number of features in the example
		*/
		def size:Int
	}

	/**
	Defines a training instance with a label
	*/
	trait LabeledInstance[C] extends Instance
	{
		def label:C
	}

	/**
	Defines a simple vector instance
	*/
	class VectorInstance(val values:Array[Double]) extends Instance
	{
		def featureAt(index:Int):Double = values(index)

		def features:Iterable[Double] = values

		def size:Int = values.size

		override def toString:String = values.mkString(", ")
	}

	/**
	Defines a simple labeled instance
	*/
	class LabeledVectorInstance[C](val values:Array[Double], val classLabel:C) extends LabeledInstance[C]
	{
		def featureAt(index:Int):Double = values(index)

		def features:Iterable[Double] = values

		def size:Int = values.size

		def label = classLabel

		override def toString:String = classLabel + ":" + values.mkString(", ")
	}

	/**
	Defines the behavior of a classifier
	*/
	abstract class Classifier[C](val classDomain:Set[C])
	{
		def classify(example:Instance):C
	}

	/**
	A classifier that is batch trained
	*/
	abstract class BatchClassifier[C](domain:Set[C]) extends Classifier[C](domain)
	{
		def batchTrain(examples:Iterable[LabeledInstance[C]]):Boolean
	}

	/**
	A classifier that is online trained
	*/
	abstract class OnlineClassifier[C](domain:Set[C]) extends Classifier[C](domain)
	{
		def onlineTrain(example:LabeledInstance[C])
	}
}
