package sml

import scala.collection.mutable.HashMap
import scala.math.log

import sml.learn._

/**
A library for naive bayes based classifiers
*/
object naivebayes
{
	/**
	A Naive Bayes Classifier with a Multinomial event model
	*/
	class MultNaiveBayes[C](val classDomain:Set[C], val dimension:Int) extends BatchClassifier[C] with OnlineClassifier[C]
	{
		val index = domain.zipWithIndex.toMap
		val classCounts = Array.fill(domain.size)(0.0)

		//class -> feature -> count
		val featCounts = domain.map(c => Range(0,dimension).map(d => new HashMap[Double,Int]()).toArray).toArray

		def onlineTrain(example:LabeledInstance[C])
		{
			val labelIndex = index(example.label)
				
			//increment the class count
			classCounts(labelIndex) += 1

			//increment each feature count
			for((feat,i) <- example.featuresWithIndex)
			{
				val featDist = featCounts(labelIndex)(i)
				featDist(feat) = featDist.getOrElse(feat,0) + 1
			}
		}

		/**
		Trains the model by collecting sufficent statistics
		*/
		def batchTrain(examples:Iterable[LabeledInstance[C]]):Boolean =
		{
			//accumulate statistics from each example
			examples.foreach(i => onlineTrain(i))
			return true
		}

		/**
		Classifies the instance via Bayes rule
		*/
		def classify(inst:Instance):C =
		{
			//for each class compute the proportional probability
			classDomain.maxBy(c => prob(inst,c))
		}

		/**
		Compute the proportional probability of the example being the given class
		*/
		def prob(example:Instance, label:C):Double =
		{
			val classProb = log(classCounts(index(label)) + 1)

			//for each feature compute the log probability
			val featProb = for((feat,i) <- example.featuresWithIndex) yield
			{
				val featDist = featCounts(index(label))(i)
				log(featDist.getOrElse(feat,0) + 1) - log(featDist.values.sum + featDist.size) + feat
			}

			return classProb + featProb.sum
		}

		def domain:Set[C] = classDomain
	}

	/**
	Runs some test
	*/
	def main(args:Array[String])
	{
		val data = List( new LabeledVectorInstance(Array(1,1), true),
		new LabeledVectorInstance(Array(3,1), true),
		new LabeledVectorInstance(Array(2,3), true),
		new LabeledVectorInstance(Array(1,2), false),
		new LabeledVectorInstance(Array(0,0), true),
		new LabeledVectorInstance(Array(-1,-1), false),
		new LabeledVectorInstance(Array(-2,-4), false))
		
		val model = new MultNaiveBayes[Boolean](Set(true,false), 2)

		model.batchTrain(data)

		for(point <- data)
		{
			println("choice " + model.classify(point) + " answer " + point.label)
		}
	}
}
