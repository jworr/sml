package sml

import java.lang.System.currentTimeMillis
import scala.util.Random
import scala.math.{pow, sqrt}

import sml.learn._

/**
A package of perceptron based classifiers
*/
object perceptron
{
	/**
	A perceptron model for binary classifications with L2 regularization
	*/
	class BinaryPerceptron(val dimension:Int, val learningRate:Double=.1, 
		val regValue:Double = 0.0, 
		val threshold:Double=1e-7, 
		val maxIterations:Int=1000)
	extends BatchClassifier[Boolean]
	{
		//define the weights with a bias term at the end
		val weights = new Array[Double](dimension+1)
		val bias = weights.size -1
		var trainTime = 0.0

		/**
		Train the model
		*/
		def batchTrain(examples:Iterable[LabeledInstance[Boolean]]):Boolean =
		{
			var delta = new Array[Double](dimension+1)
			var i = 0
			var converged = false

			val start = currentTimeMillis
			
			//while the model has not converged, continue training
			while(i < maxIterations && !converged)
			{
				var updates = 0

				//reset delta
				for(j <- 0 to delta.size-1)
				{
					delta(j) = 0.0
				}

				//shuffle the data and train
				for(example <- Random.shuffle(examples))
				{
					//if the model misclassifies the example do an update
					if(classify(example) != example.label)
					{
						val direction = if(example.label) 1.0 else -1.0

						//add to the delta
						for((feat,j) <- iterWithBias(example, bias))
						{
							delta(j) += feat * direction
						}
						updates += 1
					}
				}

				//apply the learned delta
				if(updates > 0)
				{
					//update each weight
					for( (delt, j) <- delta.zipWithIndex)
					{
						weights(j) += (delt/updates - (weights(j) * regValue)) * learningRate
					}

					//check for convergance
					converged = magnitude(delta) / updates < threshold
				}
				else
				{
					converged = true
				}
			
				i += 1
			}
				
			trainTime = currentTimeMillis - start

			return converged
		}

		/**
		Classify an instance	
		*/
		def classify(instance:Instance):Boolean =
		{
			//if the product is positive then the prediction is true
			dotProduct(instance, weights) > 0.0
		}

		def domain:Set[Boolean] = Set(true,false)

		override def toString:String = "Batch Perceptron " + compactStr(weights)
	}

	/**
	Online trained averaged binary perceptron with L2 regularization
	*/
	class OnlineBinaryPerceptron(val dimension:Int, 
		val learningRate:Double=.1,
		val regValue:Double=0.0)
	extends OnlineClassifier[Boolean]
	{
		//define the weights with a bias term at the end
		val weights = new Array[Double](dimension+1)
		val avgWeights = new Array[Double](dimension+1)
		val bias = weights.size -1
		var count = 1

		/**
		Update the model
		*/
		def onlineTrain(example:LabeledInstance[Boolean])
		{
			//if the predicted label doesn't match the output update the weights
			if(trainingClassify(example) != example.label)
			{
				val direction = if(example.label) 1.0 else -1.0
				
				//add to the delta
				for((feat,j) <- iterWithBias(example, bias))
				{
					val delta = feat * direction * learningRate - (learningRate * weights(j) * regValue)
					weights(j) += delta
					avgWeights(j) += count * delta
				}
			}
			count += 1
		}

		/**
		Classify an instance	while training
		*/
		def trainingClassify(instance:Instance):Boolean =
		{
			//if the product is positive then the prediction is true
			dotProduct(instance, weights) > 0.0
		}

		/**
		Computes the final, averaged weights
		*/
		def finalWeights:Array[Double] =
		{
			weights.zip(avgWeights).map( p => p._1 - (p._2 / count) )
		}

		/**
		Predict label
		*/
		def classify(instance:Instance):Boolean =
		{
			//do classification with the averaged weights
			dotProduct(instance, finalWeights) > 0.0
		}

		def domain:Set[Boolean] = Set(true,false)

		override def toString:String = "Online Perceptron " + compactStr(finalWeights)
	}

	/**
	Online trained averaged multiclass perceptron with L2 regularization
	*/
	class OnlinePerceptron[C](val dimension:Int, val classDomain:Set[C],
		val learningRate:Double=.1,
		val regValue:Double=0.0)
	extends OnlineClassifier[C]
	{
		//define the weights with a bias term at the end
		val weights = domain.map(d => (d,new Array[Double](dimension+1))).toMap
		val avgWeights = domain.map(d => (d,new Array[Double](dimension+1))).toMap
		val bias = weights.size -1
		var count = 1

		/**
		Update the model
		*/
		def onlineTrain(example:LabeledInstance[C])
		{
			val predicted = trainingClassify(example)
			val answer = example.label

			//if the predicted label doesn't match the output update the weights
			if(predicted != answer)
			{
				val predWeight = weights(predicted)
				val ansWeight = weights(answer)
				val avgPred = avgWeights(predicted)
				val avgAns = avgWeights(answer)

				//add to the delta
				for((feat,j) <- iterWithBias(example, bias))
				{
					val predDelta = feat * learningRate - (learningRate * predWeight(j) * regValue)
					val ansDelta = feat * learningRate - (learningRate * ansWeight(j) * regValue)
					predWeight(j) -= predDelta
					ansWeight(j) += ansDelta
					avgPred(j) -= count * predDelta
					avgAns(j) += count * ansDelta
				}
			}

			count += 1
		}

		/**
		Classify an instance	while training
		*/
		def trainingClassify(instance:Instance):C =
		{
			//if the product is positive then the prediction is true
			domain.maxBy(d => dotProduct(instance, weights(d)))
		}

		/**
		Computes the final, averaged weights
		*/
		def finalWeights(label:C):Array[Double] =
		{
			weights(label).zip(avgWeights(label)).map(p => p._1 - (p._2 / count))
		}

		/**
		Predict label
		*/
		def classify(instance:Instance):C =
		{
			//do classification with the averaged weights
			domain.maxBy(d => dotProduct(instance, finalWeights(d)))
		}

		def domain:Set[C] = classDomain

		override def toString:String = 
		{
			//generate a str for each domain
			val domainStrs = for(d <- domain) yield
			{
				s"Domain: $d " + compactStr(finalWeights(d))
			}

			return "Multiclass Online Perceptron: " + domainStrs.mkString("\n")
		}
	}

	/**
	Iterate through the features of an instance with a bias term added
	*/
	def iterWithBias(instance:Instance, biasIndex:Int):Iterable[(Double, Int)] =
	{
		instance.featuresWithIndex ++ Seq( (1.0, biasIndex) )
	}

	/**
	Computes the dot product between the instance features and
	the weights
	*/
	def dotProduct(instance:Instance, weights:Array[Double]):Double =
	{
		instance.features.zip(weights).map(p => p._1 * p._2).sum + weights.last
	}

	/**
	Computes the magnitude of the vector
	*/
	def magnitude(vector:Array[Double]):Double = sqrt(vector.map(i => pow(i,2.0)).sum)

	def runTest()
	{
		val data = List( new LabeledVectorInstance(Array(1,1), true),
		new LabeledVectorInstance(Array(3,1), true),
		new LabeledVectorInstance(Array(2,3), true),
		new LabeledVectorInstance(Array(1,2), false),
		new LabeledVectorInstance(Array(0,0), true),
		new LabeledVectorInstance(Array(-1,-1), false),
		new LabeledVectorInstance(Array(-2,-4), false))

		val model = new BinaryPerceptron(2)

		model.batchTrain(Random.shuffle(data))
		
		println("batch")
		println(model.weights.mkString(","))
		for(point <- data)
		{
			println("choice " + model.classify(point) + " answer " + point.label)
		}

		val online = new OnlineBinaryPerceptron(2, .5)

		println("online")
		for(point <- Random.shuffle(data))
		{
			online.onlineTrain(point)	
		}

		println(online)
		for(point <- data)
		{
			println("choice " + online.classify(point) + " answer " + point.label)
		}	
	}

	def multiClassTest()
	{
		val data = List( 
		new LabeledVectorInstance(Array(1,1), "class1"),
		new LabeledVectorInstance(Array(3,1), "class1"),
		new LabeledVectorInstance(Array(2,3), "class1"),
		new LabeledVectorInstance(Array(1,2), "class2"),
		new LabeledVectorInstance(Array(0,0), "class1"),
		new LabeledVectorInstance(Array(-1,-1), "class2"),
		new LabeledVectorInstance(Array(-2,-4), "class2"),
		new LabeledVectorInstance(Array(-1,1), "class3"),
		new LabeledVectorInstance(Array(-3,2), "class3"),
		new LabeledVectorInstance(Array(-2,1), "class3"),
		new LabeledVectorInstance(Array(-1,3), "class3")
		)
	
		val online = new OnlinePerceptron(2, Set("class1", "class2", "class3"))

		println("online")
		for(point <- Random.shuffle(data))
		{
			online.onlineTrain(point)	
		}

		println(online)
		for(point <- data)
		{
			println("choice " + online.classify(point) + " answer " + point.label)
		}
	}

	/**
	Do a couple tests
	*/
	def main(args:Array[String])
	{
		runTest

		multiClassTest
	}
}
