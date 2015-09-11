package sml

import scala.util.Random
import scala.math.{pow, sqrt}

import sml.learn._

/**
A package of perceptron based classifiers
*/
object perceptron
{
	/**
	A perceptron model for binary classifications
	*/
	class BinaryPerceptron(val dimension:Int, val learningRate:Double=.1, val threshold:Double=1e7, val maxIterations:Int=1000)
	extends BatchClassifier[Boolean](Set(true,false))
	{
		//define the weights with a bias term at the end
		val weights = new Array[Double](dimension+1)
		val bias = weights.size -1

		/**
		Train the model
		*/
		def batchTrain(examples:Iterable[LabeledInstance[Boolean]]):Boolean =
		{
			var delta = new Array[Double](dimension+1)
			var i = 0
			var converged = false
			
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
						for( (feat,j) <- example.featuresWithIndex)
						{
							delta(j) -= feat * direction
						}

						//update the bias
						delta(bias) -= direction
						updates += 1
					}
				}

				//apply the learned delta
				if(updates > 0)
				{
					//update each weight
					for( (delt, j) <- delta.zipWithIndex)
					{
						weights(j) -= learningRate * delta(j)/updates
					}
				}

				//check for convergance
				converged = magnitude(delta) < threshold
				i += 1
			}

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
	}

	/**
	Online trained binary perceptron
	*/
	class OnlineBinaryPerceptron(val dimension:Int, val learningRate:Double=.1)
	extends OnlineClassifier[Boolean](Set(true,false))
	{
		//define the weights with a bias term at the end
		val weights = new Array[Double](dimension+1)
		val bias = weights.size -1

		/**
		Update the model
		*/
		def onlineTrain(example:LabeledInstance[Boolean])
		{
			//if the predicted label doesn't match the output update the weights
			if(classify(example) != example.label)
			{
				val direction = if(example.label) 1.0 else -1.0
				
				//add to the delta
				for( (feat,j) <- example.featuresWithIndex)
				{
					weights(j) += feat * direction * learningRate
				}

				//update the bias
				weights(bias) += direction * learningRate
			}
		}

		/**
		Classify an instance	
		*/
		def classify(instance:Instance):Boolean =
		{
			//if the product is positive then the prediction is true
			dotProduct(instance, weights) > 0.0
		}
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

	/**
	Do a couple tests
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

		val model = new BinaryPerceptron(2)

		model.batchTrain(data)
		
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

		println(online.weights.mkString(","))
		for(point <- data)
		{
			println("choice " + online.classify(point) + " answer " + point.label)
		}
	}
}
