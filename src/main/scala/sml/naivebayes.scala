package sml

import scala.collection.Map
import scala.collection.mutable.HashMap
import scala.math.{log, pow, exp}

import sml.learn._

/**
A library for naive bayes based classifiers
*/
object naivebayes
{
	/**
	Common NaiveBayes functionality
	*/
	abstract class NaiveBayes[C](val classDomain:Set[C], val dimension:Int) extends BatchClassifier[Int,C] with OnlineClassifier[Int,C]
	{
		val index = domain.zipWithIndex.toMap
		val classCounts = Array.fill(domain.size)(0.0)

		/**
		Trains the model by collecting sufficent statistics
		*/
		def batchTrain(examples:Iterable[LabeledInstance[Int,C]]):Boolean =
		{
			//accumulate statistics from each example
			examples.foreach(i => onlineTrain(i))
			return true
		}

		/**
		Classifies the instance via Bayes rule
		*/
		def classify(inst:Instance[Int]):C =
		{
			//for each class compute the proportional probability
			classDomain.maxBy(c => prob(inst,c))
		}	

		def domain:Set[C] = classDomain

		def prob(example:Instance[Int], label:C):Double
	}

	/**
	A Naive Bayes Classifier with a Discrete event model, assumes the data
	is independent discrete variables
	*/
	class DiscNaiveBayes[C](classDomain:Set[C], dimension:Int) extends NaiveBayes[C](classDomain, dimension)
	{
		//class -> feature -> count
		val featCounts = domain.map(c => Range(0,dimension).map(d => new HashMap[Double,Int]()).toArray).toArray

		def onlineTrain(example:LabeledInstance[Int,C])
		{
			val labelIndex = index(example.label)
				
			//increment the class count
			classCounts(labelIndex) += 1

			//increment each feature count
			for((i,feat) <- example.featuresWithKey)
			{
				val featDist = featCounts(labelIndex)(i)
				featDist(feat) = featDist.getOrElse(feat,0) + 1
			}
		}

		/**
		Compute the proportional probability of the example being the given class
		*/
		def prob(example:Instance[Int], label:C):Double =
		{
			val classProb = log(classCounts(index(label)) + 1)

			//for each feature compute the log probability
			val featProb = for((i,feat) <- example.featuresWithKey) yield
			{
				val featDist = featCounts(index(label))(i)
				(log(featDist.getOrElse(feat,0) + 1) - log(featDist.values.sum + featDist.size))
			}

			return classProb + featProb.sum
		}

		override def toString:String =
		{
			//for each class output the weights
			val wStrs = for(label <- classDomain) yield
			{
				s"$label: " + strSummary(featCounts(index(label)))
			}

			return wStrs.mkString("\n")
		}
	}

	/**
	Returns a string summary of the weights
	*/
	def strSummary(counts:Seq[HashMap[Double,Int]]):String =
	{
		val feats = 
			if(counts.size > 10) 
				counts.sortBy(f => f.values.sum).slice(counts.size -10, counts.size) 
			else 
				counts
	
		//generate a string for the feature counts
		val parts = for( (value,index) <- feats.zipWithIndex ) yield
		{
			s"$index: " + value.map(p => p._1+"->"+p._2).mkString(",")
		}

		return parts.mkString(" ")
	}

	/**
	A Naive Bayes Classifier with a Multinomial event model - assumes the
	data is a vector of counts
	*/
	class MultNaiveBayes[C](classDomain:Set[C], dimension:Int) extends NaiveBayes[C](classDomain, dimension)
	{
		//class -> feature -> count
		val featCounts = domain.map(c => Range(0,dimension).map(d => 1.0).toArray).toArray

		def onlineTrain(example:LabeledInstance[Int,C])
		{
			val labelIndex = index(example.label)
				
			//increment the class count
			classCounts(labelIndex) += 1

			//increment each feature count
			for((i,feat) <- example.featuresWithKey)
			{
				featCounts(labelIndex)(i) += 1
			}
		}

		/**
		Compute the proportional probability of the example being the given class
		*/
		def prob(example:Instance[Int], label:C):Double =
		{
			val classProb = log(classCounts(index(label)) + 1)
			val denom = featCounts(index(label)).sum

			//for each feature compute the log probability
			val featProb = for((i,feat) <- example.featuresWithKey) yield
			{
				val num = featCounts(index(label))(i)
				(log(num) - log(denom)) * feat
			}

			return classProb + featProb.sum 
		}

		override def toString:String =
		{
			//for each class output the weights
			val wStrs = for(label <- classDomain) yield
			{
				s"$label: " + compactStr(featCounts(index(label)))
			}

			return wStrs.mkString("\n")
		}
	}

	/**
	A model averaged Naive Bayes Classifier with a Multinomial event model - 
	assumes the	data is a vector of counts
	*/
	class AvgMultNaiveBayes[C](classDomain:Set[C], dimension:Int, val regValue:Double=1.0) extends DiscNaiveBayes[C](classDomain, dimension)
	{
		/**
		Use model averaging to compute the probability of the instance
		*/
		override def prob(example:Instance[Int], label:C):Double =
		{
			val regTerm = pow(regValue, -1 * (dataCount +1))
			val classIndex = index(label)

			//compute the total conditional likelihood	
			val denom = featCounts(classIndex).map(d => d.values.sum).sum

			//total up the class probabilities
			val classProb = for(ci <- domain.map(index)) yield
			{
				val counts = classCounts(ci)
				
				(if(classIndex == ci) counts + 1 else counts) * log(counts)
			}
			
			//total up the feature probabilities
			val featProb = for((i,feat) <- example.featuresWithKey) yield
			{
				val margin = logMargLike(i, feat)
				val cond = logCondLike(i, classIndex, feat) + regTerm

				//add two log likelihoods
				//log( e^a + e^b ) = a + log(1 + e^(b-a))
				margin + log(1.0 + exp(cond - margin))
			}

			return classProb.sum + featProb.sum
		}

		/**
		Returns the log conditional likelihood of the feature	
		*/
		def logCondLike(featIndex:Int, labelIndex:Int, feat:Double):Double =
		{
			val featDist = addPrior(featCounts(labelIndex)(featIndex))
			val denom = featCounts(labelIndex).map(f => addPrior(f).values.sum).sum

			return computeLike(featIndex, feat, denom, featDist)	
		}

		/**
		Returns the log marginal likelihood of the feature
		*/
		def logMargLike(featIndex:Int, feat:Double):Double =
		{
			val denom = index.values.map(i => addPrior(featCounts(i)(featIndex)).values.sum).sum
			val featDist = addPrior(featMargin(featIndex))
			
			return computeLike(featIndex, feat, denom, featDist)
		}

		def computeLike(featIndex:Int, feat:Double, denom:Double, featDist:Map[Double,Int]):Double =
		{
			//compute the marginal likelihood
			val like = for((f,count) <- featDist) yield
			{
				(if(f == feat) count + 1 else count) * f * log(count)
			}
			
			//make sure the term is added in even if it 
			val totalLike = if(featDist.contains(feat)) like.sum else (like.sum + (feat*log(2)))

			return totalLike - ((denom+1) * log(denom))
		}

		def addPrior(dist:Map[Double,Int]):Map[Double,Int] = dist.map(p => (p._1, p._2)).toMap

		/**
		Returns the feature marginal at the given index
		*/
		def featMargin(featIndex:Int):Map[Double,Int] =
		{
			val results = new HashMap[Double,Int]()

			//sum up all the levels of the feature
			for( featDist <- index.values.map(i => featCounts(i)(featIndex)) )
			{
				for( (level, count) <- featDist )
				{
					results(level) = results.getOrElse(level,0) + count
				}
			}

			return results.toMap
		}

		/**
		Returns the total number of examples trained on
		*/
		def dataCount:Double= classCounts.sum
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
		
		val model = new DiscNaiveBayes[Boolean](Set(true,false), 2)

		model.batchTrain(data)

		for(point <- data)
		{
			println("choice " + model.classify(point) + " answer " + point.label)
		}
	}
}
