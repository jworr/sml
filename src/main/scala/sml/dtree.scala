package sml

import sml.learn._
import sml.util.{Counter, log2}

/**
 * A C4.5 trained decision tree module
 */
object dtree 
{
	/**
	 * A class that represents and trains a decision tree classifier
	 */
	class DecisionTree[K,C](val maxDepth:Int, val domain:Set[C], val featureDomain:Iterable[K], val splitDomain:K=>Iterable[Double]=null) extends BatchClassifier[K,C]
	{
		var root:Node = null

		/**
		 * Trains the decision tree
		 */
		def batchTrain(examples:Iterable[LabeledInstance[K,C]]):Boolean =
		{
			val splits = if(splitDomain == null) allSplits(featureDomain, examples) else splitDomain
			root = trainHelper(examples, 0, splits)
			true	
		}

		/**
		 * Splits the data and recurses
		 */
		def trainHelper(examples:Iterable[LabeledInstance[K,C]], depth:Int, splits:K=>Iterable[Double]):Node =
		{
			//if more nodes are possible, continue recursing
			if(depth <= maxDepth)
			{
				//check if there is a single class in the example
				if( examples.forall(e => e.label == examples.head.label) )
				{
					Leaf(examples.head.label)
				}
				else
				{
					//find the best split
					val(pred, left, right) = findSplit(examples, splits)

					if(left.nonEmpty && right.nonEmpty)
					{
						DecisionNode(pred, trainHelper(left, depth+1, splits), trainHelper(right, depth+1, splits))
					}
					else
					{
						makeLeaf(examples)
					}
				}
			}
			else
			{
				makeLeaf(examples)
			}
		}

		/**
		 * Finds the best node to split the data
		 */
		def findSplit(examples:Iterable[LabeledInstance[K,C]], splitPoints:K=>Iterable[Double])
		:(SplitPredicate[K], Iterable[LabeledInstance[K,C]], Iterable[LabeledInstance[K,C]]) =
		{
			//find all the splits
			val splits = for(pred <- featureDomain.flatMap(k => splitPoints(k).map(f => SplitPredicate(k, f)))) yield
			{
				val (left, right) = pred.split(examples)

				(pred, left, right)
			}

			splits.minBy(t => splitEntropy(t._2, t._3))
		}

		/**
		 * Computes the entropy of the split
		 */
		def splitEntropy(left:Iterable[LabeledInstance[K,C]], right:Iterable[LabeledInstance[K,C]]):Double =
		{
			setEntropy(left) + setEntropy(right)
		}

		/**
		 * Computes the entropy for the test
		 */
		def setEntropy(data:Iterable[LabeledInstance[K,C]]):Double =
		{
			Counter(data.map(_.label)).counts.map(_._2 / data.size.toDouble).map(p => -1.0 * p * log2(p)).sum
		}

		/**
		 * Makes a leaf node
		 */
		def makeLeaf(examples:Iterable[LabeledInstance[K,C]]):Leaf[C] =
		{
			Leaf(Counter(examples.map(_.label)).counts.maxBy(_._2)._1)
		}

		/**
		 * Classifies the example
		 */
		def classify(example:Instance[K]):C =
		{
			classifyHelper(example, root)
		}

		/**
		 * Recurses through the tree until a final decision is reached
		 */
		def classifyHelper(example:Instance[K], current:Node):C = current match
		{
			case d:DecisionNode[K] => classifyHelper(example, d.nextNode(example))
			case l:Leaf[C] => l.choice
		}

		override def toString:String =
		{
			root.treeString("", true).mkString("\n")
		}
	}

	/**
	 * Returns a mapping of all the splits
	 */
	def allSplits[K,C](featureDomain:Iterable[K], data:Iterable[LabeledInstance[K,C]]):Map[K,Iterable[Double]] =
	{
		//for each dimension compute all the possible splits
		val splits = for(key <- featureDomain) yield
		{
			val points = data.map(_.featureAt(key)).toSet.toSeq.sorted.sliding(2).map(_.sum/2.0).toIterable

			(key, points)
		}

		splits.toMap
	}

	/**
	 * The criteria for making a decision about an instance
	 */
	case class SplitPredicate[K](val key:K, val threshold:Double)
	{
		/**
		 * determine if the instance meets the criteria
		 */
		def test(inst:Instance[K]):Boolean = 
		{
			inst.featureAt(key) >= threshold 
		}

		/**
		 * Split some data based on the predicate
		 */
		def split[C](data:Iterable[LabeledInstance[K,C]]):(Iterable[LabeledInstance[K,C]], Iterable[LabeledInstance[K,C]]) =
		{
			data.partition(test(_))
		}

		override def toString:String =
		{
			s"x($key) >= $threshold"
		}
	}

	/*
	 * A node in the decision tree, 
	 */
	abstract class Node
	{
		/**
		 * Recursively builds a string out of the tree
		 */
		def treeString(prefix:String, isTail:Boolean):Iterable[String]

		def tailPrefix(isTail:Boolean):String = if(isTail) "└── " else "├── "

		def extendPrefix(isTail:Boolean):String = if(isTail) "    " else "│   "
	}

	case class DecisionNode[K](val pred:SplitPredicate[K], val left:Node, val right:Node) extends Node
	{
		/**
		 * Makes a decision about the given instance
		 */
		def decide(inst:Instance[K]):Boolean = pred.test(inst)

		/**
		 * Returns the next node in the tree based on the node's decision
		 */
		def nextNode(inst:Instance[K]):Node =
		{
			if(decide(inst))
				left	
			else
				right
		}

		def treeString(prefix:String, isTail:Boolean):Iterable[String] =
		{
			Seq(prefix + tailPrefix(isTail) + this) ++ left.treeString(prefix + extendPrefix(isTail), false) ++ right.treeString(prefix + extendPrefix(isTail), true)
		}

		override def toString:String = pred.toString
	}

	/**
	 * A leaf node 
	 */
	case class Leaf[C](val choice:C) extends Node
	{
		override def toString:String =
		{
			s"$choice"
		}

		def treeString(prefix:String, isTail:Boolean):Iterable[String] =
		{
			Seq(prefix + tailPrefix(isTail) + this)
		}
	}
}