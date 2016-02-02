package sml.test

import org.scalatest._

import sml.learn._
import sml.perceptron._

/**
 * Runs several tests on the various perceptron models
 */
object perceptron
{

	val binaryData = List( new LabeledVectorInstance(Array(1,1), true),
		new LabeledVectorInstance(Array(3,1), true),
		new LabeledVectorInstance(Array(2,3), true),
		new LabeledVectorInstance(Array(1,2), false),
		new LabeledVectorInstance(Array(0,0), true),
		new LabeledVectorInstance(Array(-1,-1), false),
		new LabeledVectorInstance(Array(-2,-4), false))

	val multiClassData = List( 
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

	class OnlineTest extends FlatSpec with Matchers
	{
		"Online perceptron model" should "learn boundry" in
		{
			val online = new OnlineBinaryPerceptron(2, .5)

			binaryData.foreach(d => online.onlineTrain(d))

			online.weights.forall(w => w >= 0.0) should be (true)

			binaryData.count(d => online.classify(d) == d.label) should be (binaryData.size -1)
		}
	}

	class BatchTest extends FlatSpec with Matchers
	{
		"Batch perceptron model" should "learn boundry" in
		{
			val model = new BinaryPerceptron(2)

			model.batchTrain(binaryData)

			model.weights(0) > 0.0 should be (true)

			binaryData.count(d => model.classify(d) == d.label) should be (binaryData.size -2)
		}
	}

	class MultiClassTest extends FlatSpec with Matchers
	{
		"Multiclass perceptron model" should "learn boundaries" in
		{
			val online = new OnlinePerceptron(2, Set("class1", "class2", "class3"), .5)
			
			multiClassData.foreach(d => online.onlineTrain(d))

			//multiClassData.foreach(d => println(s"$d ${online.classify(d)}"))

			multiClassData.count(d => online.classify(d) == d.label) should be (multiClassData.size - 3)
		}
	}
}
