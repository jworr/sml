package sml.test

import org.scalatest._

import sml.learn._
import sml.test.perceptron.multiClassData
import sml.dtree._

/**
 * tests the decision tree class
 */
object dtree 
{
	val quadData = List( 
		new LabeledVectorInstance(Array(1,1), "class1"),
		new LabeledVectorInstance(Array(3,1), "class1"),
		new LabeledVectorInstance(Array(-2,1), "class2"),
		new LabeledVectorInstance(Array(-5,3), "class2"),
		new LabeledVectorInstance(Array(-2,-2), "class3"),
		new LabeledVectorInstance(Array(-3,-1), "class3"),
		new LabeledVectorInstance(Array(2,-1), "class4"),
		new LabeledVectorInstance(Array(4,-2), "class4")
		)

	class OnlineTest extends FlatSpec with Matchers
	{
		"Decision Tree Test" should "correctly classify most the points" in
		{
			val domain = multiClassData.map(_.label).toSet
			val model = new DecisionTree(5, domain, Seq(0,1))

			model.batchTrain(multiClassData)

			println(model)

			multiClassData.count(d => model.classify(d) == d.label) should be (multiClassData.size)
		}

		it should "correctly classify all the points in" in
		{
			val domain = multiClassData.map(_.label).toSet

			val model = new DecisionTree(5, domain, Seq(0,1))

			model.batchTrain(quadData)

			println(model)

			quadData.count(d => model.classify(d) == d.label) should be (quadData.size)
		}
	}
}
