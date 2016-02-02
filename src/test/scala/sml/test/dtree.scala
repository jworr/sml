package sml.test

import org.scalatest._

import sml.test.perceptron.multiClassData
import sml.dtree._

/**
 * tests the decision tree class
 */
object dtree 
{
	class OnlineTest extends FlatSpec with Matchers
	{
		"Decision Tree Test" should "correctly classify all the points" in
		{
			val domain = multiClassData.map(_.label).toSet
			val model = new DecisionTree(5, domain, Seq(0,1))

			model.batchTrain(multiClassData)

			println(model)

			multiClassData.count(d => model.classify(d) == d.label) should be (multiClassData.size -1)
		}
	}
}
