package neuronalnet.classification

import neuronalnet.classification.extern.filereader.MNISTReader
import neuronalnet.classification.layers.HiddenLayerBuilder
import neuronalnet.classification.nets.{NetBuilder, Net}
import neuronalnet.classification.trainingData.TrainSet
import collection.mutable.MutableList

/**
 * Created by simipro on 14/04/15.
 */
object ImageApplication {


  def main(args: Array[String]) {
    var trainData = new MutableList[TrainSet]()

    val reader: MNISTReader = new MNISTReader("E:\\Dev\\MachineLearning\\train-labels\\t10k-labels.idx1-ubyte", "E:\\Dev\\MachineLearning\\train-images\\t10k-images.idx3-ubyte")
    reader.register(I => {
      // Image is an 28*28 picture = 784 features
      val x = Array.ofDim[Double](784)
      val y = Array.ofDim[Double](10) // 0 - 9
      var counter = 0
      for (i <- 0 until I.image.length) {
        for (j <- 0 until I.image(i).length) {
          x(counter) = I.image(i)(j)
          counter = counter + 1
        }
      }
      for (i <- 0 to 9) {
        y(i) = if (i == I.label) 1 else 0
      }
      trainData += new TrainSet(x, y)
    })
    reader.read()

    val net: Net = new NetBuilder()
      .setInputLayerUnits(784)
      .addHiddenLayers(new HiddenLayerBuilder().setUnits(100))
      .setOutputLayerUnits(10)
      .build()

    var cost = 0.1
    val tryC = 0.1
    val sampleSize = 100
    while (cost > 0.09) {
      println("Try: " + tryC)
      val trainData2 = trainData.map(x => (x, util.Random.nextDouble)).sortBy(_._2).take(sampleSize).map(_._1)
      cost = net.train(trainData2)
      println("Actual Cost: " + cost)
    }




    net.input(trainData(17).x)
    println("RESULT: " + net.getResult() + " Expected Result: " + trainData(17).y)

  }

}
