package neuronalnet.layers

import neuronalnet.neurons.OutputNeuron

/**
 * Created by Simon on 11.04.2015.
 */
class OutputLayer(units:Int,preLayer: Layer) extends Layer(units) {

  def getResult():Double= {
      neurons.apply(0).value
  }


  override def createUnits(): Unit = {
    for (x <- 1 to units) {
      neurons += new OutputNeuron
    }

    neurons.foreach(N => {
      preLayer.neurons.foreach(preNeuron => {
        preNeuron.register(N)
      })
    })
  }
}
