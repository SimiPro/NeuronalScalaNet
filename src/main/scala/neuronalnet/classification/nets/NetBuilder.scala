package neuronalnet.classification.nets

import neuronalnet.classification.layers.{HiddenLayer, HiddenLayerBuilder, InputLayer, OutputLayer}

import scala.collection.mutable


/**
 * Class to Build a net. Default 1 Input Layer, 1 Hidden Layer, 1 Output Layer
 * 2 Input Units, 1 Hidden Unit, 1 Output Unit
 * so just to make easy And and Or nets
 */
class NetBuilder {


  var inputLayerUnits = 2
  var inputLayer: InputLayer = _

  var hiddenLayerUnits = 1
  var hiddenLayers = mutable.MutableList[HiddenLayerBuilder]()

  var outputLayer: OutputLayer = _
  var outputLayerUnits = 1


  def init() = {

  }

  init()


  def setInputLayerUnits(units: Int): NetBuilder = {
    inputLayerUnits = units
    this
  }

  def addHiddenLayers(layer: HiddenLayerBuilder): NetBuilder = {
    hiddenLayers += layer
    this
  }

  def setOutputLayerUnits(units: Int): NetBuilder = {
    outputLayerUnits = units
    this
  }


  def build(): Net = {
    inputLayer = new InputLayer(inputLayerUnits)
    //val hiddenLayer = new HiddenLayer(2)
    outputLayer = new OutputLayer(outputLayerUnits)



    val hiddenLayers_ = mutable.MutableList[HiddenLayer]()
    hiddenLayers.foreach(B => hiddenLayers_ += B.build())

    //hiddenLayers_ += new HiddenLayer(1)
    //hiddenLayers_ += new HiddenLayer(1)

    val net: Net = new Net(inputLayer, hiddenLayers_, outputLayer)

    /*
        // input bias
        inputLayer.neurons(0).postNeurons(1).weight = -0.942323210084099
        inputLayer.neurons(0).postNeurons(2).weight = 1.20704207063339

        inputLayer.neurons(1).postNeurons(1).weight = 2.30184752317254
        inputLayer.neurons(1).postNeurons(2).weight = 14.993607769314

        inputLayer.neurons(2).postNeurons(1).weight = -2.2736524305323
        inputLayer.neurons(2).postNeurons(2).weight =  -14.8740590387072



        // hidden
        hiddenLayer.neurons(0).postNeurons(0).weight = 4.57205432165676
        hiddenLayer.neurons(1).postNeurons(0).weight = 22.4465774962807
        hiddenLayer.neurons(2).postNeurons(0).weight = -16.6739428978086

    */
    net
  }


}
