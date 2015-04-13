package neuronalnet.classification.nets

import neuronalnet.classification.layers.{OutputLayer, HiddenLayer, InputLayer, HiddenLayerBuilder}
import scala.collection.mutable



/**
 * Class to Build a net. Default 1 Input Layer, 1 Hidden Layer, 1 Output Layer
 * 2 Input Units, 1 Hidden Unit, 1 Output Unit
 * so just to make easy And and Or nets
 */
class NetBuilder {
  var inputLayerUnits = 2
  var inputLayer:InputLayer = _

  var hiddenLayerUnits = 1
  var hiddenLayers = mutable.MutableList[HiddenLayerBuilder]()

  var outputLayer:OutputLayer = _
  var outputLayerUnits = 1



  def init() = {

  }

  init()



  def setInputLayerUnits(units:Int):NetBuilder = {
    inputLayerUnits = units
    this
  }

  def addHiddenLayers(layer:HiddenLayerBuilder):NetBuilder = {
    hiddenLayers += layer
    this
  }

  def setOutputLayerUnits(units:Int):NetBuilder = {
    outputLayerUnits = units
    this
  }



  def build():Net = {
    inputLayer = new InputLayer(2)
    val hiddenLayer = new HiddenLayer(1)
    outputLayer = new OutputLayer(1)
    val hiddenLayers_ = mutable.MutableList[HiddenLayer](hiddenLayer)
    hiddenLayers_ += new HiddenLayer(1)
    //hiddenLayers_ += new HiddenLayer(1)

    val net:Net = new Net(inputLayer, hiddenLayers_, outputLayer)

    net
  }








}
