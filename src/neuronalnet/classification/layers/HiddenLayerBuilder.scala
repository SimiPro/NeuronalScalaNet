package neuronalnet.classification.layers

/**
 * Created by Simon on 12.04.2015.
 */
class HiddenLayerBuilder {
  var units = 1
  def setUnits(units: Int): HiddenLayerBuilder = {
    this.units = units
    this
  }


  def build() : HiddenLayer  = {
      new HiddenLayer(units)
  }
}
