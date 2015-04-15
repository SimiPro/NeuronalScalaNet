package neuronalnet.classification.nets

/**
 * Created by simipro on 15/04/15.
 */
object TestNet {

  var testNet:Net = _

  def setNet(net:Net): Unit = {
    testNet = net
  }

  def getTestNet():Net = {
    if (testNet == null) {
      throw new Exception("If u want to use the testnet e.g for gradient Checking in the connections u have to set a " +
        "new net which has the exactly same configuration as ur prod net")
    }
    testNet
  }

}
