package neuronalnet.neurons

import neuronalnet.ActivationWeight.ActivationWeight
import neuronalnet.ActivationWeight.ActivationWeight
import neuronalnet.{MathHelper, ActivationWeight, Application}

/**
 * Created by Simon on 11.04.2015.
 */
class HiddenNeuron extends Neuron {


  override def trigger(newValue:Double,activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

      // as soon as all connections pulled the trigger we can calculate forward
      // the first one resets the old value
      activationWeight match {
        case ActivationWeight.exeeded =>  {
          // sigmoid func
          increaseCurrentActivationValue()
          setValue(newValue)
          val sigValue = MathHelper.sigmoid(value)
          finalValue = sigValue
          // trigger next connections
          postNeurons.foreach(C => {
            C.postNeuron.trigger(sigValue*C.weight)
          })
        }
        case ActivationWeight.tolow => {
          setValue(newValue)
          increaseCurrentActivationValue()
        }
        case ActivationWeight.reset => {
          resetValues()
          increaseCurrentActivationValue()
          setValue(newValue)
        }
      }
  }

  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }



  override def setResult(error: Double): Unit = {
    // set theta based on error from post neuron
    setError(postNeurons.apply(0).postNeuron.getError()*finalValue)

    // error = weight*delta_3

   // postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()
    val delta_2 = error*MathHelper.sigmoidGradient(value)

    preNeurons.foreach(C => {
      //TODO:  why no need of *C.weight ??? ? ??
        C.preNeuron.setResult(delta_2)
    })
  }
}
