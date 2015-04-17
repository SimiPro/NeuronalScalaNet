package neuronalnet.classification.neurons

import ActivationWeight.ActivationWeight
import _root_.akka.actor.{Props, ActorRef, ActorLogging, Actor}
import neuronalnet.classification.math_.MathHelper
import neuronalnet.classification.neurons.akka._

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
abstract class Neuron(index:Int) extends Actor with ActorLogging {

  def setResult(y: Double): Unit = {
  }

  /**
   * Index in the Layer 0 ... n
   */
  def getIndex():Int = this.index


  var value = 0.0
  var finalValue = 0.0
  var activationValue = 0.0
  var currentActivationValue = 0.0
  var postNeurons: mutable.MutableList[ActorRef] = mutable.MutableList[ActorRef]()
  var preNeurons: mutable.MutableList[ActorRef] = mutable.MutableList[ActorRef]()
  var error = 0.0
  var delta = 0.0


  def setValue(value: Double)

  def resetValues(): Unit = {
    this.value = 0
    this.finalValue = 0
    this.currentActivationValue = 0
    this.error = 0.0
    this.delta = 0.0
  }

  def increaseActivationValue(): Unit = {
    activationValue = activationValue + 1
  }

  def increaseCurrentActivationValue(): Unit = {
    currentActivationValue = currentActivationValue + 1
  }

  override def receive = {
    case Impuls(value) => trigger(value)
    case GradientCheck(trainData, net) => {
      postNeurons.foreach(C => C ! GradientCheck(trainData, net))
    }
    case IncreaseActivationValue() => this.increaseActivationValue()
    case ConnectPostNeuron(postNeuron) => register(postNeuron)
    case ConnectPreNeuron(preNeuron) => preNeurons += preNeuron
    case x => log.info("Unkown Message: {} ", x )
  }

  def trigger(newValue: Double): Unit = {
    // we need this special "reset" because we cant delete the weight after we used it for forward prop
    // because we need it for backprop too
    if (currentActivationValue == activationValue) {
      trigger(newValue, ActivationWeight.reset)
    } else if (currentActivationValue == activationValue - 1) {
      trigger(newValue, ActivationWeight.exeeded)
    } else {
      trigger(newValue, ActivationWeight.tolow)
    }
  }


  def trigger(newValue: Double, activationWeight: ActivationWeight)


  /**
   * it registers the neuron and set his activation weight +1
   * @param postNeuron
   * @return
   */
  def register(postNeuron: ActorRef) = {
    postNeuron ! IncreaseActivationValue()
    var connection = context.actorOf(Props(new Connection(postNeuron, context.self, MathHelper.randomInitializedNumber())))
    postNeurons += connection
    postNeuron ! ConnectPreNeuron(connection)
  }

  override def toString(): String = {
    "Neuron of: " + this.getClass +
      " \nActivationValue: " + this.activationValue +
      " \nCurrent Activationvalue: " + this.currentActivationValue +
      " \nActual Value: " + this.value
  }
}
