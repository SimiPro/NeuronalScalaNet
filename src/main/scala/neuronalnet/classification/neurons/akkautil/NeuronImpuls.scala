package neuronalnet.classification.neurons.akka

import akka.actor.{ActorSelection, ActorRef}
import neuronalnet.classification.nets.Net
import neuronalnet.classification.neurons.Connection
import neuronalnet.classification.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 15.04.2015.
 */
sealed trait NeuronImpuls {

}

case class Impuls(value:Double) extends NeuronImpuls
case class ImpulsArray(x: Array[Double]) extends NeuronImpuls

case class GradientCheck(trainData: mutable.MutableList[TrainSet], net: Net)

case class UpdateWeights(dataSize: Int, alpha:Double)

case class Result(index:Int, result:Double)
case class ResultArray(y:Array[Double])
case class IncreaseActivationValue()
case class PreNeuron(connection: ActorRef)
case class ConnectPostLayer(postLayer: ActorRef)
case class ConnectPreLayer(preLayer:ActorRef)
case class ConnectPostNeuron(postNeuron: ActorRef)
case class ConnectPreNeuron(preNeuron: ActorRef)
case class DeltaImpuls(delta:Double)
case class GetFinalValue(a:Double)
case class Train(set:mutable.MutableList[TrainSet])
case class RegisterResultListener(listener:ActorRef)
case class OutputResult(result:Array[Double])
case class ResultImpuls(value:Double, index:Int)