package neuronalnet.classification.extern.filereader

import java.io.{FileInputStream, DataInputStream}

import scala.collection.mutable

/**
 * Created by Simon on 14.04.2015.
 */
class MNISTReader(var pathToLabels: String, var pathToImages: String) {
  pathToLabels = "E:/Dev\\MachineLearning\\t10k-labels-idx1-ubyte\\t10k-labels.idx1-ubyte"
  pathToImages = "E:/Dev\\MachineLearning\\t10k-images-idx3-ubyte\\t10k-images.idx3-ubyte"

  var labels = new DataInputStream(new FileInputStream(pathToLabels))
  var images = new DataInputStream(new FileInputStream(pathToImages))
  var magicNumber = labels.readInt()
  if (magicNumber != 2049) {
    throw new Exception("Label files magic number is 2049, not: " + magicNumber)
  }
  magicNumber = images.readInt()
  if (magicNumber != 2051) {
    throw new Exception("Image files magic number is 2051, not: " + magicNumber)
  }
  var numLabels = labels.readInt()
  var numImages = images.readInt()
  var numRows = images.readInt()
  var numCols = images.readInt()
  if (numLabels != numImages) {
    throw new Exception("Label & Image File not contain same amount of entries")
  }


  def read(): Unit = {
    val start = System.currentTimeMillis()
    var numLabelsRead = 0
    var numImagesRead = 0
    while (labels.available() > 0 && numLabelsRead < numLabels) {
      val label = labels.readByte()
      numLabelsRead = numLabelsRead + 1
      val image = Array.ofDim[Int](numCols, numRows)
      for (i <- 0 until numCols) {
        for (j <- 0 until numRows) {
          image(i)(j) = images.readUnsignedByte()
        }
      }
      numImagesRead = numImagesRead + 1
      // At this point we loaded the image. save it for later use or do whatever we want to do with em
      onFile(image, label)
      if ((numLabelsRead % 800) == 0) {
        println(" " + numLabelsRead + " / " + numLabels)
        val end = System.currentTimeMillis()
        val elapsed = end - start
        val minutes = elapsed / (1000 * 60)
        val seconds = (elapsed / 1000) - (minutes * 60)
        println("  " + minutes + " m " + seconds + "s ")
      }
    }
    val end = System.currentTimeMillis()
    val elapsed = end - start
    val minutes = elapsed / (1000 * 60)
    val seconds = (elapsed / 1000) - (minutes * 60)
    println("Read " + numLabelsRead + " samples in " + minutes + " m " + seconds + " s ")
  }

  val onImage = mutable.MutableList[Function[Image, Unit]]()

  def register(f: (Image) => Unit): Unit = {
    onImage += f
  }

  def onFile(image: Array[Array[Int]], label: Byte): Unit = {
    onImage.foreach(F => {
      F(Image(image, label))
    })
  }

  case class Image(image: Array[Array[Int]], label: Byte)

}
