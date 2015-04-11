package neuronalnet

/**
 * Created by Simon on 11.04.2015.
 */
object ApplicationTest {

  def main(args: Array[String]) {
    println(MathHelper.sigmoidGradient(-3))
    println(MathHelper.sigmoidGradient(3))
    println(MathHelper.sigmoidGradient(200))
    println(MathHelper.sigmoidGradient(-200))
    println(MathHelper.sigmoidGradient(0))
    Assert(MathHelper.sigmoidGradient(1) == 0.19661193324148185)
    Assert(MathHelper.sigmoidGradient(-1) == 0.19661193324148185)
  }

  def Assert(toHave:Boolean):Unit = {
    if (!toHave) {
      throw new Exception("Fail")
    }

  }


}
