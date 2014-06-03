import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg._
import breeze.optimize.DiffFunction

/**
 * Created by guillaume on 26/05/14.
 */
class NeuralNetwork(val featureSize: Int, val hiddenLayerSize: Int, val outputSize: Int) {
    val theta1 = DenseMatrix.rand[Double](hiddenLayerSize, featureSize + 1) / 100.0
    val bias1 = DenseVector.rand[Double](hiddenLayerSize) / 100.0
    val theta2 = DenseMatrix.rand[Double](outputSize, hiddenLayerSize + 1) / 100.0
    val bias2 = DenseVector.rand[Double](outputSize) / 100.0
    val learningRate = 1.0
    val regularisationFactor = 0.5
    val layers = theta1 :: theta2 :: Nil

    def activation(xi: Double) = 1.0 / (1.0 + math.exp(-xi))

    def activation(x: DenseVector[Double]): DenseVector[Double] = x.map(activation)

    def activationAndDerivative(xi: Double) = {
        val axi: Double = activation(xi)
        (axi, axi * (1.0 - axi))
    }

    def activationAndDerivative(x: DenseVector[Double]): DenseVector[(Double, Double)] = x.map(activationAndDerivative)

    def derivative(x : DenseVector[Double]) = {
        x.map({ xi: Double => activationAndDerivative(xi)._2})
    }

    //    val regularisationCost = new DiffFunction[DenseMatrix[Double]]{
    //        def calculate(theta : DenseMatrix[Double]) = {
    //            val a = regularisationFactor / (theta.rows * theta.cols)
    //            ( sum(theta(::, *) norm()) * a, theta * 2.0 * a)
    //        }
    //    }

    def regularisationCost(theta: DenseMatrix[Double]): Double = {
        val a = regularisationFactor / (theta.rows * theta.cols)
        sum(theta(::, *) norm()) * a
    }

    def regularisationCost: Double = regularisationCost(theta1) + regularisationCost(theta2)

    object log extends UFunc with MappingUFunc {
        implicit object implDouble extends Impl[Double, Double] {
            def apply(a: Double) = scala.math.log(a)
        }
    }

    def applyWithBias(theta: DenseMatrix[Double], x: DenseVector[Double]): DenseVector[Double] = {
        (theta(::, 0 to -2) * x) + theta(::, -1)
    }

    def apply1(x: DenseVector[Double])= applyWithBias(theta1, x)
    def apply2(x: DenseVector[Double]) = applyWithBias(theta2, x)

    private def cost(x1: DenseVector[Double], t: Int): Double = {
        val x2 = activation(apply1(x1))
        val y = activation(apply2(x2))
        sum(log((y(0 until t) * -1.0) + 1.0))
            + math.log(y(t))
            + sum(log((y(t+1 to -1) * -1.0) + 1.0 ))
    }

    //    def cost(xt: Iterator[(DenseVector[Double], Int)]) : Double = {
    //        xt.fold(regularisationCost: Double)()
    //    }

    def backPropagate(error: DenseVector[Double], z: DenseVector[Double], theta: DenseMatrix[Double]) = {
        (theta(::, 0 to -2).t * error) :* derivative(z)
    }

    def forwardPropagate(a1: DenseVector[Double], theta1: DenseMatrix[Double]) = {
        val z2 = theta1 * a1
        (z2, activation(z2))
    }

    def forwardPropagation(x: DenseVector[Double]) = specialMap(x, layers, forwardPropagate)

    //    def backPropagation()

    private def specialMap[A, B, C](a0: A, lb: Seq[B], f: (A, B) => (A, C)): (List[A], List[C]) = {
        def aux(a0: A, lb: Seq[B], la: List[A], lc: List[C]): (List[A], List[C]) = {
            if (lb.isEmpty) (la, lc)
            else {
                val ac: (A, C) = f(a0, lb.head)
                aux(ac._1, lb.tail, ac._1 :: la, ac._2 :: lc)
            }
        }
        aux(a0, lb, Nil, Nil)
    }

    private def propagation(x: DenseVector[Double], t: Int) = {
        println("propagation")
        val a1 = x
        println(s"a1: ${a1.size}")
        val z2 = apply1(a1)
        val a2 = activation(z2)
        println(s"a2: ${a2.size}")
        val z3 = apply2(a2)
        val a3 = activation(z3)
        println(s"a3: ${a3.size}")
        val y = DenseVector.zeros[Double](outputSize)
        y(t) = 1.0
        val d3 = a3 - y
        println(s"d3: ${d3.size}")
        val d2: DenseVector[Double] = backPropagate(d3, z2, theta2)
        println(s"d2: ${d2.size}")

        val delta1 = DenseMatrix.zeros[Double](hiddenLayerSize, featureSize + 1)
        println("delta1: " + matrixShape(delta1))
        println("d2 * a1.t: " + matrixShape(d2 * a1.t))
        delta1(::, 0 to -2) := d2 * a1.t
        delta1(::, -1) := d2
        val delta2 = DenseMatrix.zeros[Double](outputSize, hiddenLayerSize + 1)
        delta2(::, 0 to -2) := d3 * a2.t
        delta2(::, -1) := d3
        (delta1, delta2)
    }

    def propagation(xtl: Iterator[(DenseVector[Double], Int)]): Unit = {
        val n_examples = xtl.length
        val delta1 = DenseMatrix.zeros[Double](hiddenLayerSize, featureSize + 1)
        val delta2 = DenseMatrix.zeros[Double](outputSize, hiddenLayerSize + 1)
        for ((x, t) <- xtl) {
            val d1d2 = propagation(x, t)
            delta1 += d1d2._1
            delta2 += d1d2._2
        }
        delta1(::, 0 to -2) += (theta1(::, 0 to -2) * (1.0 / regularisationFactor))
        delta1 :*= 1.0 / n_examples
        delta2(::, 0 to -2) += (theta2(::, 0 to -2) * (1.0 / regularisationFactor))
        delta2 :*= 1.0 / n_examples
        theta1 += delta1 * learningRate
        theta2 += delta2 * learningRate
    }

    def matrixShape[T](m: DenseMatrix[T]) = s"(${m.rows}, ${m.cols})"

    def train(xtl: Iterator[(DenseVector[Double], Int)]) {

    }
}

object NeuralNetwork{
    println("Hello world")


    def main(args: Array[String]){
        //        test
        println("main")
        val nn = new NeuralNetwork(200, 100, Note.durationClass)
        val xt = DataLoader.load("Jerusalem/img", 1, 65, 20, 50, _.isNote, _.duration)

        nn.propagation(xt)
        //        println(s"${d1.rows}, ${d1.cols}")
        //        println(s"${d2.rows}, ${d2.cols}")
    }

    def test {
        println("test")
        val theta = DenseMatrix.rand[Double](5, 4)
        val x = DenseVector.rand[Double](3)
        // val y = DenseVector.rand[Double](4)
        println(theta)
        println(x)
        println(theta(::, 0 to -2))
        println(theta(::, 0 to -2) * x)
    }
}