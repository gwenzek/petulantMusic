import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg._
import breeze.optimize.DiffFunction

/**
 * Created by guillaume on 26/05/14.
 */
class NeuralNetwork {
    val featureSize = 200
    val hiddenLayerSize = 100
    val outputSize = 20
    val theta1 = DenseMatrix.rand[Double](hiddenLayerSize, featureSize + 1) / 100.0
    val bias1 = DenseVector.rand[Double](hiddenLayerSize) / 100.0
    val theta2 = DenseMatrix.rand[Double](outputSize, hiddenLayerSize + 1) / 100.0
    val bias2 = DenseVector.rand[Double](outputSize) / 100.0
    val learningRate = 1.0

    def activation_f(xi : Double) = 1.0 / (1.0 + math.exp(-xi))
    def activation(x : DenseVector[Double]) = x.map(activation_f)
    def activationAndDerivative(x : DenseVector[Double]) = {
        def f(xi : Double) = {
            val exi = math.exp(-xi)
            val fxi = 1.0 / (1.0 + exi)
            (fxi, exi * fxi * fxi)
        }
        x.map(f)
    }
    def derivative(x : DenseVector[Double]) = {
        def f(xi : Double) = {
            val exi = math.exp(-xi)
            val fxi = 1.0 / (1.0 + exi)
            exi * fxi * fxi
        }
        x.map(f)
    }

    val regularisationFactor : Double = 0.5
    val regularisationCost = new DiffFunction[DenseMatrix[Double]]{
        def calculate(theta : DenseMatrix[Double]) = {
            val a = regularisationFactor / (theta.rows * theta.cols)
            ( sum(theta(::, *) norm()) * a, theta * 2.0 * a)
        }
    }

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

    def gradientTheta2(x1: DenseVector[Double], t: Int) : Double = {

        val x2 = activation(apply1(x1))
        val y = activation(apply2(x2))
        val cost: Double = sum(log( (y(0 until t) * -1.0) +1.0 ))
            + math.log(y(t))
            + sum(log((y(t+1 to -1) * -1.0) + 1.0 ))

        return cost
    }

    def propagation(x: DenseVector[Double], t:Int) = {
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
        val d2 = (d3*theta2)(0 to -2)
        d2 *= derivative(z2)
        println(s"d2: ${d2.size}")
        
        val delta1 = a1 * d2.t
        val delta2 = a2 * d3.t
        (delta1, delta2)
    }

    // def propagation(xtl : Seq[(DenseVector[Double], Int)]) = {
    //     xtl.par.fold((DenseMatrix.zeros[Double](theta1.size), 0.0)){
    //         (delta, xt) => 
    //             val (d1, d2) = propagation(xt._1, t._2)
    //             (delta._1 + d1, delta._2 + d2)
    //     }
    // }
}

object NeuralNetwork{
    
    println("Hello world")

    def main(args: Array[String]){
        test
        println("main")
        val nn = new NeuralNetwork()
        val x = DenseVector.rand[Double](nn.featureSize)
        
        val (d1, d2) = nn.propagation(x, 1)
        println(s"${d1.rows}, ${d1.cols}")
        println(s"${d2.rows}, ${d2.cols}")

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