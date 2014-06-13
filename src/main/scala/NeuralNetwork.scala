import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg._
import breeze.stats.distributions.Rand
import breeze.plot._
import java.io.{FileWriter, FileReader}
import java.util.Scanner

/**
 * Created by guillaume on 26/05/14.
 */
class NeuralNetwork(
        val layers: List[DenseMatrix[Double]], 
        val learningRate: Double = 1.0,
        val regularisationFactor: Double = 0.1){

    val theta1 = layers(0)
    val theta2 = layers(1) 
    val outputSize = theta2.rows

    def this(featureSize: Int, hiddenLayerSize: Int, outputSize: Int) = {
        this(List(NeuralNetwork.initMatrix(hiddenLayerSize, featureSize + 1), 
                  NeuralNetwork.initMatrix(outputSize, hiddenLayerSize + 1)))
    }

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

    private def classificationCost(x1: DenseVector[Double], t: Int): Double = {
        val x2 = activation(apply1(x1))
        val y = activation(apply2(x2))
        val cost: Double = sum(log((y(0 until t) * -1.0) + 1.0))
            + math.log(y(t))
            + sum(log((y(t+1 to -1) * -1.0) + 1.0 ))
        //        println(-cost)
        -cost
    }

    def classificationCost(xt: Iterable[(DenseVector[Double], Int)]): Double = {
        xt.map((xt: (DenseVector[Double], Int)) => classificationCost(xt._1, xt._2)).sum
    }

    def accuracy(xt: Iterable[(DenseVector[Double], Int)]): Double = {
        def countCorrect(xt: Iterable[(DenseVector[Double], Int)]): Int = {
            xt.map { (xt) => if (getPredictedClass(xt._1) == xt._2) 1 else 0}.sum
        }
        val lxt = xt.toList
        countCorrect(xt).toDouble / lxt.length
    }

    def confusionMatrix(xt: Iterable[(DenseVector[Double], Int)]) = {
        val confusion = DenseMatrix.zeros[Int](outputSize, outputSize)
        xt.map { (xt) =>
            confusion(getPredictedClass(xt._1), xt._2) += 1
        }
        confusion
    }

    def getPrediction(x: DenseVector[Double]) = {
        iterateOn(x, layers, (x: DenseVector[Double], theta: DenseMatrix[Double]) => activation(applyWithBias(theta, x)))
    }

    def getPredictedClass(x: DenseVector[Double]) = argmax(getPrediction(x))

    def backPropagate(error: DenseVector[Double], z: DenseVector[Double], theta: DenseMatrix[Double]) = {
        (theta(::, 0 to -2).t * error) :* derivative(z)
    }

    def forwardPropagate(a1: DenseVector[Double], theta1: DenseMatrix[Double]) = {
        val z2 = theta1 * a1
        (z2, activation(z2))
    }

    def forwardPropagation(x: DenseVector[Double]) = iterateOnWithHistory(x, layers, forwardPropagate)

    //    def backPropagation()

    private def iterateOn[A, B](a0: A, lb: Iterable[B], f: (A, B) => A): A = {
        if (lb.isEmpty) a0
        else iterateOn(f(a0, lb.head), lb.tail, f)
    }

    private def iterateOnWithHistory[A, B, C](a0: A, lb: Seq[B], f: (A, B) => (A, C)): (List[A], List[C]) = {
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
        //        println("propagation")
        val a1 = x
        //        println(s"a1: ${a1.size}")
        val z2 = apply1(a1)
        val a2 = activation(z2)
        //        println(s"a2: ${a2.size}")
        val z3 = apply2(a2)
        val a3 = activation(z3)
        //        println(s"a3: ${a3.size}")
        val y = DenseVector.zeros[Double](outputSize)
        y(t) = 1.0
        val d3 = a3 - y
        //        println(s"d3: ${d3.size}")
        val d2: DenseVector[Double] = backPropagate(d3, z2, theta2)
        //        println(s"d2: ${d2.size}")

        val delta1 = DenseMatrix.zeros[Double](theta1.rows, theta1.cols)
        //        println("delta1: " + matrixShape(delta1))
        //        println("d2 * a1.t: " + matrixShape(d2 * a1.t))
        delta1(::, 0 to -2) := d2 * a1.t
        delta1(::, -1) := d2
        val delta2 = DenseMatrix.zeros[Double](theta2.rows, theta2.cols)
        delta2(::, 0 to -2) := d3 * a2.t
        delta2(::, -1) := d3
        (delta1, delta2)
    }

    def propagation(xtl: Iterable[(DenseVector[Double], Int)]): Unit = {
        var n_examples = 0
        val delta1 = DenseMatrix.zeros[Double](theta1.rows, theta1.cols)
        val delta2 = DenseMatrix.zeros[Double](theta2.rows, theta2.cols)
        for ((x, t) <- xtl) {
            val d1d2 = propagation(x, t)
            delta1 += d1d2._1
            delta2 += d1d2._2
            n_examples += 1
        }
        delta1 :*= 1.0 / n_examples
        //        delta1(::, 0 to -2) += (theta1(::, 0 to -2) * (regularisationFactor / theta1.size))
        delta2 :*= 1.0 / n_examples
        //        delta2(::, 0 to -2) += (theta2(::, 0 to -2) * (regularisationFactor / theta2.size))
        theta1 :-= delta1 * learningRate
        theta2 :-= delta2 * learningRate
    }

    def matrixShape[T](m: DenseMatrix[T]) = s"(${m.rows}, ${m.cols})"

    def train(xtl: Iterable[(DenseVector[Double], Int)], niter: Int, checkFrequency: Int = 1) {
        for (i <- 1 to niter) {
            propagation(xtl)
            if (i % checkFrequency == 0) {
                println(s"Iteration $i, classification cost: ${classificationCost(xtl)}," +
                        s"regularisation cost: ${regularisationCost}")
                println(s"Accuracy : ${accuracy(xtl)}")
                println("Confusion matrix:")
                println(confusionMatrix(xtl))
                dumpToFile(s"layers/layers_$i.txt")
            }
        }
        dumpToFile(s"layers/layers_$niter.txt")
    }
    
    def dumpToFile(filename: String){
        val output = new FileWriter(filename)
        for (theta <- layers) {
            output.write(s"Layer ${theta.rows} ${theta.cols}\n")
            for(i <- 0 until theta.rows){
                for(j <- 0 until theta.cols){
                    output.write(s"${theta(i, j)} ")
                }
                output.write("\n")
            }
        }
        output.close()
        println("Neural network saved to : " + filename)
    }
}

object NeuralNetwork{
    println("Hello world")

    def visualizeHiddenLayer(theta: DenseMatrix[Double], width: Int, height: Int, row: Int) {
        val rowData = theta(row to row, 0 to -2)
        val mat = rowData.reshape(width, height)
        val f = Figure()
        f.subplot(0) += image(mat)
    }

    def visualizeInput(f: Figure, x: DenseVector[Double], width: Int, height: Int) {
        val mat = new DenseMatrix(width, height, x.data)
        f.subplot(0) += image(mat)
    }

    def initMatrix(rows: Int, cols: Int) : DenseMatrix[Double] = {
        val r = math.sqrt(6) / math.sqrt(rows * cols)
        val uniform = new Rand[Double] { def draw = (Rand.uniform.draw * 2 - 1) * r }
        DenseMatrix.rand[Double](rows, cols, uniform)
    }

    def fromFile(filename: String) {
        val input = new Scanner(new FileReader(filename))

        val yieldLayers = new Iterator[DenseMatrix[Double]]{
            def hasNext = input.hasNext
            def next() = {
                val line = input.nextLine
                println(line)
                val dim = line.split(" ")
                val rows = dim(1).toInt
                val cols = dim(2).toInt
                val theta = new Array[Double](rows * cols)
                for(i <- 0 until rows; j <- 0 until cols){
                    theta(i*cols + j) = input.nextDouble
                }
                input.nextLine
                new DenseMatrix[Double](rows, cols, theta)
            }
        }
        new NeuralNetwork(yieldLayers.toList)
    }
    

    def main(args: Array[String]){
        println("main")
        // test_train
        // val nn = fromFile("layers/layers_200.txt")
    }

    def test_train {
        val width = 20
        val height = 70
        val hidden = 15
        val firstLine = 20
        val lastLine = 50
        val xtl = DataLoader.load("data",
            width, height, firstLine, lastLine,
            _.isNote, _.duration).filter(_._1.length == width*height).toList
        // xtl.foreach(xt => print(s"${xt._2} "))
        // println()
        val nn = new NeuralNetwork(width * height, hidden, Note.durationClass)
        nn.dumpToFile("layers/layers_0.txt")
        nn.train(xtl, 200, 10)
        // for (i <- 0 until nn.hiddenLayerSize) {
        //     visualizeHiddenLayer(nn.theta1, width, height, i)
        // }
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