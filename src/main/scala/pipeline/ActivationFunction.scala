package pipeline

import breeze.linalg.DenseVector


class ActivationFunction(var f: Double => (Double, Double)){
    
    def apply(xi: Double) : Double = f(xi)._1
    def apply(x: DenseVector[Double]): DenseVector[Double] = x.map(f(_)._1)
    def d(x: DenseVector[Double]) = x.map(f(_)._2)
}

object ActivationFunction {
    
    val sigmoid = new ActivationFunction( x => {
            val fx = 1.0 / (1.0 + math.exp(-x))
            (fx, fx * (1.0 - fx))
        })

    //TODO find the problem...
    val tanh = new ActivationFunction( x => {
            val fx = math.tanh(x - 0.5)
            (fx, 1 - fx * fx)
        })

}