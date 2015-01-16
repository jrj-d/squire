package regression


import breeze.linalg.{DenseVector, DenseMatrix}


case class UntrainedException(message: String) extends Exception(message)
case class DataException(message: String) extends Exception(message)


trait Regressor {
	def train(X: DenseMatrix[Double], y: DenseVector[Double]): Regressor
	def apply(X: DenseMatrix[Double]): DenseVector[Double]
	def apply(X: Array[Double]): Double
}


class Ridge(val coefficients: DenseVector[Double], val regularizer: Double) extends Regressor {

	def this(regularizer: Double) = this(null, regularizer)

	def this(coefficients: DenseVector[Double]) = this(coefficients, 0)

	def this(coefficients: Array[Double]) = this(new DenseVector[Double](coefficients), 0)

	def apply(X: DenseMatrix[Double]): DenseVector[Double] = if(coefficients == null)
		throw UntrainedException("ridge regressor has not been trained yet")
	else if(X.cols != coefficients.length)
		throw DataException("input data dimensions do not match dimensions supported by ridge regressor")
	else X * coefficients

	def apply(X: Array[Double]): Double = apply(DenseMatrix(X))(0)

	def train(X: DenseMatrix[Double], y: DenseVector[Double]): Regressor = if(X.rows != y.length)
		throw DataException("feature matrix and target vector dimensions do not match")
	else new Ridge((X.t * X + DenseMatrix.eye[Double](X.cols) * regularizer) \ (X.t * y), regularizer)

}
