import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.io.Source
import scala.math.Numeric
import scala.math._
import scala.annotation.tailrec
import scala.util.Random
import breeze.stats._
import breeze.linalg._

object LmMcmc { 
    
  def generate(nobs: Int, nfeatures: Int, sigma2: Double = 1.0) = {
    val x = DenseMatrix.fill(nobs,nfeatures)(scala.util.Random.nextGaussian())
    val beta = DenseVector.fill(nfeatures)(scala.util.Random.nextGaussian())
    val eps = DenseVector.fill(nobs)(scala.util.Random.nextGaussian())
    eps *= sqrt(sigma2)
    (x*beta+eps,x)
  }
  
  def mcmc(y: DenseVector[Double], x: DenseMatrix[Double], nmc: Int) = {
    val n = y.length
    val p = x.cols
    // The parameters of the model to be sampled
    var beta = DenseVector.fill(p)(scala.util.Random.nextGaussian())
    var sigma2: Double = 1.0
    
    // Some things that are usefully pre-computed
    val xtx = x.t * x // Problem: breeze doesn't think this matrix is symmetric
    val xty = x.t * y
    
    for(i <- 1 to nmc) {
      val precision = xtx
      precision *= 1.0/sigma2
      val mu = xtx * xty // Right now this is effort wasted inside the loop, but want to keep this structure
      val sigma = inv(precision)
      beta = rmvnorm(mu, sigma) // this is returning an error because sigma is not recognized as symmetric
      println(beta)
    }
    beta
  }
  
  def rmvnorm(mu: DenseVector[Double], sigma: DenseMatrix[Double]) = {
    val eps = DenseVector.fill(mu.length)(scala.util.Random.nextGaussian())
    val lowertri = cholesky(sigma)
    mu + lowertri * eps
  }
  
  def main(args: Array[String]) {
    args.toSeq match {
    
      case Seq("generate", nobs, nfeatures, xfile, yfile) =>
        val (y, x) = generate(nobs.toInt, nfeatures.toInt)

      case Seq("test", nobs, nfeatures) =>
        val (y, x) = generate(nobs.toInt, nfeatures.toInt)
        val betafinal = mcmc(y, x, 1000)
    }
  }
}
