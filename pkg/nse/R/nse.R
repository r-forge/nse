#'  Variance of sample mean of functional of reversible Markov chain using methods of Geyer (1992).
#'  @description Calculate Geyer (1992) NSE estimator.
#'  @details  The type "iseq" is a wrapper around \link[mcmc]{initseq} from the MCMC package and gives the positive intial sequence estimator.
#'   The type "bm" is the batch mean estimator.
#'   The type "iseq.bm" is a combinaison of the two.
#'  @examples
#'n = 1000
#'ar = c(0.9,0.6)
#'mean = c(1,5)
#'sd = c(10,2)
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar[1]), sd = sd[1]) + mean[1])
#'Ts2 = as.vector(arima.sim(n = n, list(ar = ar[2]), sd = sd[2]) + mean[2])
#'Ts = cbind(Ts1,Ts2)
#'  
#'nbatch = 30
#'nse::nse.geyer(x = Ts1, nbatch = nbatch, type =  "bm")
#'nse::nse.geyer(x = Ts, nbatch = nbatch , type =  "bm")
#'nse::nse.geyer(x = Ts1 , type = "iseq")
#'nse::nse.geyer(x = Ts1, nbatch = nbatch, type = "iseq.bm")
#'  
#'     @param x A numeric vector or a matrix(only for type "bm").
#'     @param type The type c("iseq","bm","iseq.bm").
#'     @param nbatch An optional parameter for the type bm and iseq.bm.
#'     @import mcmc
#'     @references Geyer, Charles J. "Practical markov chain monte carlo." Statistical Science (1992): 473-483.
#'     @return  The variance estimator in the univariate case or the variance-covariance matrix estimator in the multivariate case.
#'@export
nse.geyer <- function(x, type, nbatch = 30) {
  
  if(is.vector(x)) {
    x = matrix(x,ncol = 1)
  }
  size = dim(x)[1]
  
  if(type == "iseq") {
    
    f.error.multivariate(x) 
    iseq = mcmc::initseq(x = x)$var.pos / size # Intial sqequence Geyer (1992)
    out = iseq
    
  } else if(type == "bm"){
    
    ncol = dim(x)[2]
    x = as.data.frame(x)
    batch = matrix(unlist(lapply(split(x, ceiling(seq_along(x[,1]) / (size / nbatch))), FUN = function(x) colMeans(x))),ncol = ncol,byrow = TRUE)
    out   = var(x = batch) / nbatch
    
    if (is.matrix(out) && dim(out) == c(1,1)) {
    out = as.vector(out)
    }
    
  } else if(type == "iseq.bm"){
    
    f.error.multivariate(x)
    batch  = unlist(lapply(split(x, ceiling(seq_along(x) / (size / nbatch))), FUN = mean))
    iseq.bm = mcmc::initseq(x = batch)$var.pos / nbatch
    out = iseq.bm
    
  } else {
    stop("Invalid type : must be of type c('iseq','bm','iseq.bm')")
  }
  out = unname(out)
  return(out)
}


#' The spectral density at zero.
#' @description Calculate the variance of the mean with the spectrum at zero estimator.
#' @details  This is a wrapper around \link[coda]{spectrum0.ar} form the CODA package.
#' @examples 
#'n = 1000
#'ar = c(0.9)
#'mean = c(1)
#'sd = c(10)
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'  
#'nse::nse.spec0(x = Ts1)
#'  
#'     @param x  A numeric vector.
#'     @return The variance estimator.
#'     @references Plummer, Martyn, et al. "CODA: Convergence diagnosis and output analysis for MCMC." R news 6.1 (2006): 7-11.
#'     @import coda
#'@export
nse.spec0 <- function(x) {
  if(is.vector(x)) {
    x = matrix(x,ncol = 1)
  }
  f.error.multivariate(x)
  size = dim(x)[1]
  out = coda::spectrum0.ar(x)$spec/size
  out = unname(out)
  return(out)
}
#' Newey-West NSE estimators.
#' @description Calculate the variance of the mean with the Newey West (1987, 1994) HAC estimator.
#' @description This is a wrapper around \link[sandwich]{lrvar} from the sandwich package.
#' @examples 
#'n = 1000
#'ar = c(0.9,0.6)
#'mean = c(1,5)
#'sd = c(10,2)
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar[1]), sd = sd[1]) + mean[1])
#'Ts2 = as.vector(arima.sim(n = n, list(ar = ar[2]), sd = sd[2]) + mean[2])
#'Ts = cbind(Ts1,Ts2)
#'  
#'nse::nse.nw(x = Ts1)
#'nse::nse.nw(x = Ts)
#'nse::nse.nw(x = Ts1, prewhite = TRUE)
#'nse::nse.nw(x = Ts, prewhite = TRUE)
#'  
#'     @param x      A numeric vector or matrix.
#'     @param prewhite  A bool indicating if the time-serie will be prewhitened before analysis.
#'     @return The variance estimator in the univariate case or the variance-covariance matrix estimator in the multivariate case.
#'     @references Andrews, Donald WK. "Heteroskedasticity and autocorrelation consistent covariance matrix estimation." Econometrica: Journal of the Econometric Society 59.03 (1991): 817-858.
#'     @references Newey, Whitney K., and Kenneth D. West. "A simple, positive semi-definite, heteroskedasticity and autocorrelationconsistent covariance matrix.", Econometrica: Journal of the Econometric Society 55.03 (1987) : 703-708.
#'     @references Newey, Whitney K., and Kenneth D. West. "Automatic lag selection in covariance matrix estimation." The Review of Economic Studies 61.4 (1994): 631-653.
#'     @references Zeileis, Achim. "Econometric computing with HC and HAC covariance matrix estimators." (2004).
#'     @import sandwich
#'@export
nse.nw <- function(x,prewhite = FALSE) {
  out = sandwich::lrvar(x = x, type = "Newey-West", prewhite = prewhite, adjust = TRUE)
  out = unname(out)
  return(out)
}

#' Andrews NSE estimators.
#' @description Calculate the variance of the mean with the kernel based variance estimator indtroduced by Andrews (1991).
#' @details  This is a wrapper around \link[sandwich]{lrvar} from the sandwich package and use Andrews (1991) automatic bandwidth estimator.
#' @examples 
#'n = 1000
#'ar = c(0.9,0.6)
#'mean = c(1,5)
#'sd = c(10,2)
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar[1]), sd = sd[1]) + mean[1])
#'Ts2 = as.vector(arima.sim(n = n, list(ar = ar[2]), sd = sd[2]) + mean[2])
#'Ts = cbind(Ts1,Ts2)
#'  
#'nse::nse.andrews(x = Ts1, type = "Bartlett")
#'nse::nse.andrews(x = Ts, type = "Bartlett")
#'nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Bartlett")
#'nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Bartlett")
#'  
#'nse::nse.andrews(x = Ts1, type = "Parzen")
#'nse::nse.andrews(x = Ts, type = "Parzen")
#'nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Parzen")
#'nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Parzen")
#'  
#'nse::nse.andrews(x = Ts1, type = "Quadratic Spectral")
#'nse::nse.andrews(x = Ts, type = "Quadratic Spectral")
#'nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Quadratic Spectral")
#'nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Quadratic Spectral")
#'  
#'nse::nse.andrews(x = Ts, type = "Truncated")
#'nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Truncated")
#'nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Truncated")
#'  
#'nse::nse.andrews(x = Ts1, type = "Tukey-Hanning")
#'nse::nse.andrews(x = Ts, type = "Tukey-Hanning")
#'nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Tukey-Hanning")
#'nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Tukey-Hanning")
#'  
#'     @param x       A numeric vector or matrix.
#'     @param prewhite  A bool indicating if the time-serie will be prewhitened before analysis.
#'     @param type  The type of kernel used c("Bartlett","Parzen","Quadratic Spectral","Truncated","Tukey-Hanning").
#'     @return The variance estimator in the univariate case or the variance-covariance matrix estimator in the multivariate case.
#'     @references Zeileis, Achim. "Econometric computing with HC and HAC covariance matrix estimators." (2004).
#'     @references Andrews, Donald WK. "Heteroskedasticity and autocorrelation consistent covariance matrix estimation." Econometrica: Journal of the Econometric Society 59.03 (1991): 817-858.
#'     @references Newey, Whitney K., and Kenneth D. West. "A simple, positive semi-definite, heteroskedasticity and autocorrelationconsistent covariance matrix.", Econometrica: Journal of the Econometric Society 55.03 (1987) : 703-708.
#'     @import sandwich
#'@export
nse.andrews <- function(x, prewhite = FALSE, type = "Bartlett") {
  out = sandwich::lrvar(x = x, type = "Andrews", prewhite = prewhite, adjust = TRUE, kernel = type)
  out = unname(out)
  return(out)
}

#' Hirukawa NSE estimators.  
#' @description Calculate the variance of the mean with the kernel based variance estimator by Andrews (1991) using Hirukawa (2010) automatic bandwidth estimator.
#' @details This is a wrapper around \link[sandwich]{lrvar} from the sandwich package and use Hirukawa (2010) automatic bandwidth estimator.
#' @examples
#'n = 1000
#'ar = c(0.9)
#'mean = c(1)
#'sd = c(10)
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'  
#'nse::nse.hiruk(x = Ts1, type = "Bartlett")
#'nse::nse.hiruk(x = Ts1, prewhite = TRUE, type = "Bartlett")
#'  
#'nse::nse.hiruk(x = Ts1, type = "Parzen")
#'nse::nse.hiruk(x = Ts1, prewhite = TRUE, type = "Parzen")
#'  
#'     @param x      A numeric vector.
#'     @param prewhite A bool indicating if the time-serie will be prewhitened before analysis.
#'     @param type The type of kernel used c("Bartlett","Parzen").
#'     @references Zeileis, Achim. "Econometric computing with HC and HAC covariance matrix estimators." (2004).
#'     @references Andrews, Donald WK. "Heteroskedasticity and autocorrelation consistent covariance matrix estimation." Econometrica: Journal of the Econometric Society 59.03 (1991): 817-858.
#'     @references Hirukawa, Masayuki. "A two-stage plug-in bandwidth selection and its implementation for covariance estimation." Econometric Theory 26.03 (2010): 710-743.
#'     @import sandwich
#'     @return The variance estimator.
#'@export
nse.hiruk <- function(x, prewhite = FALSE, type = "Bartlett") {
  f.error.multivariate(x)
  bandwidth = f.hiruk.bandwidth.solve(x, kernel = type, prewhite = prewhite)
  out = sandwich::lrvar(x = x, type = "Andrews", prewhite = prewhite, adjust = TRUE, kernel = type, bw = bandwidth)
  out = unname(out)
  return(out)
}


#' Bootstrap NSE estimators. 
#' @description Calculate the variance of the mean with a bootstrap variance estimator.
#' @details  Use the automatic blocksize in \link[np]{b.star} from th np package which is based on Politis and White (2004) and Patton and al (2009). 
#' Two bootstrap schemes are available; The stationary bootstrap of Politis and Romano  (1994)
#' and the circular bootstrap of Politis and Romano (1992).
#' @examples  
#'n = 1000
#'ar = c(0.9,0.6)
#'mean = c(1,5)
#'sd = c(10,2)
#'nb = 100
#'  
#'Ts1 = as.vector(arima.sim(n = n, list(ar = ar[1]), sd = sd[1]) + mean[1])
#'Ts2 = as.vector(arima.sim(n = n, list(ar = ar[2]), sd = sd[2]) + mean[2])
#'Ts = cbind(Ts1,Ts2)
#'  
#'nse::nse.boot(x = Ts1, nb =  nb, type = "stationary")
#'nse::nse.boot(x = Ts, nb =  nb, type = "stationary")
#'nse::nse.boot(x = Ts1, nb =  nb, type = "circular")
#'nse::nse.boot(x = Ts, nb =  nb, type = "circular")
#'  
#'     @param x       A numeric vector or a matrix.
#'     @param nb   The number of bootstrap replication.
#'     @param type    The bootstrap schemes c("stationary","circular").
#'     @return The variance estimator in the univariate case or the variance-covariance matrix estimator in the multivariate case.
#'    @references Politis, Dimitris N., and Joseph P. Romano. "A circular block-resampling procedure for stationary data." Exploring the limits of bootstrap (1992): 263-270.
#'    @references Politis, Dimitris N., and Halbert White. "Automatic block-length selection for the dependent bootstrap." Econometric Reviews 23.1 (2004): 53-70.
#'    @references Patton, Andrew, Dimitris N. Politis, and Halbert White. "Correction to "Automatic block-length selection for the dependent bootstrap" by D. Politis and H. White." Econometric Reviews 28.4 (2009): 372-375.
#'    @references Politis, Dimitris N., and Joseph P. Romano. "The stationary bootstrap." Journal of the American Statistical association 89.428 (1994): 1303-1313.
#'    @references Hayfield, Tristen, and Jeffrey S. Racine. "Nonparametric econometrics: The np package." Journal of statistical software 27.5 (2008): 1-32.
#'@import np
#'@export
nse.boot <- function(x, nb, type = "stationary" ){
  blockSize = np::b.star(data = x, round = TRUE)
  if(type == "stationary"){
    blockSize = blockSize[1,1]
  } else if(type == "circular"){
    blockSize = blockSize[1,2]
  }
  out = var(f.bootstrap(x = x, nb = nb, statistic = colMeans, b = blockSize, type = type)$statistic)
  
  if (is.matrix(out) && dim(out) == c(1,1)) {
    out = as.vector(out)
  }
  out = unname(out)
  return(out)
}