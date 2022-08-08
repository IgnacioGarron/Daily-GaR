# LAMBDA SELECTION BELLONI AND CHERNOZHUKOV (2011)

norm2n<- function(z){  sqrt(mean(z^2)) }
lambda.BC<- function(X, R = 1000, tau = 0.1, c = 1, alpha = .10){
  set.seed(12345)
  n <- nrow(X)
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}