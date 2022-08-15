# Seleccionar numero optimo de factores (eigenratio) - Ahn and Horenstein (2013).
numb_factors <-function(data,kmax){
  X=as.matrix(data) 
  T=nrow(X)
  N=ncol(X)
  K=kmax
  if (N<T) {
    xx=(t(X)%*%X)/(T*N) 
  } else {
    xx=(X%*%t(X))/(T*N)
  }
  eig <- eigen(xx)
  a=eig$values;
  d=rev(sort(a));
  #xx=(t(X)%*%as.matrix(X))/(N*T)
  m=min(N,T);
  eing_values <- d
  ER <-  matrix(0,K-1,1)
  for (k in 1:(K-1)) { 
    ER[k,1]= eing_values[k]/eing_values[(k+1)]
  }
  n_fac=max(ER)
  number_factors=which(n_fac==ER)
  return(number_factors)
}