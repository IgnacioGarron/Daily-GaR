
# Almon LAG

# Almon lag polynomial with tail and derivative restrictions (fC=0 and dfC=0) Mogliani and Simoni (2021)
Almon_lag = function(polydegree, C){
  base_seq = (C - 1):0
  Q = matrix(0, polydegree - 1, C)
  for (ii in 0:(polydegree - 1)){
    Q[ii, ] = base_seq ^ (ii + 1) - (ii + 1) * (C - 1) ^ ii * base_seq + ii * (C - 1) ^ (ii + 1)
  }
  return(Q)
}
