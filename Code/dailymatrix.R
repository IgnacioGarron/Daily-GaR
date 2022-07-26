# DAILY MATRIX GENERATOR
daily_matrix=function(data_d,N_lag,data_names,q_data){
  fin_daily=as.matrix(data_d[,data_names])
  fin_daily<-na.omit(fin_daily) # quitar fines de semana
  T_g=q_data+4 # one year lags (4 quarters)
  ##### Generar matriz de rezagos VERIFICADO
  fvector=matrix(nrow=T_g, ncol=N_lag)
  for (x in (1:T_g)){
    fmatrx=fin_daily[which(fin_daily[,2]==x),1]
    k=N_lag-length(fmatrx)
    kf=rbind(matrix(nrow=k),as.matrix(fmatrx))
    fvector[x,]=as.vector(t(kf))}
  fv1=fvector[-4:-1,]
  fv2=fvector[-3:-1,]
  fv3=fvector[-2:-1,]
  fv4=fvector[-1:-1,]
  T_lag=q_data# Same as y (nowcasting)
  lagfin_daily=cbind(fv4[1:T_lag,],fv3[1:T_lag,],fv2[1:T_lag,],fv1[1:T_lag,])
  nlag=ncol(lagfin_daily)
  tlag=nrow(lagfin_daily)
  lfin_daily=matrix(nrow=tlag,ncol=nlag)
  for (x in (1:tlag)){
    lag=na.omit(lagfin_daily[x,])
    k=nlag-length(lag)
    kf=rbind(matrix(nrow=k),as.matrix(lag))
    lfin_daily[x,]=t(kf)}
  col.all.na=apply(lfin_daily, 2, function(x){!all(is.na(x))})
  fin=lfin_daily[,col.all.na]
  col.any.na=apply(fin, 2, function(x){!any(is.na(x))})
  fin_1=fin[,col.any.na]
  return_list=list(daily=fin_1)
  return(return_list)
}