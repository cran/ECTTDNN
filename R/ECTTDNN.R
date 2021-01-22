
ECTTDNN <- function(data,type="", t,lag_ann,hidden_nodes_ann,r)
{
  coin=data
  length_vec<-nrow(coin)
  #fit a VAR model with appropiate lag
  lag_no=VARselect(coin,lag.max = 12,type = "const")$selection
  k=as.numeric(lag_no[1])
  # conduct cointegration test (Eigen test)
  cointest=ca.jo(coin,K=k,type =type, ecdet = "const", spec = "transitory")

  # make a ca.jo object to convert in VECM and VAR (lags K should be minimum 2)
  summary(cointest)

  #Run VECM
  vecm=cajorls(cointest) # convert in vecm

  # Extract error correction term coefficients (ECT)
  vecm$rlm$coefficients[1,1] # for first variable
  vecm$rlm$coefficients[1,2] # for second variable
  print(vecm$beta)
  aux_var<-0
  ##vector of vecm$rlm$coefficients[1,2] # for second variable
  for(i in 1:length_vec)
  {
    aux_var[i]<-vecm$rlm$coefficients[1,2]
  }

  #cointegration
  #ANN
  #setting


  #ann forecasting for ORIGNAL DATA SET
  yts<-as.matrix(coin[,1])
  len_data=length(yts)
  split_train=ceiling(t*len_data)
  r_train=(split_train)
  traindata=yts[1:r_train,]
  testdata=yts[(r_train+1):len_data,]


  data_X <- as.matrix(aux_var)
  len_datar=length(data_X[,1])
  split_trainr=ceiling(t*len_datar)
  r_trainr=(split_trainr)
  traindatar=data_X[1:r_trainr,]


  fit.y0=nnetar(yts, lag_ann, P=1, hidden_nodes_ann, repeats=r,lambda="auto", model=NULL, subset=NULL)
  fit.y1=nnetar(yts, lag_ann, P=1, hidden_nodes_ann, repeats=r,lambda="auto", model=NULL, subset=traindatar)
  accuracy(fit.y0)
  accuracy(fit.y1)

  predicted_out1<- forecast(fit.y0,length(testdata))
  predicted_out2<- forecast(fit.y1,length(testdata))

  print(predicted_out1)
  print(predicted_out2)

  # summarize accuracy
  MSE_out_TDNN <- mean((testdata - predicted_out1$mean)^2)
  RMSE_out_TDNN <- sqrt(MSE_out_TDNN)
  MSE_out_ECTTDNN <- mean((testdata - predicted_out2$mean)^2)
  RMSE_out_ECTTDN <- sqrt(MSE_out_ECTTDNN)

  #mean absolute deviation (MAD)
  MAD_out_TDNN <- mean(abs(testdata - predicted_out1$mean))
  MAD_out_ECTTDNN <- mean(abs(testdata - predicted_out2$mean))

  #Mean absolute percent error (MAPE)
  MAPE_out_TDNN <- mean(abs((testdata - predicted_out1$mean)/testdata))
  MAPE_out_ECTTDNN <- mean(abs((testdata - predicted_out2$mean)/testdata))
  ECTTDNN_Predict <- predicted_out2
  outsample_accuracy <- cbind(RMSE_out_TDNN,RMSE_out_ECTTDN,MAD_out_TDNN,MAD_out_ECTTDNN,MAPE_out_TDNN,MAPE_out_ECTTDNN)
  output_f=list(outsample_accuracy,ECTTDNN_Predict)
  return(output_f)
}


