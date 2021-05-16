
# Returns list of available algorithms for selection..............

algo_list<-function(){
  return(c("Mean","Drift","Naive","Seasonal Naive","ETS","Auto ARIMA","FFNN","Ensemble"))
}

#.................................................................

# Returns list of available algorithms for selection..............

algo_list2<-function(){
  return(c("Mean","Drift","Naive","Seasonal Naive","ETS","Auto ARIMA","FFNN"))
}

#.................................................................

# Returns list of available algorithms for selection..............

ensemble_algo_list<-function(){
  return(c("Bagging","Boosting","Bucket of Models","Bayesian Combination","Stacking"))
}

#.................................................................

# Returns list of available algorithms for selection..............

test_split_list<-function(){
  return(c(0.1,0.2,0.3,0.4,0.5))
}

#.................................................................


Mean<-function(train,test_start,end,frequency){
  return(autolayer(ts(meanf(train)[["mean"]],start = test_start, end = end,frequency = frequency),
                   series="Mean"))
}
Naive<-function(train,test_start,end,frequency){
return(autolayer(ts(naive(train)[["mean"]],start = test_start, end = end,frequency = frequency),
                 series="Naïve"))
}
Drift<-function(train,test_start,end,frequency){
return(autolayer(ts(rwf(train, drift=TRUE)[["mean"]],start = test_start, end = end,frequency = frequency),
                 series="Drift"))
}
S_Naive<-function(train,test_start,end,frequency){
return(autolayer(ts(snaive(train)[["mean"]],start = test_start, end = end,frequency = frequency),
                 series="Seasonal naïve"))
}
ETS<-function(train,test_start,end,frequency){
  return(autolayer(ts(forecast(train)[["mean"]],start = test_start, end = end,frequency = frequency),
                   series="ETS"))
}
A_Arima<-function(train,test_start,end,frequency){
  
  return(autolayer(ts(forecast(auto.arima(train))[["mean"]],start = test_start, end = end,frequency = frequency),
                   series="Auto Arima"))
}

FFNN<-function(train,test_start,end,frequency){
  return(autolayer(ts(forecast(nnetar(train))[["mean"]],start = test_start, end = end,frequency = frequency),
                   series="FFNN"))
}

Ensemble<-function(train,test_start,end,frequency,method,model){
  
  if(method =="Bagging"){
    out<-meanf(train)[["mean"]]
    print("Bagging")
  }
  
  
  if(method =="Boosting"){
    out<-meanf(train)[["mean"]]
    print("Bootsting")
  }
  
  
  if(method =="Bucket of Models"){
    out<-meanf(train)[["mean"]]
    print("Bucket")
  }
  
  
  if(method =="Bayesian Combination"){
    out<-meanf(train)[["mean"]]
    print("Bayesian Combo")
  }
  
  if(method =="Stacking"){
    out<-meanf(train)[["mean"]]
    print("Stacking")
  }
  
  if(is.null(model)){
    print("nool")
    return(NULL)
  }
  
  return(autolayer(ts(out,start = test_start, end = end,frequency = frequency),series="Ensemble"))
}













