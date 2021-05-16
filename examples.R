
#Make reactive Data from fpp2 examples

make_example_data <- function(x){
  if(x=='ausbeer'){return(fpp2::ausbeer)}		
  if(x=='auscafe'){return(fpp2::auscafe)}		
  if(x=='austourists'){return(fpp2::austourists)}		
  if(x=='h02'){return(fpp2::h02)}		
  if(x=='usmelec'){return(fpp2::usmelec)}
}

#..........................................................................................
# input le liye h aage ka algorithm krna h
make_forecast <-function(ts,split =0.3,algos=NULL,method="Bagging",model=NULL){
  
  
  start<-start(ts)
  end<-end(ts)
  frequency<-frequency(ts)
  
  time<-round((end-start)*split)
  test_start = end-time
  
  train <-window(ts,start = start,end = test_start, frequency = frequency) 
  test  <-window(ts,start = test_start, end = end, frequency = frequency)   
  
  k <- autoplot(ts)
  for(x in algos){
    if(x =="Mean"){k = k + Mean(train,test_start,end,frequency)}
    if(x =="Drift"){k = k + Drift(train,test_start,end,frequency)}
    if(x =="Naive"){k = k + Naive(train,test_start,end,frequency)}
    if(x =="Seasonal Naive"){k = k + S_Naive(train,test_start,end,frequency)}
    if(x =="ETS"){k = k + ETS(train,test_start,end,frequency)}
    if(x =="Auto ARIMA"){k = k + A_Arima(train,test_start,end,frequency)}
    if(x =="FFNN"){k = k + FFNN(train,test_start,end,frequency)}
    if(x =="Ensemble"){k = k + Ensemble(train,test_start,end,frequency,method,model)}
  }


  return(k)
} 