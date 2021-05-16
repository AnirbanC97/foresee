#Make reactive Dataset.....................................................

make_data <- function(x){

inFile <- x
if(is.null(inFile)){
  return(NULL)
}

else{
print(inFile$datapath)
ext<-file_ext(inFile$datapath)

if(ext=='csv'){
  data<-read.csv(inFile$datapath)}

else if(ext=='xlsx'){
  file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
  data<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
}
hideTab(inputId = "tabpanel", target = "Example-Report")
showTab(inputId = "tabpanel", target = "Dataset")

return(data)

}

}
#........................................................................................
#Example Data List
example_data <- function(){return(c('ausbeer','auscafe','austourists','usmelec','h02'))}	












