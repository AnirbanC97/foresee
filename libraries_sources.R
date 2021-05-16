#Libraries
library(forecast)
library(fpp2)
library(zoo)
library(tools)
library(DT)

#Source code
source("dataset_operations.R")
source("examples.R")
source("algorithms.R")


#Options & Settings
set.seed(123)
options(shiny.maxRequestSize=500*1024^2)
