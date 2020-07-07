library(tidyverse)
library(tidyquant)
library(timetk)
library(tseries)
asi<-ts(read.csv("asi.csv",header= F),frequency=12, start=c(1991,1))
head(asi)
asi_tbl<-ts(asi[,],start=1991, frequency = 12)#Coerced to numeric
str(asi_tbl)
asi_tstk<-tk_ts(asi_tbl, start=1991, frequency=12);asi_tstk
adf.test(asii, alternative = "stationary")
library(fractal)
library(pracma)
hurstexp(asi) #Test for long memory using Hurst Exponent
library(fracdiff)
fdGPH(asii, bandw.exp = 0.5) #Order of differencing estimation
adf.test(diffseries(asi,0.204), alternative = "stationary")
library(arfima)
fitArfima4 <- arfima(asi,order=c(4,0,1),fixed=list(frac=0.204),dmean=FALSE);summary(fitArfima4) #Best model
