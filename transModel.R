library(data.table)
library(gjam)

# source('~/Projects/transFlux/transPreModel.R')



modelList <- list(ng = 2000, burnin = 1000, typeNames = c('CA','CON','CON'))

xydata <- transData[Site=="PK",.(intercept=1, dT=(TA-LST)*WS, Solar=Rg, Thermal=(TA+273)^4, BW = Site=='BW', Site, 
                        TR, dEVI, dNDVI, EVI, NDVI)]
xydata <- na.omit(as.data.frame(xydata))
ydata <- xydata[,c('TR','dEVI', 'dNDVI')]

str(xydata)
str(ydata)

colnames(xydata)

out <- gjamGibbs( ~ dT + Solar + Thermal,
           xdata = xydata, 
           ydata = ydata,
           modelList = modelList)

gjamPlot(out, plotPars = list(width=4, height=4))

