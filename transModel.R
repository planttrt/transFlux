library(data.table)
library(gjam)

# source('~/Projects/transFlux/transPreModel.R')



modelList <- list(ng = 2000, burnin = 1000, typeNames = c('CA',rep('CON',2)))

xydata <- transData[Site=='PK',.(dT=(LST-TA), Solar=Rg, Thermal=(LST+273)^4, Site, WS, 
                        TR, dEVI, dNDVI, EVI, NDVI)]
xydata <- na.omit(as.data.frame(xydata))
ydata <- xydata[,c('TR','dEVI', 'dNDVI')]

str(xydata)
str(ydata)

colnames(xydata)

out <- gjamGibbs( ~ dT*WS + Solar + Thermal,
  #~ dT + Solar + Thermal + Site + EVI + WS + 
   #                 dT:Site + dT:Thermal + dT:Solar + dT:EVI + dT:WS,
           xdata = xydata, 
           ydata = ydata,
           modelList = modelList)

gjamPlot(out, plotPars = list(width=4, height=4))

