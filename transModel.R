library(data.table)
library(gjam)
source('~/Projects/procVisData/dataViz.R')


if(!exists('transDataLoaded'))source('~/Projects/transFlux/transPreModel.R')

xydata <- transData[,
                    .(Site, 
                      dT=(LST-TA), 
                      Solar=Rg, 
                      Thermal= 0.96*(LST+273)^4 + 0.75*(TA+273)^4,
                      LST4 = (LST+273)^4,
                      VPD,
                      WS,
                      PR,
                      windy = (WS > 2)*1,
                      rainy = (PR > 0)*1,
                      cloud = log(Rg/RgOpen),
                      TR, dEVI, dNDVI, EVI, NDVI)]

xydata.BW <- na.omit((xydata[Site=='BW']))
ydata.BW <- xydata.BW[,c('TR','dEVI', 'dNDVI')]

xydata.PK <- na.omit((xydata[Site=='PK']))
ydata.PK <- xydata.PK[,c('TR','ET')]

modelList <- list(ng = 2000, burnin = 1000, 
                  typeNames = c('CA',rep('CON',2)))

out.BW <- gjamGibbs( ~ dT + Solar + Thermal ,#+ windy*dT + rainy*dT + cloud*dT,
                     xdata = xydata.BW, ydata = ydata.BW,
                  modelList = modelList)

out.PK <- gjamGibbs( ~ dT + Solar + Thermal ,#+ windy*dT + rainy*dT + cloud*dT,
                     xdata = xydata.PK, ydata = ydata.PK,
                     modelList = modelList)

# gjamPlot(out, plotPars = list(width=4, height=4, sigOnly=F)) 

