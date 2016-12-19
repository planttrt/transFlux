library(data.table)
library(gjam)

# source('~/Projects/transFlux/transPreModel.R')
source('~/Projects/procVisData/dataViz.R')



# transData[Year==2010,TR:=NA]

xydata <- transData[,.(Site, 
                       dT=(LST-TA), 
                       Solar=Rg,
                       Thermal=(LST+273)^4,
                       WS, 
                       TR, dEVI, dNDVI, EVI, NDVI)]
xydata <- na.omit((xydata))
ydata <- xydata[,c('TR','dEVI', 'dNDVI')]

str(xydata)
str(ydata)
summary(xydata)
colnames(xydata)


modelList <- list(ng = 2000, burnin = 1000, typeNames = c('CA',rep('CON',2)))

out <- gjamGibbs( ~ dT + Solar + Thermal + Site +
                    Site:Solar + Site:Thermal
                  ,
                  xdata = xydata, 
                  ydata = ydata,
                  modelList = modelList)

gjamPlot(out, plotPars = list(width=4, height=4, sigOnly=F))

# plotGibbsBoxplots(out$chains$bgibbs[1001:2000,c("TR_dT","TR_dT")] )  
t(apply(out$chains$bgibbs, quantile, MARGIN = 2, probs=c(.025,.975)))
