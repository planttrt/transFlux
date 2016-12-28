library(data.table)
library(gjam)
source('~/Projects/procVisData/dataViz.R')


source('~/Projects/transFlux/transPreModel.R')

# transData[Year==2010,TR:=NA]

xydata <- transData[Site=='BW',.(Site, 
                                 dT=(LST-TA), 
                                 Solar=Rg, 
                                 Thermal= 0.96*(LST+273)^4 + 0.75*(TA+273)^4,
                                 LST4 = (LST+273)^4,
                                 Site, 
                                 WS,
                                 PR,
                                 windy = (WS > 2)*1,
                                 rainy = (PR > 0)*1,
                                 cloud = log(Rg/RgOpen),
                                 TR, dEVI, dNDVI, EVI, NDVI)]
xydata <- na.omit((xydata))
ydata <- xydata[,c('TR','dEVI', 'dNDVI')]

str(xydata)
str(ydata)
summary(xydata)
colnames(xydata)


modelList <- list(ng = 2000, burnin = 1000, typeNames = c('CA',rep('CON',2)))

out <- gjamGibbs( ~ (dT + Solar + Thermal)*EVI
                  # + Site +
                  #   Site:Solar + Site:Thermal
                  ,
                  xdata = xydata, 
                  ydata = ydata,
                  modelList = modelList)

# gjamPlot(out, plotPars = list(width=4, height=4, sigOnly=F))

plotGibbsBoxplots(out$chains$bgibbs[1001:2000,c("TR_dT","TR_dT")] )

transSumm <- t(apply(out$chains$bgibbs, quantile, MARGIN = 2, probs=c(.025,.975)))
transSumm[,sig:=(sign(`2.5%`)+sign(`97.5%`))/2]
rownames(transSumm) <- colnames(out$chains$bgibbs)
str(transSumm)
print(transSumm)
