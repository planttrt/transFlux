library(data.table)
hw <- read.csv('data/hw/data.HW.csv')
hw <- as.data.table(hw)
hw[, plot(y+d/365, sap)]



xydata <- hw[!is.na(sap),.(dT= TA-STd  , Solar=Rg, Thermal=(STd+273.15)^4/1e10, sap=smooth(sap), sap.lai, lai)]
xydata <- na.omit(xydata)
ydata <- xydata[,.(sap, sap.lai)]

modelList <- list(ng = 2000, burnin = 1000, 
                  typeNames = rep('CA',2))

out <- gjamGibbs( ~ dT + Solar + Thermal ,#+ windy*dT + rainy*dT + cloud*dT,
                     xdata = xydata, ydata = ydata,
                     modelList = modelList)


gjamPlot(out, plotPars = list(width=4, height=4, sigOnly=F)) 


chains <- out$chains$bgibbs
transSumm <- as.data.table(t(apply(chains, quantile, MARGIN = 2, probs=c(.025,.975))))
rownames(transSumm) <- colnames(chains)
transSumm$sig <- rowMeans(sign(transSumm))
transSumm$var <- colnames(chains)
str(transSumm)
print(transSumm)


plotGibbsBoxplots(chains[1001:2000,] , textAdj = 0)
