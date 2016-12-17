
g <- ggplot(bothField, aes(Site, TR)) 
g + geom_violin(scale='area')

g <- ggplot(bothField , aes(WS)) 
g + geom_density(binwidth=2, aes(color=Site))

g <- ggplot(pkField, aes(TR, ET))
g + geom_point() + geom_smooth() +geom_abline(color='red')




g <- ggplot(bothFieldMODIS, aes(LST-TA, TR, size=Rg, color=Rg))
g + geom_point() + geom_smooth() +facet_grid(~Site)


plot(bwFieldMODIS$STd-bwFieldMODIS$Ta, bwFieldMODIS$TR)



source('../R.Repository/bijanFunctions.R')
source('../R.Repository/clarkFunctions.R')

library(mvtnorm)
library(pscl)
library(gjam)

x <- bothFieldMODIS[,.(intercept=1,  Site, dT=TA-LST, Solar=Rg, Thermal=(TA+273)^4)]
y <- bothFieldMODIS[,.(TR, EVI)]

xy <- bothFieldMODIS[,.(intercept=1,  Site, dT=TA-LST, Solar=Rg, Thermal=(TA+273)^4, TR, EVI)]
xy <- na.omit(xy)
x <- as.data.frame(xy[,1:5, with=F])
y <- as.data.frame(xy[,6:7, with=F])
modelList <- list(ng = 1000, burnin = 500, typeNames = c('CC','CC'))
gjamGibbs(~ Site + dT + Solar + Thermal, x, y, modelList  )


xy <- bothFieldMODIS[,.(intercept=1, dT=TA-LST, Solar=Rg, Thermal=(TA+273)^4, TR, EVI)]
xy <- na.omit(as.matrix(xy))


x <- xy[,1:4]
y <- xy[,5]

x[,-c(1,2)] <- scale(x[,-c(1,2)])
x[,-c(1)] <- scale(x[,-c(1)])

out <- runGibbs(x, y , n_burnin=1000, ng = 2000)
plotGibbs(out$bgibbs[,-1], burnin = 1000, plots = 'post')

y.dT <- y-x[,-2]%*%out$bMu[-2]
plot(x[,2],y.dT, )


plotObsPred(x%*%out$bMu, y)
abline(0,1, col='red')



## combinations of scales
typeNames <- c('OC','OC','OC','CC','CC','CC',
               'CC','CC','CA','CA','PA','PA')         
simMIX    <- gjamSimData(n=1000, S=length(typeNames), q=3, typeNames=typeNames)
modelList <- list(ng = 50, burnin = 5, typeNames = simMIX$typeNames)
outMIX    <- gjamGibbs(colna, simMIX$xdata, simMIX$y, modelList)

# repeat with ng = 2000, burnin = 500, then plot data:
plotPars  <- list(trueValues = simMIX$trueValues, width=3, height=2)
fit       <- gjamPlot(output = outMIX, plotPars)

