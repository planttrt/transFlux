library(gjam)

load('data/forestTraits.RData')
traitTypes <- forestTraits$traitTypes

x <- read.csv('~/Projects/traitsModel/data/post/plotByX.csv')
y <- read.csv('~/Projects/traitsModel/data/post/plotByW.csv')
ww <- match(colnames(y), rownames(forestTraits$specByTrait))
plotByTree <- y[,!is.na(ww)]
ww <- match(colnames(plotByTree), rownames(forestTraits$specByTrait))
specByTrait <- forestTraits$specByTrait[ww,]

dim(specByTrait)

tmp <- gjamSpec2Trait(pbys = plotByTree, 
                      sbyt = specByTrait, 
                      tTypes = traitTypes)

dim(tmp$plotByCWM)


source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')
source('~/Projects/procVisData/colorSet.R')

png('figures/transFluxFig.HydroTraits.png', res=300, units = 'in', height = 6, width = 6)
par(oma=c(2,2,2,1), mfrow=c(2,2), mar=c(0,0,1,0))
for(traits in c('leafN','leafP','SLA','ring')){
  # plot(NA, xlim=c(-90,-75), ylim=c(25,40), xlab='',ylab='', bty='n', axes=F)
  plot(physio, xlim=c(-90,-75), ylim=c(25,40), axes=F)
  mapColorData(x$plotLon, x$plotLat, tmp$plotByCWM[,traits], 
               colList = colList.Contad, symSize = .4, ADD = T)
mtext(text = traits)
    plot(physio, add=T)
}
# map('usa', add = T)
dev.off()





