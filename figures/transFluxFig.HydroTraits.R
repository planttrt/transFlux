library(gjam)

load('~/Downloads/forestTraits.RData')
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

png('figures/xylim-ring.png', res=300, units = 'in', height = 7, width = 6)
mapColorData(x$plotLon, x$plotLat, tmp$plotByCWM[,"ring"], colList = colList.Contad, symSize = .7)
title('Xylem - Ring')
dev.off()
