source('~/Projects/procVisData/dataViz.R')

bg <- bothSites$pk$betaStd
colnames(bg)
# plotGibbsBoxplots(bg, textAdj = 0, sigPlot = F, sort = F)

png('figures/transFluxFig.Betas.png', res=150, width = 6, height = 4, units = 'in')
par(mar=c(4,4,2,1))
plotGibbsBoxplots(bg[,c("dT", "Solar", "LST4")], textAdj = 0, sigPlot = F, sort = F, labels = c('dT','Solar','Thermal'))
dev.off()



