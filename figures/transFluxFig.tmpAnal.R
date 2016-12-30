library(gjam)
out <- out.BW
chains <- out$chains$bgibbs[1001:2000,]
sdy <- apply(out.BW$y, 2, sd)
sdx <- apply(out.BW$x, 2, sd)

gjamPlot(out, plotPars = list(width=4, height=4, sigOnly=F)) 
colnames(chains)
plotGibbsBoxplots(chains[,c('TR_dT','TR_Solar','TR_Thermal' )]*1e19/
                    matrix(sdx[-1], nrow = nrow(chains),ncol = 3, byrow = T) )
# plotGibbsBoxplots(out$chains$bgibbs[1001:2000,c("TR_dT","TR_dT")] )
# 
transSumm <- as.data.table(t(apply(chains, quantile, MARGIN = 2, probs=c(.025,.975))))
rownames(transSumm) <- colnames(chains)
transSumm$sig <- rowMeans(sign(transSumm))
transSumm$var <- colnames(chains)
str(transSumm)
print(transSumm)


postGibbsChains(chains, sdTraits = sdy, output.x = out.BW$x, )
