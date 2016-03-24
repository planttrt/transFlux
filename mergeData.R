library(ggplot2)
library(data.table)


#merge Blackwood
bwMODIS <- merge(bwMODIS.SR, bwMODIS.LST, by = 'yd', all=T)
bwMODIS$ydh <- bwMODIS$yd*100 + floor(bwMODIS$VTd)+1

bwField <- merge(bwRAWS, bwSap, by = 'ydh', all=T)

bwFieldMODIS <- merge(bwMODIS, bwField, by='ydh')


#merge Parker
pkMODIS <- merge(pkMODIS.SR, pkMODIS.LST, by = 'yd', all=T)
pkMODIS$ydh <- pkMODIS$yd*100 + floor(pkMODIS$VTd)+1

pkField <- merge(pkAmeriFlux, pkSap, by = 'ydh', all=T)
pkFieldMODIS <- merge(pkMODIS, pkField, by='ydh')


# plot(pkFieldMODIS$STd-pkFieldMODIS$TA, pkFieldMODIS$sap.mm.day)
# plot(bwFieldMODIS$STd-bwFieldMODIS$Ta, bwFieldMODIS$sapTot)


# qplot(STd-Ta, sapTot, data = bwFieldMODIS, geom = c('smooth','point'))

colnames(bwField)
colnames(pkField)

nmCol <- c('ydh', 'TA', 'Rg', 'VPD', 'WS', 'RH','WD','TR')

tmp1 <- cbind(bwField[,nmCol],'BW')
tmp2 <- cbind(pkField[,nmCol],'PK')
colnames(tmp1) <- colnames(tmp2) <- c(nmCol, 'Site')
bothField <- as.data.table(rbind(tmp1,tmp2))

g <- ggplot(bothField, aes(Site, TR)) 
g + geom_violin(scale='area')

g <- ggplot(bothField , aes(WS)) 
g + geom_density(binwidth=2, aes(color=Site))

g <- ggplot(pkField, aes(TR, ET))
g + geom_point() + geom_smooth() +geom_abline(color='red')



colnames(pkFieldMODIS)
colnames(bwFieldMODIS)
nmCol <- c('ydh', 'TA', 'Rg', 'VPD', 'WS', 'RH','WD','TR','b1','b2','b3','NDVI','EVI', 'STd','QCd','STn','QCn', 'LST')

tmp1 <- cbind(bwFieldMODIS[,nmCol],'BW')
tmp2 <- cbind(pkFieldMODIS[,nmCol],'PK')
colnames(tmp1) <- colnames(tmp2) <- c(nmCol, 'Site')
bothFieldMODIS <- as.data.table(rbind(tmp1,tmp2))


g <- ggplot(bothFieldMODIS, aes(LST-TA, TR, size=Rg, color=Rg))
g + geom_point() + geom_smooth() +facet_grid(~Site)


plot(bwFieldMODIS$STd-bwFieldMODIS$Ta, bwFieldMODIS$TR)
