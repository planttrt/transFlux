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
tmp <- as.data.table(rbind(tmp1,tmp2))

g <- ggplot(tmp, aes(Site, TR)) 
g + geom_violin(scale='area')

g <- ggplot(tmp , aes(WS)) 
g + geom_density(binwidth=2, aes(color=Site))

g <- ggplot(pkField, aes(TR, ET))
g + geom_point() + geom_smooth() +geom_abline(color='red')


