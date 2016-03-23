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


plot(pkFieldMODIS$STd-pkFieldMODIS$TA, pkFieldMODIS$sap.mm.day)
plot(bwFieldMODIS$STd-bwFieldMODIS$Ta, bwFieldMODIS$sapTot)

library(ggplot2)

qplot(STd-Ta, sapTot, data = bwFieldMODIS, geom = c('smooth','point'))



