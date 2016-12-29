library(ggplot2)
library(data.table)
source('~/Projects/procVisData/colorSet.R')
colList <- colList.brownGreen 

# png(filename = 'figures/transFluxFig.Climate.png', width = 7, height = 7, units = 'in',res = 150)
# 
# par(mfrow=c(2,1), mar=c(4,4,1,1))
# 
# dev.off()
# 


colnames(transData)

ggplot(data = transData, aes(Site, TA)) + 
  geom_boxplot(aes(color=Site)) + ylab('Tair (°C)') +
  ggsave(filename = 'figures/transFluxFig.Climate.TA.png',
         width = 4.5, height = 3, units = 'in', dpi=300)

ggplot(data = transData, aes(Site, WS)) + 
  geom_boxplot(aes(color=Site)) + ylab('Wind speed (m/s)') +
  ggsave(filename = 'figures/transFluxFig.Climate.WS.png',
         width = 4.5, height = 3, units = 'in', dpi=300)


ggplot(data = transData, aes(Site, RH)) + 
  geom_boxplot(aes(color=Site)) + ylab('Relative humidity (%)')+
  ggsave(filename = 'figures/transFluxFig.Climate.RH.png',
         width = 4.5, height = 3, units = 'in', dpi=300)


ggplot(data = transData, aes(Site, Rg)) + 
  geom_boxplot(aes(color=Site)) + ylab('Solar radiation (W/m2')+
  ggsave(filename = 'figures/transFluxFig.Climate.Rg.png',
         width = 4.5, height = 3, units = 'in', dpi=300)


ggplot(data = transData, aes(Site, VPD)) + 
  geom_boxplot(aes(color=Site)) + ylab('VPD (kPa)')+
  ggsave(filename = 'figures/transFluxFig.Climate.VPD.png',
         width = 4.5, height = 3, units = 'in', dpi=300)



annualPr <- rbind(
  transData.Orig$pkField[YEAR%in%(2007:2009),
                         .(totPR=sum(PREC, na.rm = T)), 
                         .(YEAR, DOY)][,.(AnnualPr=sum(totPR), nPR =sum(totPR>0), Site='BW'), .(Year=YEAR)],
  
  transData.Orig$bwField[Year%in%(2007:2009),
                         .(totPR=sum(Precip...mm., na.rm = T)),
                         .(Year, DOY)][,.(AnnualPr=sum(totPR), nPR =sum(totPR>0), Site='PK'), .(Year=Year)]
  )

ggplot(annualPr, aes(Year, AnnualPr, fill=Site)) + ylab('Annual precipitation (mm)')+
  geom_bar(stat="identity", position = "dodge")+
    ggsave(filename = 'figures/transFluxFig.Climate.tPR.png',
           width = 4.5, height = 3, units = 'in', dpi=300)
  

ggplot(annualPr, aes(Year, nPR, fill=Site)) + ylab('No of rainy days') + 
  geom_bar(stat="identity", position = "dodge")+
  ggsave(filename = 'figures/transFluxFig.Climate.nPR.png',
         width = 4.5, height = 3, units = 'in', dpi=300)

