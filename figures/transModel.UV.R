library(data.table)
source('~/Projects/procVisData/dataViz.R')


if(!exists('transDataLoaded'))source('~/Projects/transFlux/transPreModel.R')

xydata <- transData[,
                    .(Site, 
                      dT=(LST-TA), 
                      Solar=Rg, 
                      Thermal= -0.96*(LST+273)^4 + 0.75*(TA+273)^4,
                      TA4 = (TA+273)^4,
                      LST4 = (LST+273)^4,
                      VPD,
                      WS,
                      PR,
                      windy = (WS > 2)*1,
                      rainy = (PR > 0)*1,
                      CC = Rg/RgOpen,
                      # ET,
                      dEVI, dNDVI, EVI, NDVI,
                      TR )]
str(xydata)
xydata <- na.omit(xydata)
xydata$cloudy <- xydata$CC<0.8


source('~/Projects/procVisData/bijanFunctions_tmp.R')
source('~/Projects/R.Repository/clarkFunctions.R')
x <- as.matrix(xydata[,.(dT, 
                         Solar,
                         # Thermal, 
                         # TA4, 
                         NDVI,
                         dT.NDVI =dT*NDVI,
                         LST4 = LST4, 
                         SolarBW = Solar*(Site=='PK'),
                         LST4.BW = LST4*(Site=='PK')
                         )])
                      
y <- as.matrix(xydata[,.(TR)])

outGibbs <- runGibbs(x, y, n_burnin = 1000, ng = 2000)
print(outGibbs$summary)
sdx <- c(apply(x, 2, sd), 1)
print(outGibbs$summary[,1]*sdx)

bg <- outGibbs$bgibbs[-(1:1000), ]
# plotGibbsBoxplots(t(t(bg)/sdx), rangeY = c(-1,1))

  