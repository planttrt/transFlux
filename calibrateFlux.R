hwGap <- readAmeriFlux('ameriflux_data/Duke_Forest_Hardwoods/gap_filled')
hwFill <- readAmeriFlux('ameriflux_data/Duke_Forest_Hardwoods/with_gaps/')
ncGap <- readAmeriFlux('ameriflux_data/North_Carolina_Loblolly_Pine/with_gaps/')

plt <- function(data) plot(data$data$YEAR+(data$data$DTIME-1)/366, data$data$LE)

par(mfcol=c(3,1))

library(data.table)

nc <- as.data.table(ncGap$data)
hw <- as.data.table(hwFill$data)

p.nc <- nc[YEAR%in%c(2005:2008), .(p=sum(PREC, na.rm = T)), YEAR]
p.hw <- hw[YEAR%in%c(2005:2008), .(p=sum(PREC, na.rm = T)), YEAR]

m.nc <- nc[YEAR%in%c(2005:2008), .(m=mean(LE, na.rm = T)), YEAR]
m.hw <- hw[YEAR%in%c(2005:2008), .(m=mean(LE, na.rm = T)), YEAR]
m.nc$m/m.hw$m
