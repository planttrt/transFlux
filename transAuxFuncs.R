library(maps)
library(raster)
library(fields)
calcSolar <- function(DOY, Lat, Slope=0, Aspect=0, Lon=0, SLon=0, DS=0, Elevation=0){
  
  #calculate solar radiation and related variables based on location, time and topographical conditions 
  
  DOY <- (DOY+365)%%365
  
  B  <-  (DOY - 81)*360/365
  ET  <-  9.87*sin(pi/180*2*B)- 7.53*cos(pi/180*B)-1.5*sin(pi/180*B)
  Delta <-   23.45*sin(pi/180*360/365 * (284 + DOY) )
  LST  <-  (DOY*24*60)%%(24*60)
  AST  <-  LST + ET + 4* (SLon - Lon) - DS 
  HourAngle  <-  (AST - 12*60)/4
  Altitude  <-   180/pi*asin(sin(pi/180*Lat)*sin(pi/180*Delta)+ cos(pi/180*Lat)*cos(pi/180*Delta)*cos(pi/180*HourAngle))
  
  #(*Sunset Time*)
  Hss  <-  180/pi*acos(-tan(pi/180*Lat)*tan(pi/180*Delta))/15
  
  #(*Sunrise Time*)
  Hsr  <-  -180/pi*acos(-tan(pi/180*Lat)* tan(pi/180*Delta))/15
  
  #(*Day Length*)
  DayLength  <-  Hss - Hsr
  
  #(*Solar Azimuth Angle*)
  #(*The angle of sun's ray measured in the horizental plane from due south*)
  Azimuth1  <-  180/pi*asin(cos(pi/180*Delta)*
                              sin(pi/180*HourAngle)/cos(pi/180*Altitude))
  tmp <- AST < 12*60
  Azimuth2 <- 
    (-180 + abs(Azimuth1))*tmp +    (Azimuth2 <- -180 + abs(Azimuth1))*(!tmp)
  
  #   if (AST < 12*60){
  #     Azimuth2 <- -180 + abs(Azimuth1)
  #   }else{
  #     Azimuth2 <- 180 - Azimuth1
  #   }
  
  #   if (cos(pi/180*HourAngle)> tan(pi/180*Delta)/tan(pi/180*Lat)){
  #     Azimuth  <-  Azimuth1
  #   }else{
  #     Azimuth  <-  Azimuth2
  #   }
  
  tmp <- (cos(pi/180*HourAngle)> tan(pi/180*Delta)/tan(pi/180*Lat))
  Azimuth <- tmp*Azimuth1+Azimuth2*(!tmp)
  #(*Solar Incidence Angle*)
  #(*The angle between sun's ray and the normal on a surface*)
  Incidence  <-  180/pi*acos(sin(pi/180*Lat)*sin(pi/180*Delta)*cos(pi/180*Slope)- cos(pi/180*Lat)*sin(pi/180*Delta)* sin(pi/180*Slope)* cos(pi/180*Aspect)+ cos(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*HourAngle)*cos(pi/180*Slope)+         sin(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*HourAngle)*sin(pi/180*Slope)*cos(pi/180*Aspect)+        cos(pi/180*Delta)*sin(pi/180*HourAngle)*sin(pi/180*Slope)*sin(pi/180*Aspect)        )
  
  
  a0 <- 0.4237-0.00821*(6-Elevation/1000.0)^2
  a1 <- 0.5055+0.00595*(6.5-Elevation/1000.0)^2
  k <- 0.2711+0.01858*(2.5-Elevation/1000.0)^2
  tb <- (a0+a1*exp(-k/sin(pi/180*Altitude)))*(Altitude>0)
  td <- 0.271-0.294*tb
  
  
  
  Sc <- 1366.1
  Sextr <- Sc*(1+0.033*cos(pi/180*360*DOY/365))
  Sopen <- tb*Sextr
  SextrNormal <- Sextr*cos(pi/180*Incidence)
  
  
  Sdiropen <- Sopen*cos(pi/180*Incidence)
  Sdifopen <- Sopen*td*(sin(pi/180*Altitude))^2*(cos(pi/180*Slope/2.))^2
  
  
  
  list(Daylength=DayLength ,Altitude=Altitude, Azimuth=Azimuth, Incidence=Incidence, Sdiropen=Sdiropen, Sdifopen=Sdifopen)
}

readAmeriFlux <- function(folder){
  #read all files for an AmeriFlux site from a given folder, each file in the folder is assumed as time series one year measurements data
  #input: folder
  #output: a list with units, data and coverage of each column
  
  f <- dir(folder ,pattern = '*.csv', full.names = T)
  
  amer <- list('units'=NULL, coverage=NULL, data=NULL)
  prog <- txtProgressBar(min = 1, max = length(f), style = 3, initial = 1)
  
  for(i in 1:length(f)){
    tmp <- read.table(f[i], sep = ',', skip = 17, header = T, row.names = NULL)
    amer$units <- rbind(amer$units, tmp[1,])
    amer$coverage <- rbind(amer$coverage, tmp[2,])
    amer$data <- rbind(amer$data, read.table(f[i], sep = ',', skip = 20, header = F, row.names = NULL))
    setTxtProgressBar(prog, i)
    
  }
  colnames(amer$data) <- colnames(tmp)
  
  amer$data[amer$data == -6999] <- NA
  amer$data[amer$data == -9999] <- NA
  
  # amer$data$ET <- amer$data$LE/((2.502*10^3-2.308*amer$data$TA)/1000)
  #amer$data <- cbind(amer$data, ET)
  
  amer
}

dateToDOY <- function(y, m, d){
  doy <- y*0
  for(i in 1:length(y)){
    md <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if(y[i]%%4==0) md <- md <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
    sumd <- c(0, cumsum(md))
    doy[i] <- sumd[m[i]]+d[i]
  }
  doy
}



library(rjags)

lmJAGS <- function(x, y,
                   n.chains=4,
                   n.adapt=100,
                   n.burnin=1000,
                   n.gibbs=1000){
  model <- textConnection(
    "model {
    for (i in 1:N){
    y[i] ~ dnorm(x[i,]%*%beta, tau)
    }
    for (i in 1:p) 
    {
    beta[i] ~ dnorm(0, .0001)
    }
    tau <- pow(sigma, -2)
    sigma ~ dunif(0,100)
    
    for (i in 1:N){
    ypred.tmp[i] ~ dnorm(x[i,]%*%beta, tau)
    ypred[i] <- max(0, ypred.tmp[i])
    }
}
")
  
  
  jags <- jags.model(model,
                     data = list('x' = x,
                                 'y' = y,
                                 'N' = nrow(x),
                                 'p'= ncol(x)),
                     n.chains=n.chains,
                     n.adapt=n.adapt)
  
  update(jags, n.burnin)
  
  out <- jags.samples(jags,
                      c('beta',
                        'sigma',
                        'ypred'),
                      n.gibbs)
  
  out
}


lmtJAGS <- function(x, y,
                    n.chains=4,
                    n.adapt=100,
                    n.burnin=1000,
                    n.gibbs=1000){
  model <- textConnection(
    "model {
  for (i in 1:N){
  y[i] ~ dnorm(x[i,]%*%beta, tau)T(0,)
  }
  for (i in 1:p) 
  {
  beta[i] ~ dnorm(0, .0001)
  }
  tau <- pow(sigma, -2)
  sigma ~ dunif(0,100)

  for (i in 1:N){
    ypred[i] ~ dnorm(x[i,]%*%beta, tau)T(0,)
  }
  }
  ")
  
  
  jags <- jags.model(model,
                     data = list('x' = x,
                                 'y' = y,
                                 'N' = nrow(x),
                                 'p'= ncol(x)),
                     n.chains=n.chains,
                     n.adapt=n.adapt)
  
  update(jags, n.burnin)
  
  out <- jags.samples(jags,
                      c('beta',
                        'sigma',
                        'ypred'),
                      n.gibbs)
  
  out
}



plotMonthlySpatial <- function(rlist, 
                               colList = terrain.colors(10),
                               rng=NULL, 
                               xlim=NULL, ylim=NULL, 
                               legendPos= c(.78, .82, .05, .4),
                               lwdContour=1, cexLegend =1,
                               sameRange =T, nlevelsContour=5){
  
  # if(is.null(rng))rng <- range(sapply(rlist, function(x)(quantile(x@data@values, probs=c(.05, .95),na.rm = T))), na.rm = T)
  if(is.null(rng))rng <- range(sapply(rlist, quantile, probs=c(.05, .95),na.rm = T), na.rm = T)
  if(is.null(xlim)) xlim <- rlist[[1]]@extent[1:2]
  if(is.null(ylim)) ylim <- rlist[[1]]@extent[3:4]
  
  tmp <- rlist[[1]]
  lr <- raster(ncol=floor(ncol(tmp)/12),
               nrow=floor(nrow(tmp)/12))
  extent(lr) <- extent(tmp)
  
  col <- colorRampPalette(colList)(100)
  par(mfrow=c(4,3), mar=c(0,0,1.5,0))
  for(i in 1:12){
    r <- rlist[[i]]
    rlr  <- resample(r, lr)
    par(bty='n')
    if(!sameRange) rng <- range(dt[[1]]@data@values, na.rm = T)
    plot(r, breaks=seq(rng[1], rng[2],length.out = 100),
         xlim=xlim, ylim=ylim,
         axes=FALSE,legend=FALSE,  col=col)
    contour(rlr, nlevels = nlevelsContour, add=T, lwd=lwdContour)
    
    map('usa', add=T)
    
    mtext(month.name[i], line = 0, font = 2)
    if(i==12|!sameRange){
      par(bty='o')
      image.plot(legend.only=TRUE, zlim= rng, 
                 smallplot= legendPos,
                 axis.args = list(cex.axis = cexLegend, font=2),
                 legend.args = list(text= '', side=3,xpd=T, adj=0, line=.3, font=2), 
                 col = col, horizontal = F, yaxt='s') 
    }
  }
}
rMean <- function(rList){
  m <- rList[[1]]
  for(i in 2:length(rList)) m <- m + rList[[i]]
  m/length(rList)
}
