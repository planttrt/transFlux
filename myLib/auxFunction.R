
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
