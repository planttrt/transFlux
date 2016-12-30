postGibbsChains <- function(betachains,
                            burnin=1, 
                            traitsToPlot=NULL,
                            predictorsToPlot=NULL, 
                            normalized = T,
                            standardizedT = F, 
                            includeInteractions=T,
                            includeMainEffects =T,
                            excludeIntercept = T,
                            onlySignificant=T, 
                            sdTraits=NULL,
                            output.x
                            ){
  
  wFactors <- which(apply(output.x, 2, function(x)all(x%in%c(0,1))))
  sdCols <- apply(output.x, 2, sd)
  sdCols[wFactors] <- 1
  
  
  chains <- betachains[-(1:burnin),]
  
  ng <- nrow(chains)
  nbeta <- ncol(chains)
  fullMatrix <- matrix(unlist(
    strsplit(colnames(chains), split = '_')), 
    ncol=2, byrow = T)
  colnames(fullMatrix) <- c('trait', 'predictor')
  fullMatrix <- as.data.table(fullMatrix)
  id <- 1:nbeta
  fullMatrix <- cbind(id, fullMatrix)
  fullMatrix$interaction <- grepl(':',fullMatrix$predictor)
  fullMatrix
  fullMatrix[, pred:=strsplit(predictor,':')]
  tmp <- function(x){
    if(length(x)!=1) {
      return(unlist(x))
    }else{
      return(c(unlist(x),NA))
    }
  }
  fullMatrix$pred1 <- t(sapply(fullMatrix$pred, tmp))[,1]
  fullMatrix$pred2 <- t(sapply(fullMatrix$pred, tmp))[,2]
  
  
  sdCols <- sdCols[match(fullMatrix$predictor, names(sdCols))]
  sdTraits <- sdTraits[match(fullMatrix$trait, names(sdTraits))]
  
  if(normalized) chains <- t(t(chains)*sdCols)
  if(standardizedT) chains <- t(t(chains)/sdTraits)
  
  summChains <- t(apply(chains, 2, quantile, probs=c(.5,.025,.975)))
  colnames(summChains) <- c('median','low','high')
  fullMatrix <- cbind(fullMatrix, summChains)
  fullMatrix[, signifcant:=sign(high*low)==1]
  fullMatrix
  if(is.null(traitsToPlot)) traitsToPlot <- unique(fullMatrix$trait)
  if(is.null(predictorsToPlot)) predictorsToPlot <- unique(fullMatrix$predictor)
  
  predictorFilter = 1:nrow(fullMatrix)%in%unique(unlist(apply(as.matrix(predictorsToPlot), 1, grep, fullMatrix$predictor)))
  interactionFilter<- c(includeInteractions, !includeMainEffects)
  if(!(includeInteractions|includeMainEffects)) interactionFilter <- c()
  
  nameMatrix <- fullMatrix[
    trait%in%traitsToPlot&
      (predictor%in%predictorsToPlot|
         pred1%in%predictorsToPlot|
         pred2%in%predictorsToPlot)&
      #predictorFilter&
      (signifcant|!onlySignificant)&
      interaction%in%interactionFilter&
      ((predictor!='intercept')|!excludeIntercept)
    , ]
  
  list(    chains = chains[, nameMatrix$id],
           nameMatrix = nameMatrix,
           fullchain =chains,
           fullMatrix = fullMatrix
  )
}
