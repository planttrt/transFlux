source('auxFunction.R')

sapfluxBW <- read.csv('data/sapFlux.BW.csv')
sapfluxParker <- read.csv('data/sapFlux.Parker.csv')

ameriParker <- readAmeriFlux('data/other data/North_Carolina_Loblolly_Pine/with_gaps/')
ameriBW <- readAmeriFlux('da')