####### Travelling sales man problem solved with genetic algorithm in R

# Setting the work directory
setwd("C:/Users/kmsra_000/Documents/Machine_Learning/INSOFE/W12,D2 - MonteCarlo, Genetic Algo/Lab")

# Read the data into a csv file
distMatrix = read.csv("distanceinfo.csv")

# initiate a workData dataframe with colnames same as distance matrix
workData = distMatrix[0,]

# populate workdata with 500 random chromosomes to begin with
numCols <- ncol(workData)
size <- 500

# Assuming the first city is the city where the person is currently located at (departure city)
# so the first city is always 1
for (i in 1:size) {
  depCity <- 1
  randomRow <- c(depCity,sample(2:ncol(workData), ncol(workData)-1))
  workData <- rbind(workData,randomRow)
}

# set the column names of workData to the column names in distMatrix dataframe
colnames(workData) <- colnames(distMatrix)

# Compute the total distance for the chromosome and then add it a totalDist column. 

totalDistance = function(workData){
  size <- nrow(workData)
  totalDist <- rep(0,size)
  workData$totalDist <- NULL
  # Traverse the dataset and compute the total distance for each chromosome
  for (i in 1:size){
    for (j in 1:(numCols-1)){
      fromCity <- workData[i,j]
      toCity <- workData[i,j+1]
      totalDist[i] <- totalDist[i] + distMatrix[fromCity, toCity]
    }
  }
  
  #Append this column to the workData
  workData <- cbind(workData,totalDist)
  
  # Arrange the rows in ascending order
  workData <- workData[order(totalDist),]
  
  # select top 100 rows with minimum distance
  workData <- head(workData,100)
  workData <- as.data.frame(workData)
}

# initiate the workData with the above function call to add the totalDist column
workData <- totalDistance(workData)


# Crossover : Doing an ordered cross-over. Select a random cut from parent1 and then do a setdiff
# on this with elements in parent2 so as to select the remaining elements from parent2. Then form a child.

fnCrossOver = function(parent1, parent2){
  
  idx = sample(2:length(parent1)-1, 1)
  laterPart1 = c(parent2[,idx:length(parent1)])
  frontPart1 = setdiff(parent2,laterPart1)
  child1 = c(frontPart1,laterPart1)
  
  idx = sample(2:length(parent2)-1, 1)
  laterPart2 = c(parent1[,(idx):length(parent2)])
  frontPart2 = setdiff(parent1,laterPart2)
  child2 = c(frontPart2,laterPart2)
  return(list(child1, child2))
}

#Mutate - Here we are swapping two randomly selected elements
fnMutate = function(individual){
  
  idx1 = sample(2:length(individual), 1)
  idx2 = sample(2:length(individual), 1)
  # swapping the elements
  temp <- individual[,idx1]
  individual[,idx1] <- individual[,idx2]
  individual[,idx2] <- temp
  
  #laterPart <- rev(individual[,idx1:length(individual)])
  #frontPart <- individual[,1:idx1-1]
  #individual <- cbind(frontPart,laterPart)
  individual <- cbind(1,individual)
  return(individual)
}

## Genetic algorithm that does the remaining once the workData is readied.
count <- 0
mutStartProb = 1
for (i in 1:100) {
  eliteData <- workData
  eliteData$totalDist <- NULL
  newWorkData <- workData[0,]
  newWorkData$totalDist <- NULL
  mut = mutStartProb/i
  
  while (nrow(newWorkData) < 500) {
    
    # Mutation
    if (runif(1,0,1) < mutStartProb){
      idx = sample(1:100, 1)
      newWorkData[nrow(newWorkData)+1,] = fnMutate(eliteData[idx,2:10])
      
      if (nrow(newWorkData) == 500){break()}
    }
    else { #Crossover
      idx1 = sample(1:100, 1)
      idx2 = sample(1:100, 1)
      
      ls = fnCrossOver(eliteData[idx1,], eliteData[idx2, ])
      
      newWorkData <- rbind(newWorkData, ls[[1]])
      newWorkData <- rbind(newWorkData, ls[[2]])
      
      if (nrow(newWorkData) == 500){break()}
    }
  }
  newWorkData <- newWorkData[1:500,]
  
  # Compute the distance for each chromosome in the newly formulated newWorkData
  # NOTE - This function call does the top 100 row selection as well.
  #newWorkData <- na.omit(newWorkData)
  newWorkData <- totalDistance(newWorkData)

  # Stopping criteria - if the minimum is repeated twice in both the workData and newWorkData, that means the minimum
  # been achieved. So breaking the loop.
  
  if (min(workData$totalDist)== min(newWorkData$totalDist)){
    count = count+1
  }
  if(count==5){
    cat("Minimum distance achieved in iteration ", i, " = ", min(newWorkData$totalDist), "\n")
    break}
  
  # copy the newWorkData back to workData and repeat the loop for the next iteration
  workData <- newWorkData
}    

# This is the final result
head(workData,1)


