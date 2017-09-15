library(data.table)
itemData = data.frame(item = c("pocketknife", "beans", "potatoes", 
                              "onions", "phone", "lemons",
                              "sleepingbag", "rope", "compass",
                              "umbrella", "sweater", "medicines", 
                              "others"), 
                     survivalpoints = c(15, 16, 13, 14, 20, 12, 17, 
                                        18, 17, 19, 10, 12, 11), 
                     weight = c(5, 6, 3, 4, 11, 2, 7, 
                                8, 10, 9, 1, 12, 11))

workData = transpose(itemData)
colnames(workData) <- workData[1,]
workData <- workData[1,]

while ((dim(workData)[1]<=100)) {
    singleRow <- sample(0:1,dim(workData)[2],replace = TRUE)
    workData = rbind(singleRow, workData)
  }

workData <- workData[1:100,]

# Traverse the dataframe and add survival points to workData
survivalWeights = function(workData) {
    workData <- workData[1:100,]
    workData <- as.data.frame((workData))
    workData$sPoints <- NULL
    workData$weight <- NULL
    sPoints = rep(0,100)
    weight = rep(0,100)
    for (i in 1:100) {
      for(j in 1:13){
        if (workData[i,j]==1) {
          sPoints[i] = sPoints[i]+itemData[j,2]
          weight[i] = weight[i]+itemData[j,3]
        }
      }
    }
    
    workData <- cbind(workData,sPoints,weight)
    
    # Select where weight <=80 kg
    workData <- workData[weight<=80,]
    
    #Select top 20 rows to perform cross-over and mutation
    workData <- as.data.table(workData)
    workData <- workData[order(-sPoints),head(.SD, 20),]
    workData <- as.data.frame(workData)
}

workData <- survivalWeights(workData)

mutStartProb = 0.5
#Crossover
# Crossover : Randomly select a point and swap the tails
fnCrossOver = function(parent1, parent2){
  
  idx = sample(2:(length(parent1)-2), 1)
  child1 = c(parent1[,1:idx], parent2[,(idx+1):length(parent2)])
  child2 = c(parent2[,1:idx], parent1[,(idx+1):length(parent1)])
  
  return(list(child1, child2))
}

#Mutate
fnMutate = function(individual){
  
  index = sample(1:length(individual), 1)
  individual[,index] = 1 - individual[,index]
  
  return(individual)
}

# select two rows at random from top 20
count <- 0
for (i in 1:20) {
  workData <- as.data.frame(workData)
  charAttr = sapply(workData, is.character)
  charAttr = names(workData[,charAttr])
  workData[,charAttr] = as.data.frame(lapply(workData[,charAttr], function(x) {as.numeric(x)}))
  
  newWorkData <- workData[0,]
  
  mut = mutStartProb/i
  
  
  while (nrow(newWorkData) < 100) {

    # Mutation
    if (runif(1,0,1) < mut){
      idx = sample(1:20, 1)
      newWorkData[nrow(newWorkData)+1,] = fnMutate(workData[idx,])
      
      if (nrow(newWorkData) == 100){break()}
    }
    else { #Crossover
      idx1 = sample(1:20, 1)
      idx2 = sample(1:20, 1)
      
      ls = fnCrossOver(workData[idx1,], workData[idx2, ])
      
      newWorkData <- rbind(newWorkData, ls[[1]])
      newWorkData <- rbind(newWorkData, ls[[2]])

      if (nrow(newWorkData) == 100){break()}
    }
  }
  newWorkData <- newWorkData[1:100,]
  newWorkData$sPoints <- NULL
  newWorkData$weight <- NULL
  newWorkData <- survivalWeights(newWorkData)
  if (max(workData$sPoints)== max(newWorkData$sPoints)){
    count = count+1
  }
  if(count==2){
    cat("Total survival points in iteration ", i, " = ", max(newWorkData$sPoints), "\n")
    break}
  workData <- newWorkData
}    

# This is the final result
head(workData,1)



