# Poker hands dataset


#### load data ####
poker = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/poker/poker-hand-training-true.data",
                 header = FALSE, sep = ",", strip.white = TRUE, na.strings="NA", stringsAsFactors = TRUE)
names(poker)<- c("s1","c1","s2","c2","s3","c3","s4","c4","s5","c5","Class")            
poker = na.omit(poker)
#Class as factor
poker$Class = as.factor(poker$Class)
summary(poker$Class)


#### process data ####
new <- poker[,c(2,4,6,8,10,1,3,5,7,9,11)]
new["d1"] <- NA
new["d2"] <- NA
new["d3"] <- NA
new["d4"] <- NA
new["d5"] <- NA
new["flush"] <- NA


for(i in 1:nrow(new)){
  #sort the cards, then calculate the difference between cards
  x <- new[i,1:5]
  x <- sort(x)
  new[i,"d1"]<- x[,2]- x[,1] 
  new[i,"d2"]<- x[,3]- x[,2]
  new[i,"d3"]<- x[,4]- x[,3]
  new[i,"d4"]<- x[,5]- x[,4]
  new[i,"d5"]<- x[,5]- x[,1]
 
  #boolean flush
   if(new[i,"s1"] == new[i,"s2"] & new[i,"s1"] == new[i,"s3"] & new[i,"s1"] == new[i,"s4"] & new[i,"s1"] == new[i,"s5"]){
    new[i,"flush"] <- 1
  }
  else {
    new[i,"flush"] <- 0
  }
  
}


#### write new data ####
# myData <- new[, c(12,13,14,15,16,17,11)]
# 
# write.csv(myData, file = "feature_train.csv", row.names = FALSE)

#sort by class
myData <- myData[order(myData$Class, decreasing = TRUE),]

write.csv(myData, file = "feature_train_sort.csv", row.names = FALSE)

