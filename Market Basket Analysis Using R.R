getwd()
setwd("E:/R/Datasets/Market Basket Analysis")
groceries <- read.csv("groceries.csv")
head(groceries)


#libraries
library(reshape) # melt
install.packages(“arules”, dependencies=”TRUE”)
library(arules)


#help
help(pivot)


ncol(groceries)
nrow(groceries)


#Assigning column names to the Dataframe

for (i in 1:ncol(groceries)){
colnames(groceries)[i] <- c(paste("X",i,sep = ""))
}


#Assign rownumbers to the Dataframe
groceries$T.ID <- seq.int(nrow(groceries))

#Re-order the T.ID column to the first
#groceries <- groceries[,c("T.ID","X1","X2","X3","X4")]

head(groceries)

======================================================================
#Reshaping the Dataframe using to group by a single column keeping T.ID Constant
#refer http://www.statmethods.net/management/reshape.html

groceries_reshape <- melt(groceries, id=c("T.ID"))
head(groceries_reshape,100)

#Remove Null valued rows

groceries_remove_nulls <- subset(groceries_reshape, groceries_reshape$value  != '')

# Sort by transaction id

groceries_sorted5 <- groceries_remove_nulls[order(groceries_remove_nulls$T.ID),]
head(groceries_sorted5)



==========================================================================
#Do Apriori
	
#table(discretize(groceries_sorted5,method="interval", categories = 3))

#convert to factor
groceries_sorted5$T.ID = as.factor(groceries_sorted5$T.ID)


#apriori(groceries_sorted5,method="interval", categories = 3)

#Convert to Apriori
rules <- apriori(groceries_sorted5,parameter = list(supp = 0.0005, conf = 0.5))
summary(rules)
inspect(rules)

#Get the top results
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(top.support)


#top.ten.support <- sort.list(top.support,10)
#inspect(top.ten.support)
#ncol(groceries_sorted5)


write.csv(groceries_remove_nulls,file = "groceries_remove_nulls.csv",row.names=FALSE)
write.csv(temp,file = "groceries_temp.csv",row.names=FALSE)