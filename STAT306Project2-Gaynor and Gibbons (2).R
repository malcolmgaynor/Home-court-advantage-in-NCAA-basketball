#Import data
ByGame <- read.csv("~/Downloads/allNCAAdata.csv")

#EDA
library(ggcorrplot)
numbo <- read.csv("~/Downloads/awaynumbo.csv") #only numeric variables
cor(numbo) #correlation matrix
subset(numbo)
cor(awaynumbo)

subsetnumbo <- numbo[, c("Venue", "AwayFGmade", "AwayFGpct", "Away3ptpct", "AwayTotalRebounds", "AwaySteals", "AwayBlocks", "AwayTurnovers", "AwayFouls")]
cor(subsetnumbo)

#MLR
fullmodel=lm(W.L~AwayScore+AwayFGmade+AwayFGattempted+AwayFGpct+Away3ptmade+Away3ptattempted+Away3ptpct+AwayFreeThrowsMade+AwayFreeThrowsAttempted+AwayFreeThrowPct+AwayOffensiveRebounds+AwayTotalRebounds+AwayAssists+AwaySteals+AwayBlocks+AwayTurnovers+AwayFouls, data=ByGame)
summary(fullmodel)

MSE = (summary(fullmodel)$sigma)^2
step(fullmodel, scale=MSE, direction="backward")
backmodel=lm(W.L~AwayScore+AwayFGmade+AwayFGpct+AwayFreeThrowsAttempted+AwayOffensiveRebounds+AwayTotalRebounds+AwaySteals+AwayBlocks+AwayTurnovers+AwayFouls, data=ByGame)
summary(backmodel)
plot(backmodel)



#K-means clustering
# Loading package 
library(ClusterR) 
library(cluster) 

#Loading data
Clusterdata1 <- read.csv("~/Downloads/Clusterdata1.csv")
subsetclust1 <- Clusterdata1[, c("Venue", "AwayAssists", "AwayFGpct", "Away3ptpct", "AwayTotalRebounds", "AwaySteals", "AwayBlocks", "AwayTurnovers", "AwayFouls")]


# Fitting K-Means clustering Model  
# to training dataset 
set.seed(240) # Setting seed 
kmeans.re <- kmeans(subsetclust1, centers = 3, nstart = 20) 
kmeans.re 

# Cluster identification for  
# each observation 
kmeans.re$cluster 


Clusterdata <- Clusterdata %>%
  mutate(Cluster = kmeans.re$cluster) 

# Confusion Matrix 
cm <- table(subsetclust1$Venue, kmeans.re$cluster) 
cm 

## Visualizing clusters 
y_kmeans <- kmeans.re$cluster 
clusplot(subsetclust1[, c("AwayTotalRebounds", "AwayFGpct")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 4, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster Groupings"), 
         xlab = 'AwayTotalRebounds', 
         ylab = 'AwayFGpct') 

