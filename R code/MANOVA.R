###Preparation
library(tidyverse)
library(effectsize)
library(MASS)
library(dplyr)
install.packages("insight")
install.packages("parameters")
install.packages("mvnormtest")
library(mvnormtest)
install.packages("broom")
library(heplots)
library(gridExtra)

#read in data
all=read.csv(file=file.choose())


###EDA

#look at potentially relevant box plots
boxplot(AwayFGpct~Venue, data = all)
boxplot(Away3ptpct~Venue, data = all)
boxplot(AwayTotalRebounds~Venue, data = all)
boxplot(AwayAssists~Venue, data = all)
boxplot(AwaySteals~Venue, data = all)
boxplot(AwayBlocks~Venue, data = all)
boxplot(AwayTurnovers~Venue, data = all)
boxplot(AwayFouls~Venue, data = all)


###MANOVA

#to make MANOVA code easier, we define our dependent variables and our independent variable
depvs <- cbind(all$AwayFGpct,all$Away3ptpct,all$AwayTotalRebounds,all$AwayAssists,
             all$AwaySteals,all$AwayBlocks,all$AwayTurnovers,all$AwayFouls)
indepv <- all$Venue

#MANOVA test
our_manova <- manova(depvs ~ indepv, data = all)

#Summary of output
summary(our_manova)


###Shapiro-Wilk test for multivariate normality:

#taking only the relevant columns from our dataframe, called "all"
data_for_test <- subset(all, select = c(2,29,32,37:42))

#converting data frame into a matrix after omitting any rows with NA
matrix_for_test <- data.matrix(na.omit(data_for_test), rownames.force = NA)

#transposing to row format, because this function requires a row format matrix
mshapiro.test(t(matrix_for_test))


###Box's M test for homogeneity of the variance-covariance matrices:

#omit all rows with na
box_all = na.omit(all)

#set independent column names as box_indeps
box_indeps = c("AwayFGpct","Away3ptpct","AwayTotalRebounds","AwayAssists","AwaySteals","AwayBlocks","AwayTurnovers","AwayFouls")

#Box's M-test
boxM(Y = box_all[, box_indeps], group = box_all$Venue)


###Scatterplots for linearity assumptions

plot((all[all$Venue == 'WIS',])$AwayTurnovers~(all[all$Venue == 'WIS',])$AwayAssists)








