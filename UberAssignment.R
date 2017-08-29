
#Remove all variables
rm(list=ls())

library(dplyr)

#Read CSV File
UberData <- read.csv("Uber Request Data.csv")

#======= Data Cleaning and Preparation ========

# Check duplicate values
