library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)

structures <- read.csv("leagueoflegends/structures.csv")
View(structures)
structures$Address <- gsub(".*=","",structures$Address)
structures$Time <- as.integer(structures$Time) + 1


