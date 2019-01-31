#Converting csv file to R file
#Day3
#Nasreen Arend
#31 January 2019

Lam <- read.csv("data/laminaria.csv")
save(laminaria, file = "data/laminaria.RData")

