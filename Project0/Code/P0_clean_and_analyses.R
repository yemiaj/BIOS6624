#######################

#Codes related to the cleaning and preliminary analysis of Project 0: COrtisol and DHEA

######################

cort.dat<-read.csv('./Project0/DataRaw/Project0_Clean_v2.csv') #Import .csv file for Project 0




# Check missingness
for (i in 1:15) print(c(i,table( is.na(cort.dat[,i]) |  cort.dat[,i]=='' ))) 


#check for consistency between booklet and MEMs clock time

#Spaghetti as well as means plot of cortisol and dhea (1)overall, (2)within each DAYNUMBER check for location and dispersion among the different days and overall.