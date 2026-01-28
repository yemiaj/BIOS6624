#######################

#Codes related to the cleaning and preliminary analysis of Project 0: COrtisol and DHEA

######################

#Import .csv file for Project 0
cort.dat <- read.csv('./Project0/DataRaw/Project0_Clean_v2.csv') 

str(cort.dat) #See variable types and values

cort.dat$samp.date <- as.Date(cort.dat$Collection.Date, format = '%m/%d/%Y')
cort.dat$samp.date.num <- as.numeric(cort.dat$samp.date)


cort.dat <- cort.dat[, c("SubjectID", "samp.date", "samp.date.num")]


# Check missingness
for (i in 1:15) print(c(i,table( is.na(cort.dat[,i]) |  cort.dat[,i]=='' ))) 


#check for consistency between booklet and MEMs clock time

#Spaghetti as well as means plot of cortisol and dhea (1)overall, (2)within each DAYNUMBER check for location and dispersion among the different days and overall.

create a new id variable that concatenates subjid with daynumb and select mean, min, max as summary measure
Also, separate spaghetti plot by day 1, day2, day3, and overall


