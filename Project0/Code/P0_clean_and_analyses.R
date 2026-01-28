#######################

#Codes related to the cleaning and preliminary analysis of Project 0: COrtisol and DHEA

######################

#Import .csv file for Project 0
cort.dat <- read.csv('./Project0/DataRaw/Project0_Clean_v2.csv') 

str(cort.dat) #See variable types and values

#Convert character date to R date format and numeric variable
cort.dat$samp.date <- as.Date(cort.dat$Collection.Date, format = '%m/%d/%Y')
cort.dat$samp.date.num <- as.numeric(cort.dat$samp.date)

#Create new more intuitive variable names
cort.dat$samp.timepoint <- cort.dat$Collection.Sample
cort.dat$samp.day123 <- cort.dat$DAYNUMB
cort.dat$wake.time.per.diary <- cort.dat$Sleep.Diary.reported.wake.time

cort.dat$samp.col.time.booklet <- cort.dat$Booket..Clock.Time
cort.dat$samp.col.time.electronic <- cort.dat$MEMs..Clock.Time
cort.dat$time.since.waking.booklet <- cort.dat$Booklet..Sample.interval.Decimal.Time..mins.
cort.dat$time.since.waking.electronic <- cort.dat$MEMs..Sample.interval.Decimal.Time..mins.



cort.dat <- cort.dat[, c("SubjectID", "samp.date", "samp.date.num", "samp.timepoint", "samp.day123", "wake.time_per.diary")] 
#By selecting these variable I only effectively dropped Booklet & MEMs sample interval variables. I don't think (at this time that they're needed further)

str(cort.dat) #See variable types and values

#Next time around
#Select both units of cortisol and DHEA for keeps sake
#Check variable types and format accordingly
#Spaghetti as well as means plot of cortisol and dhea (1)overall, (2)within each DAYNUMBER check for location and dispersion among the different days and overall.
#create a new id variable that concatenates subjid with daynumb and select mean, min, max as summary measure when creating plots
#Also, separate spaghetti plot by day 1, day2, day3, and overall


# Check missingness
for (i in 1:15) print(c(i,table( is.na(cort.dat[,i]) |  cort.dat[,i]=='' ))) 


#check for consistency between booklet and MEMs clock time




