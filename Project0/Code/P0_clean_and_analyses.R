#######################

#Codes related to the cleaning and preliminary analysis of Project 0: COrtisol and DHEA

######################

#Import .csv file for Project 0
cort.dat <- read.csv('./Project0/DataRaw/Project0_Clean_v2.csv') 

str(cort.dat) #See variable types and values

#SubjectID==3029 has duplicated DAYNUM. Based on Collection.Date it appears there's a Day 1 and a Day 4. Not sure how to handle this yet, delete or hardcode?
cort.dat[duplicated(paste0(cort.dat$SubjectID, cort.dat$Collection.Sample, cort.dat$DAYNUMB)), ]
cort.dat[cort.dat$SubjectID==3029,]

#Hardcode seems more appropriate than deleting, although patient submitted samples spanning 5 days (10/7 through 10/12).
#Unfortunately, no meeting (office hours) with investigator before submission deadline
#Delete observation from final dataset as sensitivity analysis
cort.dat$DAYNUMB[cort.dat$SubjectID==3029 & cort.dat$Collection.Date=="10/7/2018"] <- 1
cort.dat$DAYNUMB[cort.dat$SubjectID==3029 & cort.dat$Collection.Date=="10/10/2018"] <- 2
cort.dat$DAYNUMB[cort.dat$SubjectID==3029 & cort.dat$Collection.Date=="10/12/2018"] <- 3

#Sort data by ID, sample collection time point, and days of sample collection
cort.dat <- cort.dat[order(cort.dat$SubjectID, cort.dat$DAYNUMB, cort.dat$Collection.Sample),]


#Convert character date to R date format and numeric variable
cort.dat$samp.date <- as.Date(cort.dat$Collection.Date, format = '%m/%d/%Y')
cort.dat$samp.date.num <- as.numeric(cort.dat$samp.date)

#Create new more intuitive variable names
cort.dat$samp.timepoint <- cort.dat$Collection.Sample
cort.dat$samp.day123 <- cort.dat$DAYNUMB
cort.dat$wake.time.per.diary <- cort.dat$Sleep.Diary.reported.wake.time
cort.dat$wake.time.in.mins <- as.difftime(cort.dat$Sleep.Diary.reported.wake.time, format="%H:%M", units = 'mins')



cort.dat$Booket..Clock.Time

cort.dat$booklet.sleeptime.diff <- (as.difftime(cort.dat$Booket..Clock.Time, format="%H:%M", units = 'mins') - as.difftime(cort.dat$Sleep.Diary.reported.wake.time, format="%H:%M", units = 'mins'))




cort.dat$samp.col.time.booklet <- cort.dat$Booket..Clock.Time
cort.dat$samp.col.time.electronic <- cort.dat$MEMs..Clock.Time
cort.dat$time.since.waking.booklet <- cort.dat$Booklet..Sample.interval.Decimal.Time..mins.
cort.dat$time.since.waking.electronic <- cort.dat$MEMs..Sample.interval.Decimal.Time..mins.


#Select and order variables
cort.dat <- cort.dat[, c("SubjectID", "samp.date", "samp.date.num", "samp.timepoint", "samp.day123", "wake.time.per.diary", "wake.time.in.mins")] 
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




