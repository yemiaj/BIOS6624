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

#Create new, and/or more intuitive variable names
cort.dat$samp.timepoint <- cort.dat$Collection.Sample
cort.dat$samp.day123 <- cort.dat$DAYNUMB
cort.dat$wake.time.per.diary <- cort.dat$Sleep.Diary.reported.wake.time
cort.dat$wake.time.diary.in.mins <- as.numeric(as.difftime(cort.dat$Sleep.Diary.reported.wake.time, format="%H:%M", units = 'mins'))
cort.dat$time.since.waking.booklet <- cort.dat$Booklet..Sample.interval.Decimal.Time..mins.
cort.dat$time.since.waking.electronic <- cort.dat$MEMs..Sample.interval.Decimal.Time..mins.

cort.dat$cort.ugdl <- cort.dat$Cortisol..ug.dl.
cort.dat$cort.nmoll <- cort.dat$Cortisol..nmol.L.
cort.dat$dhea.pgdl <- cort.dat$DHEA..pg.dl.
cort.dat$dhea.nmoll <- cort.dat$DHEA..nmol.L.

#Manipulate booklet and electronic (MEMs) clock time so that interval time from waking can be collected
cort.dat$book.time <- cort.dat$Booket..Clock.Time
cort.dat$mems.time <- cort.dat$MEMs..Clock.Time
cort.dat$book.time.mins <- as.difftime(cort.dat$Booket..Clock.Time, format="%H:%M", units = 'mins')
cort.dat$mems.time.mins <- as.difftime(cort.dat$MEMs..Clock.Time, format="%H:%M", units = 'mins')
  
#Select the booklet and MEMs time for the first collection timepoint for each day and do a many to one marge below
wake.times <- cort.dat[cort.dat$Collection.Sample==1, c("SubjectID", "DAYNUMB", "book.time.mins", "mems.time.mins")]
names(wake.times)[3:4] <- c("book.wake.time.mins", "mems.wake.time.mins") 

#select and merge baseline sample collection date to see if samples for this study were collected on consecutive days for all patients
samp.times <- cort.dat[cort.dat$Collection.Sample==1 & cort.dat$DAYNUMB==1, c("SubjectID", "samp.date")]
names(samp.times)[2] <- "samp.date.baseline"


cort.dat2 <- merge(cort.dat, wake.times, by=c("SubjectID", "DAYNUMB"))
cort.dat2 <- merge(cort.dat2, samp.times, by="SubjectID")


cort.dat2$book.interval.mins <- as.numeric(cort.dat2$book.time.mins - cort.dat2$book.wake.time.mins)
cort.dat2$mems.interval.mins <- as.numeric(cort.dat2$mems.time.mins - cort.dat2$mems.wake.time.mins)
cort.dat2$samp.interval <- cort.dat2$samp.date - cort.dat2$samp.date.baseline


#Select and order variables
cort.dat2 <- cort.dat2[, c("SubjectID", "samp.timepoint", "samp.day123", "samp.interval", "samp.date", "wake.time.per.diary", 
                           "wake.time.diary.in.mins", "book.time", "mems.time", "book.time.mins", "mems.time.mins", "time.since.waking.booklet", 
                           "time.since.waking.electronic", "cort.ugdl", "cort.nmoll", "dhea.pgdl", "dhea.nmoll", "book.wake.time.mins", 
                           "mems.wake.time.mins", "book.interval.mins", "mems.interval.mins")] 

str(cort.dat2) #See variable types and values


#continue: merge into cort.dat and compare book.interval.mins vs time.since.waking.booklet and mems.interval.mins vs time.since.waking.electronic
#do this for missing and non-missing values
cort.dat2[is.na(cort.dat2$time.since.waking.booklet) | is.na(cort.dat2$book.interval.mins),]

#Subject 3049 had non-missing values (==0) for "cort.dat$Booklet..Sample.interval.Decimal.Time..mins." when "cort.dat$Collection.Sample" ==1 but all values were
  # NAs for my calculated book.interval.mins. This makes sense because interval time is 0 at waking. I have hardcoded these such that this case is not dropped 
  # from analysis since the intention is to use booklet interval time as main predictor

#Hardcoding SubjectID==3049, see explanation above
cort.dat2$book.interval.mins[cort.dat2$SubjectID==3049 & cort.dat2$samp.timepoint==1] <- cort.dat2[cort.dat2$SubjectID==3049 & cort.dat2$samp.timepoint==1, "time.since.waking.booklet"]

#Doing the same assessment for electronic time interval
cort.dat2[is.na(cort.dat2$time.since.waking.electronic) | is.na(cort.dat2$mems.interval.mins),]
#There was no instance of "cort.dat2$time.since.waking.electronic" having a non-NA value while the value I computed had missing values!


#When neither the investigator provided time.since.waking.booklet nor my calculated book.interval.mins is NA, then the investigator provided and my calculated interval times are the same!
cort.dat2[!(is.na(cort.dat2$time.since.waking.booklet) | is.na(cort.dat2$book.interval.mins)) & (cort.dat2$time.since.waking.booklet != cort.dat2$book.interval.mins), ]

#Likewise, when neither the investigator provided time.since.waking.electronic nor my calculated mems.interval.mins is NA, then the investigator provided and my calculated interval times are the same!
cort.dat2[!(is.na(cort.dat2$time.since.waking.electronic) | is.na(cort.dat2$mems.interval.mins)) & (cort.dat2$time.since.waking.electronic != cort.dat2$mems.interval.mins), ]


#The preidictor of interest has 35 observations with missing values, 23 of these can be salvaged if we substitute mems.interval.mins for book.interval.mins
cort.dat2[is.na(cort.dat2$book.interval.mins),]
cort.dat2[is.na(cort.dat2$book.interval.mins) & !is.na(cort.dat2$mems.interval.mins),]

#Define a new book.interval.mins variable that replaces missing values with values from mems.interval.mins. This will be used for sensitivity analysis
cort.dat2$book.interval.mins.v2 <- cort.dat2$book.interval.mins
cort.dat2$book.interval.mins.v2[is.na(cort.dat2$book.interval.mins.v2)] <- cort.dat2[is.na(cort.dat2$book.interval.mins.v2), "mems.interval.mins"]

#Create a new ID/cluster variable for subjectID and day number
cort.dat2$SubjectID.day <- paste0(cort.dat2$SubjectID, "-", cort.dat2$samp.day123)
cort.dat2 <- cort.dat2[,c(1,23,2:22)]

#Per investigator, cortisol level >80 is likely an artifact
#Only 1 subject (3025) affected and set to NA
cort.dat2$cort.nmoll[!is.na(cort.dat2$cort.nmoll) & cort.dat2$cort.nmoll>80] <- NA

#Export final data
write.csv(cort.dat2, "./Project0/DataProcessed/Project0_data.csv")


#Next time around
#Select both units of cortisol and DHEA for keeps sake
#Check variable types and format accordingly
#Spaghetti as well as means plot of cortisol and dhea (1)overall, (2)within each DAYNUMBER check for location and dispersion among the different days and overall.
#create a new id variable that concatenates subjid with daynumb and select mean, min, max as summary measure when creating plots
#Also, separate spaghetti plot by day 1, day2, day3, and overall