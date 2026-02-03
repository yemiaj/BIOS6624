#######################

#Codes related to the cleaning and preliminary analysis of Project 0: COrtisol and DHEA

######################

#Import .csv file for Project 0
cort.dat <- read.csv('./Project0/DataRaw/Project0_Clean_v2.csv') 

str(cort.dat) #See variable types and values

#SubjectID==3029 has duplicated DAYNUM. Based on Collection.Date it appears there's a Day 1 and a Day 4.
#Not sure how to handle this yet, delete or hardcode?
#Investigator said to hardcode
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

cort.dat$book.time.hrs <- as.difftime(cort.dat$Booket..Clock.Time, format="%H:%M", units = 'hours')
cort.dat$mems.time.hrs <- as.difftime(cort.dat$MEMs..Clock.Time, format="%H:%M", units = 'hours')

#Select the booklet and MEMs time for the first collection timepoint for each day and do a many to one marge below
wake.times <- cort.dat[cort.dat$Collection.Sample==1, c("SubjectID", "DAYNUMB", "book.time.mins", "mems.time.mins")]
names(wake.times)[3:4] <- c("book.wake.time.mins", "mems.wake.time.mins") 

#select and merge baseline sample collection date to see if samples for this study were collected on consecutive days for all patients
samp.times <- cort.dat[cort.dat$Collection.Sample==1 & cort.dat$DAYNUMB==1, c("SubjectID", "samp.date")]
names(samp.times)[2] <- "samp.date.baseline"

#Merge wake.times and samp.times into the main cort.dat and create a new main data cort.dat2
cort.dat2 <- merge(cort.dat, wake.times, by=c("SubjectID", "DAYNUMB"))
cort.dat2 <- merge(cort.dat2, samp.times, by="SubjectID")

#Calculate time from waking to sample collection for booklet and MEMs. These are the equivalent of Booklet..Sample.interval.Decimal.Time..mins. and MEMs..Sample.interval.Decimal.Time..mins.
cort.dat2$book.interval.mins <- as.numeric(cort.dat2$book.time.mins - cort.dat2$book.wake.time.mins)
cort.dat2$mems.interval.mins <- as.numeric(cort.dat2$mems.time.mins - cort.dat2$mems.wake.time.mins)

#Variable to see if subject collected samples beyond the 3 consecutive days indicated
cort.dat2$samp.interval <- cort.dat2$samp.date - cort.dat2$samp.date.baseline

#SubjectID 3029 is only subject for which samples were not collected on consecutive days but at 0, 3, and 5 days for the first, second, and third samples respectively
cort.dat2[cort.dat2$samp.interval>2,] 

#Select and order variables
cort.dat2 <- cort.dat2[, c("SubjectID", "samp.timepoint", "samp.day123", "samp.interval", "samp.date", "samp.date.baseline", "wake.time.per.diary", 
                           "wake.time.diary.in.mins", "book.time", "mems.time", "book.time.mins", "mems.time.mins", "book.time.hrs", "mems.time.hrs", "time.since.waking.booklet", 
                           "time.since.waking.electronic", "cort.ugdl", "cort.nmoll", "dhea.pgdl", "dhea.nmoll", "book.wake.time.mins", 
                           "mems.wake.time.mins", "book.interval.mins", "mems.interval.mins")] 

str(cort.dat2) #See variable types and values


#Compare book.interval.mins vs time.since.waking.booklet and mems.interval.mins vs time.since.waking.electronic
#do this for missing and non-missing values
cort.dat2[is.na(cort.dat2$time.since.waking.booklet) | is.na(cort.dat2$book.interval.mins),]

#Subject 3049 had non-missing values (==0) for "cort.dat$Booklet..Sample.interval.Decimal.Time..mins." when "cort.dat$Collection.Sample" ==1 but all values were
  # NAs for my calculated book.interval.mins. This makes sense because interval time is 0 at waking. I have hardcoded these such that this case is not dropped 
  # from analysis since the intention is to use booklet interval time as main predictor

#Hardcoding SubjectID==3049, see explanation above
cort.dat2$book.interval.mins[cort.dat2$SubjectID==3049 & cort.dat2$samp.timepoint==1] <- cort.dat2[cort.dat2$SubjectID==3049 & cort.dat2$samp.timepoint==1, "time.since.waking.booklet"]

#Doing the same comparison for electronic time interval (mems.interval.mins vs time.since.waking.electronic)
cort.dat2[is.na(cort.dat2$time.since.waking.electronic) | is.na(cort.dat2$mems.interval.mins),]
#There was no instance of "cort.dat2$time.since.waking.electronic" having a non-NA value while the value I computed had missing values!

#When neither the investigator provided time.since.waking.booklet nor my calculated book.interval.mins is NA, then the investigator provided and my calculated interval times are the same!
cort.dat2[!(is.na(cort.dat2$time.since.waking.booklet) | is.na(cort.dat2$book.interval.mins)) & (cort.dat2$time.since.waking.booklet != cort.dat2$book.interval.mins), ]

#Likewise, when neither the investigator provided time.since.waking.electronic nor my calculated mems.interval.mins is NA, then the investigator provided and my calculated interval times are the same!
cort.dat2[!(is.na(cort.dat2$time.since.waking.electronic) | is.na(cort.dat2$mems.interval.mins)) & (cort.dat2$time.since.waking.electronic != cort.dat2$mems.interval.mins), ]


#The predictor of key interest has 35 observations with missing values, 23 of these can be salvaged if we substitute mems.interval.mins for book.interval.mins
cort.dat2[is.na(cort.dat2$book.interval.mins),]
cort.dat2[is.na(cort.dat2$book.interval.mins) & !is.na(cort.dat2$mems.interval.mins),]

#Define a new book.interval.mins variable that replaces missing values with values from mems.interval.mins. This will be used for sensitivity analysis
cort.dat2$book.interval.mins.v2 <- cort.dat2$book.interval.mins
cort.dat2$book.interval.mins.v2[is.na(cort.dat2$book.interval.mins.v2)] <- cort.dat2[is.na(cort.dat2$book.interval.mins.v2), "mems.interval.mins"]

#Create a new ID/cluster variable for subjectID and day number
cort.dat2$SubjectID.day <- paste0(cort.dat2$SubjectID, "-", cort.dat2$samp.day123)
cort.dat2$SubjectID.time <- paste0(cort.dat2$SubjectID, "-", cort.dat2$samp.timepoint)
cort.dat2 <- cort.dat2[,c(1,26:27,2:25)]


#Per investigator, cortisol level >80 is likely an artifact
#Only 1 subject (3025) affected and set to NA
#Also, 3 unique subjects (6 observations) have DHEA level at the limit of detection (5.205), these should be set to NA
cort.dat2[!is.na(cort.dat2$cort.nmoll) & cort.dat2$cort.nmoll>80,]
cort.dat2[!is.na(cort.dat2$dhea.nmoll) & cort.dat2$dhea.nmoll>=5.205,]

cort.dat2$cort.nmoll[!is.na(cort.dat2$cort.nmoll) & cort.dat2$cort.nmoll>80] <- NA
cort.dat2$dhea.nmoll[!is.na(cort.dat2$dhea.nmoll) & cort.dat2$dhea.nmoll==5.205] <- NA


#Coding adherence
cort.dat2$adhere.cat <- ifelse(cort.dat2$samp.timepoint==2, as.numeric(cut(abs(cort.dat2$book.interval.mins - 30), breaks=c(-Inf, 7.5, 15, Inf))), #Microsoft CoPilot used for abs() & +/-Inf coding logic
                               ifelse(cort.dat2$samp.timepoint==4, as.numeric(cut(abs(cort.dat2$book.interval.mins - 600), breaks=c(-Inf, 7.5, 15, Inf))), NA)
                               )

#Taking log of cort & DHEA
cort.dat2$log.cort.nmoll <- log(cort.dat2$cort.nmoll)
cort.dat2$log.dhea.nmoll <- log(cort.dat2$dhea.nmoll)

#Export final data
write.csv(cort.dat2, "./Project0/DataProcessed/Project0_data.csv")


#Preliminary codes for analysis of objectives
#Final decisions: Box plot (with data point dots) of cort.nmoll & dhea.nmoll by samp.timepoint
  #Use whole data and data averaged collapsed by samp.timepoint (i.e., marginalize out samp.day123)
  #Create a 2x2 plot using boxplot(), row is full vs collapsed and column is cortisol vs DHEA

#Table one of columns of samp.timepoint and grand total (Use data that has been collapsed by timepoint/day123)
  #Rows include book.time.hrs, mems.time.hrs, book.interval.mins, mems.interval.mins, cort.nmoll, dhea.nmoll,  log cort.nmoll, log dhea.nmoll, 

#Data structure: Y_{ijk} for i = 1 to 31, j = 1 to 3, and k = 1 to 4
  #Indicate nesting in SubjectID (samp.day123 (samp.timepoint))
  #Fit a random intercept model and think about the residual error (mean 0 & variance sigma^2 I_{ni})
  #Should the random effect be cort.dat2$samp.timepoint or cort.dat2$samp.timepoint & cort.dat2$samp.day123 or the nesting be in the R side.
  #It should be nested in the random effect side. Check for significance using LRT/AIC/BIC
  #nlme::gls() is for marginal models (R-side)
  #lme4::lmer() models random effects G-side only, forces Ri = sigma^2 I_{ni}
  #nlme::lme() models both Ri and Gi

#Q1
mod0 <- lme4::lmer(book.interval.mins ~ mems.interval.mins + (1 | SubjectID), data=cort.dat2) #vs SubjectID.day and SubjectID.time
summary(mod0)
summary(mod0)$coefficients
#Plot of this data, as dots with regressionline and maybe confidence interval

#Residual analysis in R-25 L08b
mod.fitted <- fitted(mod0)
mod.resid <- resid(mod0)
plot(y=mod.resid, x=mod.fitted, col='red', lwd=2)

mod0.dat <- data.frame(fitvals = mod.fitted, resid = mod.resid)
mod0.dat$id <- rownames(mod0.dat)

qqnorm(mod.resid)
qqline(mod.resid, col = "red")

#fitted and predicted values R-25 L07
#lmer formula breakdown & G-matrix 25 L07

#cor(cort.dat2$book.interval.mins, cort.dat2$mems.interval.mins, use="complete.obs")
#cor(cort.dat2$time.since.waking.booklet, cort.dat2$time.since.waking.electronic, use="complete.obs")


#Q2
#gtsummary table
#95% CI using epitools or other package
#Consider including a plot
with(cort.dat2[cort.dat2$samp.timepoint %in% c(2,4), ], table(adhere.cat, samp.timepoint, exclude=NULL))
with(cort.dat2[cort.dat2$samp.timepoint %in% c(2,4), ], prop.table(table(adhere.cat, samp.timepoint), 2))

#Q3
#Cortisol
mod2a <- lme4::lmer(log.cort.nmoll ~ book.interval.mins + (1 | SubjectID), data=cort.dat2) #vs SubjectID.day and SubjectID.time
mod.fitted.2a <- fitted(mod2a)
mod.resid.2a <- resid(mod2a)
plot(y=mod.resid.2a, x=mod.fitted.2a, col='red', lwd=2)

#DHEA
mod2b <- lme4::lmer(log.dhea.nmoll ~ book.interval.mins + (1 | SubjectID), data=cort.dat2) #vs SubjectID.day and SubjectID.time
mod.fitted.2b <- fitted(mod2b)
mod.resid.2b <- resid(mod2b)
plot(y=mod.resid.2b, x=mod.fitted.2b, col='red', lwd=2)







#Rough/working codes
mod0 <- lme4::lmer(cort.nmoll ~ book.interval.mins + (1 | SubjectID), REML = FALSE, data=cort.dat2)

boxplot(cort.dat2$cort.nmoll ~ cort.dat2$samp.timepoint)
boxplot(cort.dat2$dhea.nmoll ~ cort.dat2$samp.timepoint)

fig1 <- ggplot(data = cort.dat2, aes(x = book.interval.mins, y = mems.interval.mins, group = SubjectID))
fig1+geom_point()

fig2a <- ggplot(data = cort.dat2, aes(x = book.interval.mins, y = log(cort.nmoll), group = SubjectID))
fig2a+geom_point()

fig2b <- ggplot(data = cort.dat2, aes(x = samp.timepoint, y = log(cort.nmoll), group = SubjectID))
fig2b+geom_point()

dat1 <- cort.dat2[!(is.na(cort.dat2$mems.interval.mins) | is.na(cort.dat2$book.interval.mins)),]
mod1 <- gls(book.interval.mins ~ mems.interval.mins, data=dat1, correlation = corCompSymm(form = ~ 1 | SubjectID))
summary(mod1)

gls(book.interval.mins ~ mems.interval.mins, data=cort.dat2, na.action = na.exclude, correlation = corCompSymm(form = ~ 1 | SubjectID))

summary(lm(mems.interval.mins~book.interval.mins, data=cort.dat2))
summary(lm(book.interval.mins~mems.interval.mins, data=cort.dat2))



#Next time around
#Select both units of cortisol and DHEA for keeps sake
#Check variable types and format accordingly
#Spaghetti as well as means plot of cortisol and dhea (1)overall, (2)within each DAYNUMBER check for location and dispersion among the different days and overall.
#create a new id variable that concatenates subjid with daynumb and select mean, min, max as summary measure when creating plots
#Also, separate spaghetti plot by day 1, day2, day3, and overall