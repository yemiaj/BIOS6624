frh.dat <- read.csv("./Project3/DataRaw/frmgham2.csv")

#Subjects with prevalent stroke at baseline
frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]

#These are 32 unique subjects
table(duplicated(frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]))
length(duplicated(frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]))

#These 32 unique subjects represents 60 observations
dim(frh.dat[frh.dat$RANDID %in% frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"],])

#Subset original data and remove these 32 with prevalent stroke
frh.dat2 <- frh.dat[!(frh.dat$RANDID %in% frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]), ]

#Select needed and potentially useful covariates 
frh.dat3 <- frh.dat2[,c("RANDID", "PERIOD", "SEX", 
                        "AGE", "DIABETES", "SYSBP", 
                        "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI",
                        "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]

#Assess whether TIMEDTH and TIMESTRK is indeed unique using a long format
#Sort by subject and period first
frh.dat3 <- frh.dat3[order(frh.dat3$RANDID, frh.dat3$PERIOD), ]
frh.dat3$strk.diff <- c(NA, diff(frh.dat3$TIMESTRK)) #Set the first value to NA so diff() works

#Set the first period to NA per patient so strk.diff variable can be reviewed
#Before that, make sure that there are no duplicates by subject and period
table(duplicated(paste0(frh.dat3$RANDID, frh.dat3$PERIOD)))
#No duplicates by subject & period
frh.dat3$strk.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$strk.diff)
table(frh.dat3$strk.diff, exclude=NULL)
#TIMESTRK is unique! Because the diff is 0 and the 4402 NAs represent the number of unique subjects whose diff was set to NA at period 1
sum(table(unique(frh.dat2$RANDID))) #Unique subjects

#Do the same for TIMEDTH
frh.dat3$dth.diff <- c(NA, diff(frh.dat3$TIMEDTH)) #Set the first value to NA so diff() works
frh.dat3$dth.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$dth.diff)
table(frh.dat3$dth.diff, exclude=NULL)
#TIMEDTH is also unique! 

#This uniqueness makes life and coding (below) easy!

#Select needed and potentially useful covariates 
frh.dat4 <- frh.dat2[,c("RANDID", "PERIOD", "SEX", 
                        "AGE", "DIABETES", "SYSBP", 
                        "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI",
                        "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]

#Sort by subject and period
frh.dat4 <- frh.dat4[order(frh.dat4$RANDID, frh.dat4$PERIOD), ]


#Transform data from long to wide
frh.flat <- reshape(frh.dat4[, c("RANDID", "PERIOD", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP", "SEX")], 
                    idvar = "RANDID", 
                    timevar = "PERIOD",
                    v.names = c("STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP", "SEX"),
                    direction = "wide")

table(frh.flat$SEX.1, frh.flat$SEX.2, frh.flat$SEX.3) #Sex is the same across all PERIOD, so select 1

frh.flat <- frh.flat[,c("RANDID", 
                        "STROKE.1", "STROKE.2","STROKE.3", 
                        "TIMESTRK.1", "TIMESTRK.2", "TIMESTRK.3",
                        "DEATH.1", "DEATH.2", "DEATH.3", 
                        "TIMEDTH.1", "TIMEDTH.2", "TIMEDTH.3", 
                        "DIABETES.1", "DIABETES.2", "DIABETES.3", 
                        "SYSBP.1", "SYSBP.2", "SYSBP.3", 
                        "SEX.1")]

#Coding stroke and time to stroke

#Stroke and death status is not missing at any period, so the logic below is okay.
  #However, it will be missing in a wide format if a patient does not have rows of data for any of the 3 PERIODs
table(frh.dat4$STROKE, exclude=NULL)
table(frh.dat4$DEATH, exclude=NULL)

frh.flat$strk1 <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE) #Select stroke event
frh.flat$dth1 <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE) #Select death event

#This logic may not be really necessary because TIMESTRK is unique in the long data format (as shown above)
frh.flat$strk.tim1 <- ifelse(frh.flat$strk1==1,
                             pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE), #if STROKE==1 then pick earliest TIMESTRK,
                             pmax(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)) #otherwise pick oldest TIMESTRK

frh.flat$dth.tim1 <- ifelse(frh.flat$dth1==1,
                            pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE), #if DEATH==1 then pick earliest TIMEDTH, 
                            pmax(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)) #otherwise pick oldest TIMEDTH

table(frh.flat$strk1)
table(frh.flat$dth1)
table(frh.flat$strk1, frh.flat$dth1) #131 strokes without death; 252 death and stroke; 1273 deaths without stroke

#7 instances where death time is lower than stroke time
frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]
strk.7.IDs <- frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]$RANDID

#252 cases with death and stroke events
View(frh.flat[(frh.flat$strk1 == frh.flat$dth1) & frh.flat$strk1==1,])
#In none of these is stroke time beyond death time
View(frh.flat[(frh.flat$strk1 == frh.flat$dth1) & frh.flat$strk1==1 & (frh.flat$strk.tim1 > frh.flat$dth.tim1),])

#For the 7 cases with stroke time beyond death time, hardcode time back to death time
frh.flat$strk1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- 0 #Censor these for stroke event (they were already censored, just making sure)
frh.flat$strk.tim1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]$dth.tim1
frh.flat[frh.flat$RANDID %in% strk.7.IDs, ] #observe to ensure coding is as expected

#Number of events before and after recoding to under 10 years
survfit(Surv(strk.tim1, strk1)~1,frh.flat)
plot(survfit(Surv(strk.tim1, strk1)~1,frh.flat))
plot(sex.srv <- survfit(Surv(strk.tim1, strk1)~SEX.1,frh.flat), col=1:2, lwd=3, mark.time = FALSE)
survdiff(Surv(strk.tim1, strk1)~SEX.1,frh.flat) #p=0.03
cox.zph(coxph(Surv(strk.tim1, strk1)~SEX.1,frh.flat)) #PH okay based on Schoenfeld, how about log-log?


#Now code first 10 years of follow-up
frh.flat$stroke.time <- ifelse(frh.flat$strk.tim1 > 365.25*10, 365.25*10, frh.flat$strk.tim1) #Note the multiplier in the methods
frh.flat$stroke.event <- ifelse(frh.flat$strk.tim1 > 365.25*10, 0, frh.flat$strk1) #Note the hardcoding in the methods too

table(frh.flat$strk.tim1>365.25*10, frh.flat$strk1) #the number of stroke events recoded back down
table(frh.flat$stroke.event,frh.flat$strk1)

plot(sex.srv2 <- survfit(Surv(stroke.time, stroke.event)~SEX.1,frh.flat), col=1:2, lwd=5, lty=2, mark.time = FALSE)

summary(sex.srv2, times=(1:10)*365.25)
#Include number of events in Table 1












#Rethink logic and finalize data cleaning
View(frh.flat[(frh.flat$TIMESTRK.1 > frh.flat$TIMEDTH.1) & !is.na(frh.flat$TIMEDTH.1),])
View(frh.flat[(frh.flat$TIMESTRK.2 > frh.flat$TIMEDTH.2) & !is.na(frh.flat$TIMESTRK.2),])
View(frh.flat[(frh.flat$TIMESTRK.3 > frh.flat$TIMEDTH.3) & !is.na(frh.flat$TIMEDTH.3),])
View(frh.flat[(frh.flat$TIMESTRK.1 > frh.flat$TIMEDTH.1) | (frh.flat$TIMESTRK.2 > frh.flat$TIMEDTH.2) | (frh.flat$TIMESTRK.3 > frh.flat$TIMEDTH.3),])

#Naive coding, pending detailed review of coding logic
#df$max_val <- apply(df[, c("var1", "var2", "var3")], 1, max, na.rm = TRUE)
frh.flat$stroke.yes <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE)
frh.flat$stroke.time <- pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)
frh.flat$dead.yes <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE)
frh.flat$dead.time <- pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)

#died before stroke (may censor for stroke at the date of death) #Note this during data description
frh.flat[frh.flat$stroke.time>frh.flat$dead.time,]

#stroke and died
View(frh.flat[(frh.flat$stroke.yes==frh.flat$dead.yes) & frh.flat$stroke.yes==1,])

#Still with prevalent stroke, but not diagnosed at baseline
table(frh.dat2$PREVSTRK)
frh.dat2[frh.dat2$PREVSTRK==1,"RANDID"]
length(unique(frh.dat2[frh.dat2$PREVSTRK==1,"RANDID"])) #76 unique

#Assess uniqueness of TIMEDTH and TIMESTRK in the long format
#Sort by subject and period first
frh.dat3 <- frh.dat3[order(frh.dat3$RANDID, frh.dat3$PERIOD), ]
frh.dat3$strk.diff <- c(NA, diff(frh.dat3$TIMESTRK)) #Set the first value to NA so diff() works

#Set the first period to NA per patient so strk.diff variable can be reviewed
#Before that, make sure that there are no duplicates by subject and period
table(duplicated(paste0(frh.dat3$RANDID, frh.dat3$PERIOD)))
#No duplicates by subject & period
frh.dat3$strk.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$strk.diff)
table(frh.dat3$strk.diff, exclude=NULL)
#TIMESTRK is unique!

#Do the same for TIMEDTH
frh.dat3$dth.diff <- c(NA, diff(frh.dat3$TIMEDTH)) #Set the first value to NA so diff() works
frh.dat3$dth.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$dth.diff)
table(frh.dat3$dth.diff, exclude=NULL)
#TIMEDTH is also unique! The number of NAs is the same as the number of subjects in the flat data (4402, show how I arrived at this)

#This makes life and coding (below) easy!

#Transform data from long to wide
frh.flat <- reshape(frh.dat3[, c("RANDID", "PERIOD", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP", "SEX")], 
                    idvar = "RANDID", 
                    timevar = "PERIOD",
                    v.names = c("STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP", "SEX"),
                    direction = "wide")

table(frh.flat$SEX.1, frh.flat$SEX.2, frh.flat$SEX.3) #Sex is the same across all PERIOD, so select 1

frh.flat <- frh.flat[,c("RANDID", 
                        "STROKE.1", "STROKE.2","STROKE.3", 
                        "TIMESTRK.1", "TIMESTRK.2", "TIMESTRK.3",
                        "DEATH.1", "DEATH.2", "DEATH.3", 
                        "TIMEDTH.1", "TIMEDTH.2", "TIMEDTH.3", 
                        "DIABETES.1", "DIABETES.2", "DIABETES.3", 
                        "SYSBP.1", "SYSBP.2", "SYSBP.3", 
                        "SEX.1")]

View(frh.flat[(frh.flat$TIMESTRK.1 > frh.flat$TIMEDTH.1) & !is.na(frh.flat$TIMEDTH.1),])
View(frh.flat[(frh.flat$TIMESTRK.2 > frh.flat$TIMEDTH.2) & !is.na(frh.flat$TIMESTRK.2),])
View(frh.flat[(frh.flat$TIMESTRK.3 > frh.flat$TIMEDTH.3) & !is.na(frh.flat$TIMEDTH.3),])

View(frh.flat[(frh.flat$TIMESTRK.1 > frh.flat$TIMEDTH.1) | (frh.flat$TIMESTRK.2 > frh.flat$TIMEDTH.2) | (frh.flat$TIMESTRK.3 > frh.flat$TIMEDTH.3),])

#Naive coding, pending detailed review of coding logic
#df$max_val <- apply(df[, c("var1", "var2", "var3")], 1, max, na.rm = TRUE)
frh.flat$stroke.yes <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE)

frh.flat$stroke.time <- pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)

frh.flat$dead.yes <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE)
frh.flat$dead.time <- pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)

#died before stroke (may censor for stroke at the date of death) #Note this during data description
frh.flat[frh.flat$stroke.time>frh.flat$dead.time,]

#stroke and died
View(frh.flat[(frh.flat$stroke.yes==frh.flat$dead.yes) & frh.flat$stroke.yes==1,])

#Rethink logic and finalize data cleaning

#Naive coding, pending detailed review of coding logic
#df$max_val <- apply(df[, c("var1", "var2", "var3")], 1, max, na.rm = TRUE)
frh.flat$stroke.yes <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE)
frh.flat$stroke.time <- pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)

frh.flat$dead.yes <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE)
frh.flat$dead.time <- pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)

#died before stroke (may censor for stroke at the date of death) #Note this during data description
frh.flat[frh.flat$stroke.time>frh.flat$dead.time,]

#stroke and died
View(frh.flat[(frh.flat$stroke.yes==frh.flat$dead.yes) & frh.flat$stroke.yes==1,])

#KM Plot
plot(survfit(Surv(stroke.time/365.25, stroke.yes)~1, frh.flat), conf.int=FALSE, lwd=3, mark.time = F, xlab="Time (years)", ylab="Stroke Probability", main="Framingham Study: KM Plot of Time to Stroke", axes=F)
axis(1, at=seq(0,24,1))
axis(2, at=seq(0,1,.1))
abline(v=10)

plot(survfit(Surv(stroke.time/365.25, stroke.yes)~1, frh.flat), conf.int=FALSE, lwd=3, mark.time = F, xlab="Time (years)", ylab="Stroke Probability", main="Framingham Study: KM Plot of Time to Stroke", xlim=c(0,10))
axis(1, at=seq(0,24,1))
axis(2, at=seq(0,1,.1))

frh.flat$stroke.time2 <- ifelse(frh.flat$stroke.time>365.25*10,365.25*10,frh.flat$stroke.time)
plot(survfit(Surv(stroke.time2/365.25, stroke.yes)~1, frh.flat), conf.int=FALSE, lwd=3, mark.time = F, xlab="Time (years)", ylab="Stroke Probability", main="Framingham Study: KM Plot of Time to Stroke \truncated at 10 years")

plot(survfit(Surv(stroke.time/365.25, stroke.yes)~1, frh.flat[frh.flat$SEX.1==1, ]), conf.int=FALSE, lwd=3, mark.time = F, xlab="Time (years)", ylab="Stroke Probability", main="Framingham Study: KM Plot of Time to Stroke")
lines(survfit(Surv(stroke.time/365.25, stroke.yes)~1, frh.flat[frh.flat$SEX.1==2, ]), conf.int=FALSE, lwd=3, mark.time = F, col=2, lty=2)

#Coding time to stroke, calling death event too
#This is not quite right, only select stroke events, not stroke and death
#Then put the 10 year cap
frh.flat$stroke2 <- ifelse(frh.flat$stroke.yes==1 | frh.flat$dead.yes==1, 1, 0)
frh.flat$stroke.time2 <- ifelse(frh.flat$stroke2==1, pmin(frh.flat$stroke.time, frh.flat$dead.time), pmax(frh.flat$stroke.time, frh.flat$dead.time))

frh.flat$stroke.time2 <- NULL
frh.flat[frh.flat$stroke2==1]$stroke.time2 <- pmin(frh.flat$stroke.time, frh.flat$dead.time)
frh.flat[frh.flat$stroke2==0] <- pmax(frh.flat$stroke.time, frh.flat$dead.time)

frh.flat$stroke.yes <- c(STROKE.1, STROKE.2, STROKE.3)
  with(frh.flat, max(, na.rm = TRUE))

#Missing for any of timestrk variables 
View(frh.flat[is.na(frh.flat$TIMESTRK.1) | is.na(frh.flat$TIMESTRK.2) | is.na(frh.flat$TIMESTRK.3), ])
View(frh.int <- frh.flat[!(is.na(frh.flat$TIMESTRK.1) | is.na(frh.flat$TIMESTRK.2) | is.na(frh.flat$TIMESTRK.3)), ])

#Does the development of other outcomes increases chances of stroke?
#That is, is stroke independent of other outcomes?
#There's not much I can do about that in the context of this project, soooo...abandon complicated analysis
getwd()