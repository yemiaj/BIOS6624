frh.dat <- read.csv("./Project3/DataRaw/frmgham2.csv")

#Subjects with prevalent stroke at baseline
frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]
#These are 32 unique subjects
table(duplicated(frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]))
#These 32 unique subjects represents 60 observations
dim(frh.dat[frh.dat$RANDID %in% frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"],])

#Subset original data and remove these 32 with prevalent stroke
frh.dat2 <- frh.dat[!(frh.dat$RANDID %in% frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]), ]


#Still with prevalent stroke, but not diagnosed at baseline
table(frh.dat2$PREVSTRK)
frh.dat2[frh.dat2$PREVSTRK==1,"RANDID"]
length(unique(frh.dat2[frh.dat2$PREVSTRK==1,"RANDID"])) #76

#Select baseline 
frh.dat3 <- frh.dat2[,c("RANDID", "PERIOD", "SEX", "TOTCHOL", "AGE", "SYSBP", "DIABP", "CURSMOKE", "CIGPDAY", "BMI", 
                        "DIABETES", "BPMEDS", "HEARTRTE", "GLUCOSE", "educ", "PREVSTRK", "TIME", "HDLC", "LDLC", 
                        "HYPERTEN", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]

frh.flat <- reshape(frh.dat3[, c("RANDID", "PERIOD", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")], 
                    idvar = "RANDID", 
                    timevar = "PERIOD",
                    v.names = c("STROKE", "TIMESTRK", "DEATH", "TIMEDTH"),
                    direction = "wide")

frh.flat <- frh.flat[,c("RANDID", "STROKE.1", "STROKE.2","STROKE.3", "TIMESTRK.1", "TIMESTRK.2", "TIMESTRK.3",
                        "DEATH.1", "DEATH.2", "DEATH.3", "TIMEDTH.1", "TIMEDTH.2", "TIMEDTH.3")]

#Naive coding, pending detailed review of coding logic
frh.flat$stroke.yes <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE)
frh.flat$stroke.time <- pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)

frh.flat$dead.yes <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE)
frh.flat$dead.time <- pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)

#died before stroke (may censor for stroke at the date of death) #Note this during data description
frh.flat[frh.flat$stroke.time>frh.flat$dead.time,]

#stroke and died
View(frh.flat[(frh.flat$stroke.yes==frh.flat$dead.yes) & frh.flat$stroke.yes==1,])


plot(survfit(Surv(stroke.time,stroke.yes)~1, frh.flat))



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

)
   

#Missing for any of timestrk variables 
View(frh.flat[is.na(frh.flat$TIMESTRK.1) | is.na(frh.flat$TIMESTRK.2) | is.na(frh.flat$TIMESTRK.3), ])
View(frh.int <- frh.flat[!(is.na(frh.flat$TIMESTRK.1) | is.na(frh.flat$TIMESTRK.2) | is.na(frh.flat$TIMESTRK.3)), ])


 
#Does the development of other outcomes increases chances of stroke?
#That is, is stroke independent of other outcomes?
#There's not much I can do about that in the context of this project, soooo...abandon complicated analysis
getwd()




