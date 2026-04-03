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
frh.dat3 <- frh.dat2[,c("RANDID", "SEX", "TOTCHOL", "AGE", "SYSBP", "DIABP", "CURSMOKE", "CIGPDAY", "BMI", "DIABETES",
                        "BPMEDS", "HEARTRTE", "GLUCOSE", "educ", "PREVSTRK", "TIME", "PERIOD", "HDLC", "LDLC", "HYPERTEN",
                        "DEATH", "STROKE", "TIMESTRK", "TIMEDTH")]



#Does the development of other outcomes increases chances of stroke?
#That is, is stroke independent of other outcomes?
#There's not much I can do about that in the context of this project, soooo...abandon complicated analysis
getwd()




