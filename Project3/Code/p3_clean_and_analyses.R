#######################

#Codes related to the cleaning of data provided for Project 3: Framingham Heart Study: Risk Factors and Probability of Stroke by Sex

######################

#Import data provided on Canvas
frh.dat <- read.csv("./Project3/DataRaw/frmgham2.csv")

#Imported data has 11627 rows and 4434 unique observations
nrow(frh.dat); length(unique(frh.dat$RANDID))

#Identify subjects with prevalent stroke at baseline
exc.stroke <- frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"]
#There are no duplicate IDs
table(duplicated(exc.stroke))
#and there are 32 of them with prevalent stroke at baseline
length(unique(exc.stroke))
#These 32 unique subjects represents 60 observations
nrow(frh.dat[frh.dat$RANDID %in% exc.stroke,])
#The breakdown is as follows, 13 participants have 1 rows, 10 have 2 rows and 9 have 3 rows
table(table(frh.dat[frh.dat$RANDID %in% exc.stroke,1]))


#Subset original data and remove these 32 participants with prevalent stroke
frh.dat2 <- frh.dat[!(frh.dat$RANDID %in% exc.stroke), ]
#There are 4402 unique subjects after excluding the 32 
length(unique(frh.dat2$RANDID))


#Select needed covariates (per professor)
frh.dat3 <- frh.dat2[,c("RANDID", "PERIOD", "SEX",                         #These are identifying variables
                        "AGE", "DIABETES", "SYSBP",                        #These variables must be included in the model (per professor)
                        "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI", #These variables should be tested/assessed for inclusion in the model (per professor)
                        "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]         #Variables needed to ascertain the endpoint of time to stroke


#In a long data format, assess whether TIMEDTH and TIMESTRK is indeed unique per participant
#Sort by subject and period first
frh.dat3 <- frh.dat3[order(frh.dat3$RANDID, frh.dat3$PERIOD), ]
frh.dat3$strk.diff <- c(NA, diff(frh.dat3$TIMESTRK)) #Set the first value to NA so that diff() works as expected

#Set the first period to NA per patient so strk.diff variable can be reviewed
    #Before that, make sure that there are no duplicates by subject and period
table(duplicated(paste0(frh.dat3$RANDID, frh.dat3$PERIOD))) #No duplicates by subject & period

frh.dat3$strk.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$strk.diff)

table(frh.dat3$strk.diff, exclude=NULL)
#TIMESTRK is unique! Because the diff is 0 and the 4402 NAs represent the number of unique subjects whose diff was set to NA at period 1


#Do the same for TIMEDTH
frh.dat3$dth.diff <- c(NA, diff(frh.dat3$TIMEDTH))
frh.dat3$dth.diff <- ifelse(frh.dat3$PERIOD==1, NA, frh.dat3$dth.diff)
table(frh.dat3$dth.diff, exclude=NULL)
#TIMEDTH is also unique! 

#This uniqueness makes life and coding (below) easy!


#Select needed covariates
frh.dat4 <- frh.dat2[,c("RANDID", "PERIOD", "SEX", 
                        "AGE", "DIABETES", "SYSBP", 
                        "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI",
                        "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]

#Sort by subject and period
frh.dat4 <- frh.dat4[order(frh.dat4$RANDID, frh.dat4$PERIOD), ]

#Transform data from long to wide, including the 2 of the variables that we will check for time-varying changes
frh.flat <- reshape(frh.dat4[, c("RANDID", "PERIOD", "SEX", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP")], 
                    idvar = "RANDID", 
                    timevar = "PERIOD",
                    v.names = c("PERIOD", "SEX", "STROKE", "TIMESTRK", "DEATH", "TIMEDTH", "DIABETES", "SYSBP"),
                    direction = "wide")

table(frh.flat$SEX.1, frh.flat$SEX.2, frh.flat$SEX.3) #Sex is the same across all PERIOD, so select 1

#Organize variables
frh.flat <- frh.flat[,c("RANDID", 
                        "PERIOD.1","PERIOD.2","PERIOD.3",
                        "STROKE.1", "STROKE.2","STROKE.3", 
                        "TIMESTRK.1", "TIMESTRK.2", "TIMESTRK.3",
                        "DEATH.1", "DEATH.2", "DEATH.3", 
                        "TIMEDTH.1", "TIMEDTH.2", "TIMEDTH.3", 
                        "DIABETES.1", "DIABETES.2", "DIABETES.3", 
                        "SYSBP.1", "SYSBP.2", "SYSBP.3", 
                        "SEX.1")]


#Coding stroke event status and time to stroke

#Stroke and death status is not missing at any period, so the logic below is okay.
  #However, it will be missing in a wide format if a patient does not have rows of data for any of the 3 PERIODs
table(frh.dat4$STROKE, exclude=NULL)
table(frh.dat4$DEATH, exclude=NULL)

#Select maximum of STROKE and DETAH status at any period as indicator for stroke and death event respectively
frh.flat$strk1 <- pmax(frh.flat$STROKE.1, frh.flat$STROKE.2, frh.flat$STROKE.3, na.rm = TRUE) #Select stroke event
frh.flat$dth1 <- pmax(frh.flat$DEATH.1, frh.flat$DEATH.2, frh.flat$DEATH.3, na.rm = TRUE) #Select death event

#This logic may not be really necessary because TIMESTRK is unique in the long data format (as shown above)
frh.flat$strk.tim1 <- ifelse(frh.flat$strk1==1,
                             pmin(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE), #if STROKE==1 then pick earliest TIMESTRK,
                             pmax(frh.flat$TIMESTRK.1, frh.flat$TIMESTRK.2, frh.flat$TIMESTRK.3, na.rm = TRUE)) #otherwise pick oldest TIMESTRK

frh.flat$dth.tim1 <- ifelse(frh.flat$dth1==1,
                            pmin(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE), #if DEATH==1 then pick earliest TIMEDTH, 
                            pmax(frh.flat$TIMEDTH.1, frh.flat$TIMEDTH.2, frh.flat$TIMEDTH.3, na.rm = TRUE)) #otherwise pick oldest TIMEDTH

#Stroke and death events
table(frh.flat$strk1)
table(frh.flat$dth1)
table(frh.flat$strk1, frh.flat$dth1) #131 strokes without death; 252 death and stroke; 1273 deaths without stroke

#7 instances where death time is lower than stroke time
frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]
strk.7.IDs <- frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]$RANDID

#252 cases with death and stroke events
#View(frh.flat[(frh.flat$strk1 == frh.flat$dth1) & frh.flat$strk1==1,])
#In none of these is stroke time beyond death time
#View(frh.flat[(frh.flat$strk1 == frh.flat$dth1) & frh.flat$strk1==1 & (frh.flat$strk.tim1 > frh.flat$dth.tim1),])

#For the 7 cases with stroke time beyond death time, hardcode stroke time back to death time
frh.flat$strk1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- 0 #Censor these for stroke event (they were already censored, just making sure)
frh.flat$strk.tim1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]$dth.tim1 #Set stroke time back to death time
frh.flat[frh.flat$RANDID %in% strk.7.IDs, ] #observe to ensure coding is as expected

library(survival)
#Number of events before and after recoding to under 10 years
survfit(Surv(strk.tim1, strk1)~1,frh.flat) #383 events
plot(survfit(Surv(strk.tim1, strk1)~1,frh.flat))
plot(sex.srv <- survfit(Surv(strk.tim1, strk1)~SEX.1,frh.flat), col=1:2, lwd=3, mark.time = FALSE)
sex.srv
survdiff(Surv(strk.tim1, strk1)~SEX.1,frh.flat) #p=0.03
cox.zph(coxph(Surv(strk.tim1, strk1)~SEX.1,frh.flat)) #PH okay based on Schoenfeld, how about log-log?


#Limit data to the first 10 years of follow-up
frh.flat$stroke.time <- ifelse(frh.flat$strk.tim1 > 365.25*10, 365.25*10, frh.flat$strk.tim1)
frh.flat$stroke.event <- ifelse(frh.flat$strk.tim1 > 365.25*10, 0, frh.flat$strk1)

#The number of stroke events recoded back down between full follow-up and back to first 10 years
table(frh.flat$strk.tim1>365.25*10, frh.flat$strk1)
table(frh.flat$stroke.event,frh.flat$strk1)

#Kaplan-Meier plot of stroke time and event and survival function from 0 to 10
plot(sex.srv2 <- survfit(Surv(stroke.time, stroke.event)~SEX.1,frh.flat), col=1:2, lwd=2, lty=2, mark.time = FALSE)
summary(sex.srv2, times=(0:10)*365.25)
#Include number of events in Table 1


#Create a combined flat dataset, which includes the flat dataset and other variables
flat.dat0 <- frh.flat[,c("RANDID","PERIOD.1","PERIOD.2","PERIOD.3","DIABETES.1","DIABETES.2","DIABETES.3","SYSBP.1",
                         "SYSBP.2","SYSBP.3","SEX.1","strk1","dth1","strk.tim1","dth.tim1","stroke.time","stroke.event")]

frh.dat5 <- frh.dat2[frh.dat2$PERIOD==1, c("RANDID", "PERIOD", "SEX", 
                                           "AGE", "DIABETES", "SYSBP",
                                           "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI",
                                           "STROKE", "TIMESTRK", "DEATH", "TIMEDTH")]
flat.dat <- merge(frh.dat5, flat.dat0, by="RANDID")

#Split flat.dat by gender and use this version for descriptive, preliminary analysis, schoenfeld, martingale analysis
#Do complete case on flat.dat after selecting needed variables. Use this for variable selection
#Go back to flat.dat and do completecase for variables in the final model. Then run final model and compare estimates for data here versus the version (if N differs)





#Martingale residual analysis for age, sysbp, totchol, and bmi

#Write a best subset model using coxph
#Do all and martingale analysis by sex, https://www.mayo.edu/research/documents/biostat-58pdf/doc-10027288
#include # of events in Table 1
#Talk about tie handline
#Figure 1, overlay of males and females KMplot with HR (coef), c-index etc
#Supp Figure 1, schoenfeld plot and log-log on the right? Or log-log and schoenfeld p-value (state the null) in the plot
#Supp Figure 2, martingale residuals 
#Figure 3: requested risk profiles
#Figure 2: KM plot of males and females showing the prognostic model with 3 levels

#Compare best subset containig the 3 key predictors vs backwards vs vibing (2-way intractions and polynomial terms)
#Compare fitted vs observed for survival models
#Eligible set, exclude first 32. Final set using complete case. Eligible set for univariable analysis. Martingale uses complete case (among continuous variables only)
#Are there differences in missing vs others?


#Functional form of the conts within the 3, then pairwise interaction. Then for each additional variable (ranked by LRT), test for main effect and pairwise interaction
#Versus algorithm based
#IF they need these profiles then the variables must be in the model!


#Martingale residual analysis
m.flat.dat <- flat.dat[flat.dat$SEX==1 & !is.na(flat.dat$BMI),
                       c("RANDID", "PERIOD", "SEX", "AGE", "DIABETES", "SYSBP", 
                         "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL","BMI", "stroke.time","stroke.event")]
m.flat.dat2 <- m.flat.dat[complete.cases(m.flat.dat),] #29 excluded


f.flat.dat <- flat.dat[flat.dat$SEX==2 & !is.na(flat.dat$BMI),
                       c("RANDID", "PERIOD", "SEX", "AGE", "DIABETES", "SYSBP", 
                         "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL","BMI", "stroke.time","stroke.event")]
f.flat.dat2 <- f.flat.dat[complete.cases(f.flat.dat),] #81 excluded


male.mod.0 <- coxph(Surv(stroke.time, stroke.event)~1, data = m.flat.dat)
resid.male.mod.0 <- resid(male.mod.0, type = "martingale")
plot(m.flat.dat$BMI, resid.male.mod.0, xlab="Age", ylab="Residual")
lines(lowess(m.flat.dat$BMI, resid.male.mod.0, iter=0),lty=2, col=2, lwd=4)

female.mod.0 <- coxph(Surv(stroke.time, stroke.event)~1, data = f.flat.dat)
resid.female.mod.0 <- resid(female.mod.0, type = "martingale")
plot(f.flat.dat$BMI, resid.female.mod.0, xlab="Age", ylab="Residual")
lines(lowess(f.flat.dat$BMI, resid.female.mod.0, iter=0),lty=2, col=2, lwd=4)


m.mod0 <- coxph(Surv(stroke.time, stroke.event)~BMI, m.flat.dat2)
m.mod <- summary(m.mod0)
sch.test <- cox.zph(m.mod0)
lab <- paste0("Schoenfeld Test p-value: ", round(sch.test$table[2,3],3))
lab1 <- paste0("Cox PH: HR [beta] (SE/p-value) = ", 
               round(m.mod$coefficients[,2],2), " [",
               round(m.mod$coefficients[,1],2), "]",
               " (", round(m.mod$coefficients[,3],2), "/",
              round(m.mod$coefficients[,5],4), ")")
lab2 <- paste0("C-index: ", round(m.mod$concordance[1],4))
#lab0 <- paste0("Variable Name: ", vr)
plot(sch.test)
abline(h=round(m.mod$coefficients[,1],3), lty=2, col="blue", lwd=3)
#mtext(lab0, side = 3, line = 3, font=2)
mtext(lab1, side = 3, line = 2, font=2)
mtext(lab, side = 3, line = 1, font=2)
mtext(lab2, side = 3, line = 0, font=2)


#KM plot skeleton
temp <- flat.dat[, c("RANDID", "PERIOD", "SEX", "AGE", "DIABETES", "SYSBP","BPMEDS",
                     "PREVCHD", "CURSMOKE", "TOTCHOL","BMI", "stroke.time","stroke.event")]
temp$age.cat <- as.numeric(temp$AGE >= median(temp$AGE, na.rm=TRUE))
temp$sysbp.cat <- as.numeric(temp$SYSBP >= median(temp$SYSBP, na.rm=TRUE))
temp$totchol.cat <- as.numeric(temp$TOTCHOL >= median(temp$TOTCHOL, na.rm=TRUE))
temp$bmi.cat <- as.numeric(cut(temp$BMI, right=FALSE, breaks=c(min(temp$BMI, na.rm=T), 18.5, 24.9, 29.9, max(temp$BMI, na.rm=T))))

temp.m <- temp[temp$SEX==1, c("RANDID", "stroke.time","stroke.event", "age.cat", "DIABETES", "sysbp.cat", "BPMEDS",
                              "PREVCHD", "CURSMOKE", "totchol.cat", "bmi.cat")]
temp.f <- temp[temp$SEX==2,c("RANDID", "stroke.time","stroke.event", "age.cat", "DIABETES", "sysbp.cat", "BPMEDS",
                             "PREVCHD", "CURSMOKE", "totchol.cat", "bmi.cat")]

fit1 <- survfit(Surv(temp.m[,2], temp.m[,3])~temp.m[,8], data=temp.m)
fit2 <- survfit(Surv(temp.f[,2], temp.f[,3])~temp.f[,8], data=temp.f)

par(mfrow=c(2,4))
plot(fit1, lwd=3, col=1:4, ylim=c(0.9,1))
plot(fit2, lwd=3, col=1:4, ylim=c(0.9,1))

plot(fit1, fun="cloglog", col=1:4, lwd=3, xlab="time", ylab="log(-log(S(t)))")
plot(fit2, fun="cloglog", col=1:4, lwd=3, xlab="time", ylab="log(-log(S(t)))")




mods3 <- glmulti::glmulti(
  y = Surv(stroke.time, stroke.event)~AGE+DIABETES+SYSBP+BPMEDS+PREVCHD+CURSMOKE+TOTCHOL+BMI ,
  fitfunction = "coxph",
  data = m.flat.dat2,
  intercept = TRUE,
  level = 2,
  method = "h",
  crit = "aic",
  confsetsize = 10,
  plotty = FALSE
)

coxph(Surv(stroke.time, stroke.event)~BMI, m.flat.dat2)

full_model <- coxph(Surv(stroke.time, stroke.event) ~ AGE+DIABETES+SYSBP+BPMEDS+PREVCHD+CURSMOKE+TOTCHOL+BMI, data = m.flat.dat2)
backward_model <- stepAIC(full_model, direction = "both")
summary(backward_model)

full_model0 <- coxph(Surv(stroke.time, stroke.event) ~ AGE+DIABETES+SYSBP+BPMEDS+PREVCHD+CURSMOKE+TOTCHOL+BMI, data = f.flat.dat2)
backward_model0 <- stepAIC(full_model0, direction = "both")
summary(backward_model0)




as.numeric(temp$BMI >= median(temp$TOTCHOL, na.rm=TRUE))

#Only TOTCHOL and BMI have missing values, AGE and SUSBP doesn't


temp

crs.fit <- survfit(Surv(time, status)~group, crs)
plot(crs.fit, fun="cloglog", col=1:2, lwd=3, xlab="time", ylab="log(-log(S(t)))")
legend('left', c('Group=0', 'Group=1'), col=1:2, lwd=3, bty='n')


Add schoenfeld plot to log-log plot
Add KM plot and log-rank test too





text(x = 5, y = 5, labels = my_label)





legend("bottom", "hi")
abline(h=m.mod$coefficients, lty=2, col=2, lwd=3)

legend()



s





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