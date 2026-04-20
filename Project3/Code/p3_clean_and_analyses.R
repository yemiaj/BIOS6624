#######################

# Codes related to the cleaning of data provided for Project 3: Framingham Heart Study: Risk Factors and Probability of Stroke by Sex

######################



##########################
# Needed R packages 
#########################
library(gtsummary) # For creating publication-ready tables
library(survival) # Survival analysis workhorse
library(muhaz) # Create hazard plot
library(MASS) # Needed for implementation of backwards selection


#Import data provided on Canvas
frh.dat <- read.csv("./Project3/DataRaw/frmgham2.csv")

#Imported data has 11627 rows and 4434 unique observations
nrow(frh.dat); length(unique(frh.dat$RANDID))
cat("The imported data has", nrow(frh.dat), "observations and", length(unique(frh.dat$RANDID)), "unique subjects.")

#Identify subjects with prevalent stroke at baseline
exc.stroke <- frh.dat[frh.dat$PREVSTRK==1 & frh.dat$PERIOD==1, "RANDID"] #These are the IDs
table(duplicated(exc.stroke)) #None of them are duplicates
length(unique(exc.stroke)) #and there are 32 of them with prevalent stroke at baseline
nrow(frh.dat[frh.dat$RANDID %in% exc.stroke,]) #These 32 unique subjects represents 60 observations
table(table(frh.dat[frh.dat$RANDID %in% exc.stroke,1])) #The breakdown is as follows, 13 participants have 1 row, 10 have 2 rows and 9 have 3 rows of data in the long format data


#Subset original data and remove these 32 participants with prevalent stroke
frh.dat2 <- frh.dat[!(frh.dat$RANDID %in% exc.stroke), ]

#Remaining data after excluding prevalent stroke at baseline
nrow(frh.dat2); length(unique(frh.dat2$RANDID))
cat("After excluding subjects with prevalent stroke, the remaining data has", nrow(frh.dat2), "observations and", length(unique(frh.dat2$RANDID)), "unique subjects.")


#Select needed covariates (per professor)
frh.dat3 <- frh.dat2[,c("RANDID", "PERIOD", "SEX",                         #These are identifying/ID-related variables
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

#Transform data from long to wide, including the 2 of the variables that we will check for time-varying changes as secondary analysis
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

#Stroke and death status is not missing at any period, so the logic below is okay. However, it will be missing in a wide format if a patient does not have rows of data for either of the 3 PERIODs
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

#For the 7 cases with stroke time beyond death time, hardcode stroke time back to death time (as instructed by professor)
frh.flat$strk1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- 0 #Censor these for stroke event (they were already censored, just making sure)
frh.flat$strk.tim1[frh.flat$strk.tim1>frh.flat$dth.tim1] <- frh.flat[frh.flat$strk.tim1>frh.flat$dth.tim1,]$dth.tim1 #Set stroke time back to death time
frh.flat[frh.flat$RANDID %in% strk.7.IDs, ] #observe to ensure coding is as expected


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
plot(sex.srv2 <- survfit(Surv(stroke.time/365.25, stroke.event) ~ SEX.1, frh.flat), col=1:2, lwd=2, lty=2, mark.time = FALSE, axes = FALSE)
axis(1,at=seq(0,11,1)); axis(2, at=seq(0,1,.1))
summary(sex.srv2, times=(0:10))
sex.srv2
survdiff(Surv(stroke.time/365.25, stroke.event) ~ SEX.1, frh.flat)


#Follow-up between periods. Secondary objective (transiton matrix and colored correlation plot)
table(flat.dat$PERIOD.2, flat.dat$PERIOD.3, flat.dat$PERIOD.1, exclude=NULL) #No missing at PERIOD.1
table(flat.dat$PERIOD.2, flat.dat$PERIOD.3, exclude=NULL) #So this is equivalent to the line above
table(flat.dat$PERIOD.2, flat.dat$PERIOD.3, flat.dat$dth1, exclude=NULL)


#Create a combined flat dataset, which includes the flat dataset and other variables
flat.dat0 <- frh.flat[,c("RANDID","PERIOD.1","PERIOD.2","PERIOD.3","DIABETES.1","DIABETES.2","DIABETES.3","SYSBP.1",
                         "SYSBP.2","SYSBP.3","SEX.1","strk1","dth1","strk.tim1","dth.tim1","stroke.time","stroke.event")]

frh.dat5 <- frh.dat2[frh.dat2$PERIOD==1, c("RANDID", "PERIOD", "SEX", 
                                           "AGE", "DIABETES", "SYSBP",
                                           "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]
flat.dat <- merge(frh.dat5, flat.dat0, by="RANDID")

#Save final flat 'clean' data to .csv
write.csv(flat.dat, "./Project3/DataProcessed/final.frmgham.csv")


##########################
# Analysis datasets
#########################
flat.dat.complete <- flat.dat[flat.dat$anymiss==0, ]


##########################
# Table 1
#########################
#Create categorical variables for SYSBP and BMI
flat.dat$SYSBP.high <- ifelse(flat.dat$SYSBP>160, 1, 0) #Define high blood pressure as systolic BP > 160 (as instructed in class)
flat.dat$BMI.cat <- ifelse(is.na(flat.dat$BMI), NA, ifelse(flat.dat$BMI < 18.50, 1, 
                                                           ifelse(flat.dat$BMI < 25.00, 2,
                                                                  ifelse(flat.dat$BMI < 30.00, 3, 4)))) #Define high blood pressure as systolic BP > 160 (as instructed in class)

#Count of observations with missing data for any of the variables of interest
flat.dat$anymiss <- rowSums(is.na(flat.dat[,c("AGE", "DIABETES", "SYSBP", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]))

#Duplicate final data which will be used for Table 1
flat.dat.tab <- flat.dat

flat.dat.tab$sex.event <- paste0(flat.dat.tab$SEX, flat.dat.tab$stroke.event)
flat.dat.tab$sex.event <- factor(flat.dat.tab$sex.event, levels = c(10, 11, 20, 21), labels = c("Men/No Stroke", "Men/Yes Stroke", "Women/No Stroke", "Women/Yes Stroke"))
flat.dat.tab$SEX <- factor(flat.dat.tab$SEX, levels = c(1, 2), labels = c("Men", "Women"))
flat.dat.tab$DIABETES <- factor(flat.dat.tab$DIABETES, levels = c(1, 0), labels = c("Diabetic", "Not Diabetic"))

flat.dat.tab$BPMEDS <- factor(flat.dat.tab$BPMEDS, levels = c(1, 0), labels = c("Current Use", "Not Currently Used"))
flat.dat.tab$PREVCHD <- factor(flat.dat.tab$PREVCHD, levels = c(1, 0), labels = c("Prevalent Disease", "Free of Disease"))
flat.dat.tab$CURSMOKE <- factor(flat.dat.tab$CURSMOKE, levels = c(1, 0), labels = c("Current Smoker", "Not Current Smoker"))
flat.dat.tab$stroke.event <- factor(flat.dat.tab$stroke.event, levels = c(1, 0), labels = c("Stroke Event", "No Stroke Event"))
flat.dat.tab$anymiss <- factor(flat.dat.tab$anymiss, levels = c(0, 1, 2), labels = c("No missing data", "Missing for 1 variable", "Missing for 2 variables"))
flat.dat.tab$SYSBP.high <- factor(flat.dat.tab$SYSBP.high, levels = c(1, 0), labels = c("High Systolic BP (>160 mmHg)", "Normal Systolic BP (<=160 mmHg)"))
flat.dat.tab$BMI.cat <- factor(flat.dat.tab$BMI.cat, levels = c(1, 2, 3, 4), labels = c("Underweight", "Healthy Weight", "Overweight", "Obese"))

tab1 <- flat.dat.tab |>
  tbl_summary(by = SEX,
              
              include = c("AGE", "DIABETES", "SYSBP", "SYSBP.high", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI", "BMI.cat", "stroke.event", "anymiss"),
              
              label = list(AGE ~ "Age at exam (years)", 
                           DIABETES ~ "Diabetic (casual glucose >= 200 mg/dL)",
                           SYSBP ~ "Average systolic blood pressure (mmHg)",
                           BPMEDS ~ "Use of anti-hypertensive medication",
                           PREVCHD ~ "Prevalent Coronary Heart Disease", 
                           CURSMOKE ~ "Current cigarette smoking ", 
                           TOTCHOL ~ "Serum total cholesterol (mg/dL)", 
                           BMI ~ "Body Mass Index (kg/m^2", 
                           stroke.event ~ "Stroke event status", 
                           SYSBP.high ~ "Systolic blood pressure status", 
                           BMI.cat ~ "BMI Categories", 
                           anymiss ~ "Nos. missing values for key variables"),
              
              type = list(c("AGE", "SYSBP", "TOTCHOL", "BMI") ~ "continuous",
                          c("DIABETES", "BPMEDS", "PREVCHD", "CURSMOKE", "stroke.event", "SYSBP.high", "BMI.cat", "anymiss") ~ "categorical"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd}), [{median}]", 
                               all_categorical() ~ "{n} ({p}%)"),

              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 0),
              
              missing_text = "NA (missing)",
              missing_stat = "{N_miss} ({p_miss}%)") |>
  
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) |>
  add_overall(last = TRUE) |>
  modify_header(label ~ "**Participants' Baseline Characteristics**") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Participants' Sex**") 
tab1



##########################
# Supplementary Table 1: Table 1 by sex+stroke combination status
#########################
supp.tab1 <- flat.dat.tab |>
  tbl_summary(by = sex.event,
              
              include = c("AGE", "DIABETES", "SYSBP", "SYSBP.high", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI", "BMI.cat", "stroke.event", "anymiss"),
              
              label = list(AGE ~ "Age at exam (years)", 
                           DIABETES ~ "Diabetic (casual glucose >= 200 mg/dL)",
                           SYSBP ~ "Average systolic blood pressure (mmHg)",
                           BPMEDS ~ "Use of anti-hypertensive medication",
                           PREVCHD ~ "Prevalent Coronary Heart Disease", 
                           CURSMOKE ~ "Current cigarette smoking ", 
                           TOTCHOL ~ "Serum total cholesterol (mg/dL)", 
                           BMI ~ "Body Mass Index (kg/m^2", 
                           stroke.event ~ "Stroke event status", 
                           SYSBP.high ~ "Systolic blood pressure status", 
                           BMI.cat ~ "BMI Categories", 
                           anymiss ~ "Nos. missing values for key variables"),
              
              type = list(c("AGE", "SYSBP", "TOTCHOL", "BMI") ~ "continuous",
                          c("DIABETES", "BPMEDS", "PREVCHD", "CURSMOKE", "stroke.event", "SYSBP.high", "BMI.cat", "anymiss") ~ "categorical"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd}), [{median}]", 
                               all_categorical() ~ "{n} ({p}%)"),
              
              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 0),
              
              missing_text = "NA (missing)",
              missing_stat = "{N_miss} ({p_miss}%)") |>
  
  #add_p(pvalue_fun = label_style_pvalue(digits = 3)) |>
  #add_overall(last = TRUE) |>
  modify_header(label ~ "**Participants' Baseline Characteristics**") |>
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Participants' Sex and Stroke Status Combination**") 
supp.tab1



##########################
# Figure 1: Survival and hazard plot by gender
#########################
male.fit <- survfit(Surv(stroke.time/365.25, stroke.event) ~ 1, data = flat.dat[flat.dat$SEX==1, ])
female.fit <- survfit(Surv(stroke.time/365.25, stroke.event) ~ 1, data = flat.dat[flat.dat$SEX==2, ])

times=seq(0,10,1)

male.rsk<-data.frame(rbind(summary(male.fit[1], times=seq(0,10,1))$n.risk))
#male.rsk<-cbind(male.rsk,0)
female.rsk<-data.frame(rbind(summary(female.fit[1], times=seq(0,10,1))$n.risk))
#female.fit<-cbind(female.fit,0)

par(mar=c(7.5, 5, 2, 2)+0.1)
par(mfrow=c(1,2))
plot(male.fit, conf.int=FALSE, mark.time=T, xlab='Follow-up time (years)', ylab='Probability of stroke', lwd=2, cex=.8, 
     xaxt='n', yaxt='n', main='KM Plot of Time to Stroke by Sex',xlim=c(0,10), cex.main=1, ylim=c(0.9,1))
axis(1,at=seq(0,10,1)); axis(2,at=seq(0.9,1,.01))
lines(female.fit, conf.int=FALSE, mark.time=T, lwd=2, cex=.8, col=2)
mtext(side=1, line=4,   at=c(-1),"Nos. at Risk", cex=.8, font=2)
mtext(side=1, line=5, at=c(-1, times), c("Men:", male.rsk[1,]), cex=1)
mtext(side=1, line=6, at=c(-1, times), c("Women:", female.rsk[1,]), cex=1, col=2)

summary(male.fit, times=10)
summary(female.fit, times=10)

legend('left',c("Sex (N/events); 5-year probability",
                "Men = 1930 / 49; 97.3%", 
                "Women = 2472 / 62; 97.4%"),
       bty='n', lty=c(0,1,1),lwd=4, cex=1, text.font=2, col=c(0,1,2))


#Hazard plot
m.haz.plot <- muhaz(flat.dat[flat.dat$SEX==1,]$stroke.time/365.25, flat.dat[flat.dat$SEX==1,]$stroke.event, min.time = 0, max.time = 10, bw.method = 'global', b.cor = "both")
f.haz.plot <- muhaz(flat.dat[flat.dat$SEX==2,]$stroke.time/365.25, flat.dat[flat.dat$SEX==2,]$stroke.event, min.time = 0, max.time = 10, bw.method = 'global', b.cor = "both")
plot(m.haz.plot, col=1, lwd=4, main="Instantaneous Risk (Hazard) Plot for Time to Stroke", cex.main=1)
lines(f.haz.plot, col=2, lwd=4, lty=2)
legend("left", c("Men", "Women"), col=1:2, bty="n", lty=1, lwd=4, text.font=2)



#Modelling
##########################
# Supp. Figure 1: Univariable Analysis, KM & Cox PH regression
#########################
flat.dat.km <- flat.dat[, c("RANDID", "PERIOD", "SEX", "AGE", "DIABETES", "SYSBP.high","BPMEDS",
                            "PREVCHD", "CURSMOKE", "TOTCHOL","BMI.cat", "stroke.time","stroke.event","anymiss")] #Data specific for creating KM plot
flat.dat.km$age.cat <- as.numeric(flat.dat.km$AGE >= median(flat.dat.km$AGE, na.rm=TRUE))
flat.dat.km$totchol.cat <- as.numeric(flat.dat.km$TOTCHOL >= median(flat.dat.km$TOTCHOL, na.rm=TRUE))

flat.dat.km <- flat.dat.km[, c("RANDID", "PERIOD", "SEX", "age.cat", "DIABETES", "SYSBP.high","BPMEDS",
                               "PREVCHD", "CURSMOKE", "totchol.cat","BMI.cat", "stroke.time","stroke.event", "anymiss")]

uni.km.plot <- function(vari, lab){
  form <- as.formula(paste0("Surv(stroke.time/365.25, stroke.event) ~ ", vari))
  m <- survfit(form, data=flat.dat.km[flat.dat.km$SEX==1,])
  f <- survfit(form, data=flat.dat.km[flat.dat.km$SEX==2,])
  
  mc <- survdiff(form, data=flat.dat.km[flat.dat.km$SEX==1,])$chisq
  fc<- survdiff(form, data=flat.dat.km[flat.dat.km$SEX==2,])$chisq
  
  m.mod <- summary(coxph(form, data=flat.dat.km[flat.dat.km$SEX==1,]))
  f.mod <- summary(coxph(form, data=flat.dat.km[flat.dat.km$SEX==2,]))

  m.zph <- cox.zph(coxph(form, data=flat.dat.km[flat.dat.km$SEX==1,]))$table[2,3]
  f.zph <- cox.zph(coxph(form, data=flat.dat.km[flat.dat.km$SEX==2,]))$table[2,3]
  
  m.lab1 <- paste0("Cox PH: HR [SE] (p-value) = ", round(m.mod$coefficients[,2],2),
                 " [", round(m.mod$coefficients[,3],2), "]", " (p-value = ", round(m.mod$coefficients[,5],4), ")")
  m.lab2 <- paste0("C-index: ", round(m.mod$concordance[1],3))
  m.lab3 <- paste0("Logrank Chisq Statistic: ", round(mc,3))
  m.lab4 <- paste0("Schoenfeld residual PH test p-value: ", round(m.zph,3))
  
  
  f.lab1 <- paste0("Cox PH: HR [SE] (p-value) = ", round(f.mod$coefficients[,2],2),
                 " [", round(f.mod$coefficients[,3],2), "]", " (p-value = ", round(f.mod$coefficients[,5],4), ")")
  f.lab2 <- paste0("C-index: ", round(f.mod$concordance[1],3))
  f.lab3 <- paste0("Logrank Chisq Statistic: ", round(fc,3))
  f.lab4 <- paste0("Schoenfeld residual PH test p-value: ", round(f.zph,3))
  
  
  plot(m, lwd=4, xlab="Time (years)", ylab="Stroke Probability", col=1:4, main="", xaxt='n', ylim=c(0.8,1))
  mtext(paste0("Men: Time to Stroke by ", lab), side = 3, line = 0, font=2, adj=0, cex=.9)
  axis(1,at=seq(0,10,1))
  mtext(m.lab1, side = 3, line = -23, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab3, side = 3, line = -24, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab2, side = 3, line = -25, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab4, side = 3, line = -26, font=2, adj=0, col='blue', cex=.8)
  legend('left', paste0(sort(unique(flat.dat.km[flat.dat.km$SEX==1, vari]))), lwd=4, col=1:4, bty='n')

  
  plot(f, lwd=4, xlab="Time (years)", ylab="Stroke Probability", col=1:4, main="", xaxt='n', ylim=c(0.8,1))
  mtext(paste0("Women: Time to Stroke by ", lab), side = 3, line = 0, font=2, adj=0, cex=.9)
  axis(1,at=seq(0,10,1))
  mtext(f.lab1, side = 3, line = -23, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab3, side = 3, line = -24, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab2, side = 3, line = -25, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab4, side = 3, line = -26, font=2, adj=0, col='blue', cex=.8)
  legend('left', paste0(sort(unique(flat.dat.km[flat.dat.km$SEX==2, vari]))), lwd=4, col=1:4, bty='n')
}


par(mfrow=c(4,4))
uni.km.plot("age.cat", "Age (>=median vs < median)")
uni.km.plot("DIABETES", "Diabetes status")
uni.km.plot("SYSBP.high", "Systolic BP (>160 vs <=160 mmmHg)")
uni.km.plot("BPMEDS", "Anti-hyper. meds use status")
uni.km.plot("PREVCHD", "Prevalent CHD status")
uni.km.plot("CURSMOKE", "Smoking status (current vs not-current)")
uni.km.plot("totchol.cat", "Serum total cholesterol status (>=median vs < median)")
uni.km.plot("BMI.cat", "BMI categories")

uni.km.plot("anymiss", "Missingness")


##########################
# Figure 1: Martingale analysis for functional form
#########################
#Continuous variables: AGE SYSBP TOTCHOL BMI
martingale.plot<- function(vari, lab){
  
  form <- as.formula("Surv(stroke.time/365.25, stroke.event) ~ 1")
  
  m <- coxph(form, data=flat.dat.complete[flat.dat.complete$SEX==1,])
  resid.m <- resid(m, type = "martingale")
  plot(flat.dat.complete[flat.dat.complete$SEX==1, vari], resid.m, xlab=lab, ylab="Martingale Residual", main = paste0("Men: Martingale Residual Plot for ", lab))
  lines(lowess(flat.dat.complete[flat.dat.complete$SEX==1, vari], 
               resid.m, iter=0),lty=2, col=2, lwd=4)
  
  f <- coxph(form, data=flat.dat.complete[flat.dat.complete$SEX==2,])
  resid.f <- resid(f, type = "martingale")
  plot(flat.dat.complete[flat.dat.complete$SEX==2, vari], resid.f, xlab=lab, ylab="Martingale Residual", main = paste0("Women: Martingale Residual Plot for ", lab))
  lines(lowess(flat.dat.complete[flat.dat.complete$SEX==2, vari], 
               resid.f, iter=0),lty=2, col=2, lwd=4)
}
par(mfrow=c(2,4))
martingale.plot("AGE", "Age at exam (years)")
martingale.plot("SYSBP", "Average systolic blood pressure (mmHg)")
martingale.plot("TOTCHOL", "Serum total cholesterol (mg/dL)")
martingale.plot("BMI", "Body Mass Index (kg/m^2)")


logLik(coxph(Surv(stroke.time, stroke.event)~TOTCHOL+I(TOTCHOL^2), data = m.flat.dat2))

logLik(coxph(Surv(stroke.time, stroke.event)~BMI+I(BMI^2), data = f.flat.dat))

#Martingale did not reveal any major non-linearity, at least none that fits a quadratic term
#Other non-linear approaches, e.g. restricted splines may be a better fit, but not pursuing that here



##########################
# Primary analysis: Build the final model (use complete data, then use data with non-missing values for final variables in the model)
#########################
flat.dat.comp2 <- flat.dat.complete[,c("RANDID", "stroke.time", "stroke.event", "SEX", "AGE", "DIABETES", "SYSBP", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]

#backwards selection, then test for interaction (keep an eye on number of events)

#Full model for males and females
male.full.model <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP + BPMEDS + PREVCHD + CURSMOKE + TOTCHOL + BMI, data = flat.dat.comp2[flat.dat.comp2$SEX==1, ])
female.full.model <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP + BPMEDS + PREVCHD + CURSMOKE + TOTCHOL + BMI, data = flat.dat.comp2[flat.dat.comp2$SEX==2, ])

# Perform backward selection
male_backward <- stepAIC(male.full.model, direction = "backward", trace = 0)
summary(male_backward)$coefficients
names(male_backward$coefficients) #Variables in the final backward model for maless

female_backward <- stepAIC(female.full.model, direction = "backward", trace = 0)
summary(female_backward)$coefficients
names(female_backward$coefficients) #Variables in the final backward model for females


#Function to assess interaction between a candidate variable and variables already in the model

#Function for males
#Modified function from Microsoft Copilot
male.interaction <- function(var, data) {
  
  # Base model
  m0 <- as.formula("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP + CURSMOKE")
  mf0 <- coxph(m0, data = data)
  
  # Main-effects
  m1 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES+ SYSBP + CURSMOKE +", var))
  mf1 <- coxph(m1, data = data)
  
  # Interaction with AGE
  m2 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE * ", var, " + DIABETES + SYSBP + CURSMOKE"))
  mf2 <- coxph(m2, data = data)
  
  # Interaction with DIABETES
  m3 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ DIABETES * ", var, " + AGE + SYSBP + CURSMOKE"))
  mf3 <- coxph(m3, data = data)
  
  # Interaction with SYSBP
  m4 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ SYSBP * ", var, " + AGE + DIABETES + CURSMOKE"))
  mf4 <- coxph(m4, data = data)
  
  # Interaction with CURSMOKE
  m5 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ CURSMOKE * ", var, " + AGE + DIABETES + SYSBP"))
  mf5 <- coxph(m5, data = data)
  
  
  # Extract -2 log-likelihoods
  res <- data.frame(
    model = c(
      "Baseline (AGE + DIABETES + SYSBP + CURSMOKE)",
      paste0("Main effects (+ ", var, ")"),
      paste0("AGE:", var, " int."),
      paste0("DIABETES:", var, " int."),
      paste0("SYSBP:", var, " int."),
      paste0("CURSMOKE:", var, " int.")
    ),
    neg2LogLik = c(
      -2 * as.numeric(logLik(mf0)),
      -2 * as.numeric(logLik(mf1)),
      -2 * as.numeric(logLik(mf2)),
      -2 * as.numeric(logLik(mf3)),
      -2 * as.numeric(logLik(mf4)),
      -2 * as.numeric(logLik(mf5))
    )
  )
  res$LRTstat <- (-2 * as.numeric(logLik(mf0))) - res$neg2LogLik
  res$LRTstat.pvalue <- 1-pchisq(res$LRTstat,1)
  res <- cbind(res[,1], round(res[,2:4],3))
  names(res)[1] <- "model"
  return(res)
}
#BPMEDS PREVCHD CURSMOKE TOTCHOL   BMI 
m1 <- male.interaction("BPMEDS", flat.dat.comp2[flat.dat.comp2$SEX==1, ])
m2 <- male.interaction("PREVCHD", flat.dat.comp2[flat.dat.comp2$SEX==1, ])
m3 <- male.interaction("TOTCHOL", flat.dat.comp2[flat.dat.comp2$SEX==1, ])
m4 <- male.interaction("BMI", flat.dat.comp2[flat.dat.comp2$SEX==1, ])

cbind(m1, m2, m3, m4)

#Function for females, even though backwards selection identified AGE and SYSBP only, DIABETES is added because this was specified by the PI
female.interaction <- function(var, data) {
  
  # Base model
  m0 <- as.formula("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP")
  mf0 <- coxph(m0, data = data)
  
  # Main-effects
  m1 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP + ", var))
  mf1 <- coxph(m1, data = data)
  
  # Interaction with AGE
  m2 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE * ", var, " + DIABETES + SYSBP"))
  mf2 <- coxph(m2, data = data)
  
  # Interaction with DIABETES
  m3 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ DIABETES * ", var, " + AGE + SYSBP"))
  mf3 <- coxph(m3, data = data)
  
  # Interaction with SYSBP
  m4 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ SYSBP * ", var, " + AGE + DIABETES"))
  mf4 <- coxph(m4, data = data)
  
  # Extract -2 log-likelihoods
  res <- data.frame(
    model = c(
      "Baseline (AGE + DIABETES+ SYSBP)",
      paste0("Main effects (+ ", var, ")"),
      paste0("AGE:", var, " int."),
      paste0("DIABETES:", var, " int."),
      paste0("SYSBP:", var, " int.")
    ),
    neg2LogLik = c(
      -2 * as.numeric(logLik(mf0)),
      -2 * as.numeric(logLik(mf1)),
      -2 * as.numeric(logLik(mf2)),
      -2 * as.numeric(logLik(mf3)),
      -2 * as.numeric(logLik(mf4))
    )
  )
  res$LRTstat <- (-2 * as.numeric(logLik(mf0))) - res$neg2LogLik
  res$LRTstat.pvalue <- 1-pchisq(res$LRTstat,1)
  res <- cbind(res[,1], round(res[,2:4],3))
  names(res)[1] <- "model"
  return(res)
}

f1 <- female.interaction("BPMEDS", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f2 <- female.interaction("PREVCHD", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f3 <- female.interaction("CURSMOKE", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f4 <- female.interaction("TOTCHOL", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f5 <- female.interaction("BMI", flat.dat.comp2[flat.dat.comp2$SEX==2, ])

cbind(f1, f2, f3, f4, f5)

##########################
# Final model
#########################

male.final.model <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP * BPMEDS, data = flat.dat.comp2[flat.dat.comp2$SEX==1, ])
summary(male.final.model)

female.final.model <- coxph(Surv(stroke.time, stroke.event) ~ DIABETES + SYSBP + AGE * TOTCHOL, data = flat.dat.comp2[flat.dat.comp2$SEX==2, ])
summary(female.final.model)

#Build model (check impac of binary sysbp and cont bmi)
#Risk profiles


#Reproduce the p-values in Table 1
#Reason for backwards
#Multiple testing as limitation
#Proximity of collection of risk factors



#proportion of missingness by sex. This will make it into Table 1, so not needed here
#prop.table(table(flat.dat$anymiss, flat.dat$SEX),2)*100 




#Split flat.dat by gender and use this version for descriptive, preliminary analysis, schoenfeld, martingale analysis
#Do complete case on flat.dat after selecting needed variables. Use this for variable selection
#Go back to flat.dat and do completecase for variables in the final model. Then run final model and compare estimates for data here versus the version (if N differs)

m.flat.dat <- flat.dat[flat.dat$SEX==1,]
f.flat.dat <- flat.dat[flat.dat$SEX==2,]




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
#Include number of events in Table 1
#Variables were coded to show risk (i.e. such that HR >1), not protective effect

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


male.mod.0 <- coxph(Surv(stroke.time, stroke.event)~1, data = m.flat.dat2)
resid.male.mod.0 <- resid(male.mod.0, type = "martingale")
plot(m.flat.dat2$BMI, resid.male.mod.0, xlab="Age", ylab="Residual")
lines(lowess(m.flat.dat2$BMI, resid.male.mod.0, iter=0),lty=2, col=2, lwd=4)

female.mod.0 <- coxph(Surv(stroke.time, stroke.event)~1, data = f.flat.dat2)
resid.female.mod.0 <- resid(female.mod.0, type = "martingale")
plot(f.flat.dat2$BMI, resid.female.mod.0, xlab="Age", ylab="Residual")
lines(lowess(f.flat.dat2$BMI, resid.female.mod.0, iter=0),lty=2, col=2, lwd=4)


logLik(coxph(Surv(stroke.time, stroke.event)~TOTCHOL+I(TOTCHOL^2), data = m.flat.dat2))

logLik(coxph(Surv(stroke.time, stroke.event)~BMI+I(BMI^2), data = f.flat.dat))
#Martingale did not reveal any major non-linearity, at least none that fits a quadratic term
#Other non-linear approaches, e.g. restricted splines may be a better fit, but not pursuing that here

#Model building
vars <- paste0("AGE + DIABETES+ SYSBP")
#BPMEDS PREVCHD CURSMOKE TOTCHOL   BMI 

#In a stepwise manner, there is no interaction between age, diabetes, or sysbp
mod.m.0 <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES+ SYSBP, m.flat.dat2)
mod.m.1 <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES+ SYSBP + AGE:DIABETES, m.flat.dat2)
1-pchisq(logLik(mod.m.0)*-2 - logLik(mod.m.1)*-2, 1)


mod.fun <- function(var){
  form <- as.formula( paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES+ SYSBP + ", var))
  mod <- coxph(form, data=m.flat.dat2)
}
#Example usage
mod.fun("BPMEDS")


#Exhaustive model selection 
https://rpubs.com/kaz_yos/exhaustive
#all subset regression
https://rpubs.com/kaz_yos/all-subset
#bess
https://cran.r-project.org/web/packages/BeSS/BeSS.pdf


#BPMEDS PREVCHD CURSMOKE TOTCHOL   BMI 
#Use LRT to order this variable for inclusion

#Microsoft Copilot used to create the skeleton for this code
mod.fun <- function(var, data) {
  
  # Base model
  m0 <- as.formula("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP")
  mf0 <- coxph(m0, data = data)
  
  # Main-effects
  m1 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP + ", var))
  mf1 <- coxph(m1, data = data)
  
  # Interaction with AGE
  m2 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE * ", var, " + DIABETES + SYSBP"))
  mf2 <- coxph(m2, data = data)
  
  # Interaction with DIABETES
  m3 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ DIABETES * ", var, " + AGE + SYSBP"))
  mf3 <- coxph(m3, data = data)
  
  # Interaction with SYSBP
  m4 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ SYSBP * ", var, " + AGE + DIABETES"))
  mf4 <- coxph(m4, data = data)
  
  # Extract -2 log-likelihoods
  res <- data.frame(
    model = c(
      "Baseline (AGE + DIABETES+ SYSBP)",
      paste0("Main effects (+ ", var, ")"),
      paste0("AGE:", var, " interaction"),
      paste0("DIABETES:", var, " interaction"),
      paste0("SYSBP:", var, " interaction")
    ),
    neg2LogLik = c(
      -2 * as.numeric(logLik(mf0)),
      -2 * as.numeric(logLik(mf1)),
      -2 * as.numeric(logLik(mf2)),
      -2 * as.numeric(logLik(mf3)),
      -2 * as.numeric(logLik(mf4))
    )
  )
  res$LRTstat <- (-2 * as.numeric(logLik(mf0))) - res$neg2LogLik
  res$LRTstat.pvalue <- 1-pchisq(res$LRTstat,1)
  return(res)
}

#BPMEDS PREVCHD CURSMOKE TOTCHOL   BMI 
mod.fun("BPMEDS", m.flat.dat2)
mod.fun("CURSMOKE", m.flat.dat2)

#females
#mod.fun("BPMEDS", f.flat.dat2)
#mod.fun("PREVCHD", f.flat.dat2)
mod.fun("CURSMOKE", f.flat.dat2)
mod.fun("TOTCHOL", f.flat.dat2)
cbind(a,b)

#Final model for males
male.mod <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP*BPMEDS+ CURSMOKE, m.flat.dat2)
summary(male.mod)

#Final model for females
female.mod <- coxph(Surv(stroke.time, stroke.event) ~ DIABETES + SYSBP + AGE*TOTCHOL, f.flat.dat2)
summary(female.mod)


-2*logLik(coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP, f.flat.dat2))
-2*logLik(coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP+ AGE*TOTCHOL, f.flat.dat2))

summary(coxph(Surv(stroke.time, stroke.event) ~ DIABETES + SYSBP + AGE*TOTCHOL, f.flat.dat2))

d








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
temp$sysbp.cat <- ifelse(temp$SYSBP<=160,0,1)
temp$totchol.cat <- as.numeric(temp$TOTCHOL >= median(temp$TOTCHOL, na.rm=TRUE))
temp$bmi.cat <- as.numeric(cut(temp$BMI, right=FALSE, breaks=c(min(temp$BMI, na.rm=T), 18.5, 24.9, 29.9, max(temp$BMI, na.rm=T))))

temp.m <- temp[temp$SEX==1, c("RANDID", "stroke.time","stroke.event", "age.cat", "DIABETES", "sysbp.cat", "BPMEDS",
                              "PREVCHD", "CURSMOKE", "totchol.cat", "bmi.cat")]
temp.f <- temp[temp$SEX==2,c("RANDID", "stroke.time","stroke.event", "age.cat", "DIABETES", "sysbp.cat", "BPMEDS",
                             "PREVCHD", "CURSMOKE", "totchol.cat", "bmi.cat")]

par(mfrow=c(2,4))
v<-11
fit1 <- survfit(Surv(temp.m[,2], temp.m[,3])~temp.m[,v], data=temp.m)
fit2 <- survfit(Surv(temp.f[,2], temp.f[,3])~temp.f[,v], data=temp.f)

plot(fit1, lwd=3, col=1:4, ylim=c(0.8,1), main=paste0("Males, ", colnames(temp.m)[v]))
plot(fit2, lwd=3, col=1:4, ylim=c(0.8,1), main=paste0("Females, ", colnames(temp.m)[v]))



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