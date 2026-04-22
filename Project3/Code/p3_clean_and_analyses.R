#######################

# Code related to the cleaning of data provided for Project 3: Framingham Heart Study: Risk Factors and Probability of Stroke by Sex

######################


##########################
# Needed R packages 
#########################
library(gtsummary) # For creating publication-ready tables
library(survival) # Survival analysis workhorse
library(muhaz) # Create hazard plot
library(MASS) # Needed for implementation of backwards selection
library(gt) #Needed to export gtsummary table to Word
library(flextable) #Save a dataframe object to Word

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
1-pchisq(survdiff(Surv(strk.tim1, strk1)~SEX.1,frh.flat)$chisq,1)
cox.zph(coxph(Surv(strk.tim1, strk1)~SEX.1,frh.flat)) #PH okay based on Schoenfeld, how about log-log?


#Limit data to the first 10 years of follow-up
frh.flat$stroke.time <- ifelse(frh.flat$strk.tim1 > 365.25*10, 365.25*10, frh.flat$strk.tim1)
frh.flat$stroke.event <- ifelse(frh.flat$strk.tim1 > 365.25*10, 0, frh.flat$strk1)

#The number of stroke events recoded back down between full follow-up and back to first 10 years
table(frh.flat$strk.tim1>365.25*10, frh.flat$strk1)
table(frh.flat$stroke.event,frh.flat$strk1)

frh.flat$stroke.time <- frh.flat$stroke.time/365.25 #Change the time scale from days to years

#Kaplan-Meier plot of stroke time and event and survival function from 0 to 10
plot(sex.srv2 <- survfit(Surv(stroke.time, stroke.event) ~ SEX.1, frh.flat), col=1:2, lwd=2, lty=2, mark.time = FALSE, axes = FALSE)
axis(1,at=seq(0,11,1)); axis(2, at=seq(0,1,.1))
summary(sex.srv2, times=(0:10))
sex.srv2
survdiff(Surv(stroke.time, stroke.event) ~ SEX.1, frh.flat)
1-pchisq(survdiff(Surv(stroke.time, stroke.event) ~ SEX.1, frh.flat)$chisq,1)


#Create a combined flat dataset, which includes the flat dataset and other variables
flat.dat0 <- frh.flat[,c("RANDID","PERIOD.1","PERIOD.2","PERIOD.3","DIABETES.1","DIABETES.2","DIABETES.3","SYSBP.1",
                         "SYSBP.2","SYSBP.3","SEX.1","strk1","dth1","strk.tim1","dth.tim1","stroke.time","stroke.event")]

frh.dat5 <- frh.dat2[frh.dat2$PERIOD==1, c("RANDID", "PERIOD", "SEX", 
                                           "AGE", "DIABETES", "SYSBP",
                                           "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]
flat.dat <- merge(frh.dat5, flat.dat0, by="RANDID")

#Count of observations with missing data for any of the variables of interest
flat.dat$anymiss <- rowSums(is.na(flat.dat[,c("AGE", "DIABETES", "SYSBP", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]))

flat.dat$period.ltfu12 <- rowSums(is.na(flat.dat[,c("PERIOD.1", "PERIOD.2")])) #redundant because no missing at baseline
flat.dat$period.ltfu23 <- rowSums(is.na(flat.dat[,c("PERIOD.2", "PERIOD.3")]))
flat.dat$period.ltfu123 <- rowSums(is.na(flat.dat[,c("PERIOD.1", "PERIOD.2", "PERIOD.3")])) #redundant because no missing at baseline, so this is equal to period.ltfu23

flat.dat$sysbp.miss.fu <- rowSums(is.na(flat.dat[,c("SYSBP.1", "SYSBP.2", "SYSBP.3")])) #Missingness across time for any of these key variables
flat.dat$diabetes.miss.fu <- rowSums(is.na(flat.dat[,c("DIABETES.1", "DIABETES.2", "DIABETES.3")]))

#Create categorical variables for SYSBP and BMI
flat.dat$SYSBP.high <- ifelse(is.na(flat.dat$SYSBP), NA, ifelse(flat.dat$SYSBP>=160, 1, 0)) #Define high blood pressure as systolic BP >= 160 (as instructed in class)
flat.dat$BMI.cat <- ifelse(is.na(flat.dat$BMI), NA, ifelse(flat.dat$BMI < 18.50, 1, 
                                                           ifelse(flat.dat$BMI < 25.00, 2,
                                                                  ifelse(flat.dat$BMI < 30.00, 3, 4))))

#Classify sysbp at follow-up periods (for time-varying analysis)
flat.dat$SYSBP.high.p1 <- ifelse(is.na(flat.dat$SYSBP.1), NA, ifelse(flat.dat$SYSBP.1>=160, 1, 0))
flat.dat$SYSBP.high.p2 <- ifelse(is.na(flat.dat$SYSBP.2), NA, ifelse(flat.dat$SYSBP.2>=160, 1, 0))
flat.dat$SYSBP.high.p3 <- ifelse(is.na(flat.dat$SYSBP.3), NA, ifelse(flat.dat$SYSBP.3>=160, 1, 0))


#Calculate change in SYSBP at different timepoints
flat.dat$SYSBP.diff.12 <- ifelse(flat.dat$period.ltfu123==0, flat.dat$SYSBP.2 - flat.dat$SYSBP.1, NA)
flat.dat$SYSBP.diff.23 <- ifelse(flat.dat$period.ltfu123==0, flat.dat$SYSBP.3 - flat.dat$SYSBP.2, NA)



#Create a 4-level categorical variable for the combinations for Sex and Stroke event
flat.dat$sex.event <- paste0(flat.dat$SEX, flat.dat$stroke.event)


#Save final flat clean data to .csv
write.csv(flat.dat, "./Project3/DataProcessed/final.frmgham.csv")


##########################
# Analysis datasets
#########################

#Duplicate final data which will be used for Table 1
flat.dat.tab <- flat.dat

#Complete data without missing values for the 8 predictor variables of interest ("AGE", "DIABETES", "SYSBP", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")
flat.dat.complete <- flat.dat[flat.dat$anymiss==0, ]
cat("Complete case data (with respect to the 8 predictors) has", nrow(flat.dat.complete), "observations compared to the final analysis set with", nrow(flat.dat), "observations.")


##########################
# Table 1
#########################
flat.dat.tab$sex.event <- factor(flat.dat.tab$sex.event, levels = c(10, 11, 20, 21), labels = c("Men/No Stroke", "Men/Yes Stroke", "Women/No Stroke", "Women/Yes Stroke"))
flat.dat.tab$SEX <- factor(flat.dat.tab$SEX, levels = c(1, 2), labels = c("Men", "Women"))
flat.dat.tab$DIABETES <- factor(flat.dat.tab$DIABETES, levels = c(1, 0), labels = c("Diabetic", "Not Diabetic"))

flat.dat.tab$BPMEDS <- factor(flat.dat.tab$BPMEDS, levels = c(1, 0), labels = c("Current Use", "Not Currently Used"))
flat.dat.tab$PREVCHD <- factor(flat.dat.tab$PREVCHD, levels = c(1, 0), labels = c("Prevalent Disease", "Free of Disease"))
flat.dat.tab$CURSMOKE <- factor(flat.dat.tab$CURSMOKE, levels = c(1, 0), labels = c("Current Smoker", "Not Current Smoker"))
flat.dat.tab$stroke.event <- factor(flat.dat.tab$stroke.event, levels = c(1, 0), labels = c("Stroke Event", "No Stroke Event"))
flat.dat.tab$anymiss <- factor(flat.dat.tab$anymiss, levels = c(0, 1, 2), labels = c("No missing data", "Missing for 1 variable", "Missing for 2 variables"))
flat.dat.tab$SYSBP.high <- factor(flat.dat.tab$SYSBP.high, levels = c(1, 0), labels = c("High Systolic BP (>=160 mmHg)", "Normal Systolic BP (<160 mmHg)"))
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

tab1 %>%
  as_gt() %>%
  gt::gtsave(filename = "./Project3/Reports/table_figures/Table 1.docx")


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

supp.tab1 %>%
  as_gt() %>%
  gt::gtsave(filename = "./Project3/Reports/table_figures/Supp. Table 1.docx")


##########################
# Figure 1: Survival and hazard plot by gender
#########################
male.fit <- survfit(Surv(stroke.time, stroke.event) ~ 1, data = flat.dat[flat.dat$SEX==1, ])
female.fit <- survfit(Surv(stroke.time, stroke.event) ~ 1, data = flat.dat[flat.dat$SEX==2, ])

times=seq(0,10,1)

male.rsk<-data.frame(rbind(summary(male.fit[1], times=seq(0,10,1))$n.risk))
#male.rsk<-cbind(male.rsk,0)
female.rsk<-data.frame(rbind(summary(female.fit[1], times=seq(0,10,1))$n.risk))
#female.fit<-cbind(female.fit,0)

pdf(file = "./Project3/Reports/table_figures/Figure 1.pdf", width = 16, height = 7)
#png(filename = "./Project3/Reports/Figure 1.png", width = 1300, height = 600)
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
m.haz.plot <- muhaz(flat.dat[flat.dat$SEX==1,]$stroke.time, flat.dat[flat.dat$SEX==1,]$stroke.event, 
                    min.time = 0, max.time = 10, bw.method = 'global', b.cor = "both")
f.haz.plot <- muhaz(flat.dat[flat.dat$SEX==2,]$stroke.time, flat.dat[flat.dat$SEX==2,]$stroke.event, min.time = 0, max.time = 10, bw.method = 'global', b.cor = "both")
plot(m.haz.plot, col=1, lwd=4, main="Instantaneous Risk (Hazard) Plot for Time to Stroke", cex.main=1)
lines(f.haz.plot, col=2, lwd=4, lty=2)
legend("left", c("Men", "Women"), col=1:2, bty="n", lty=1, lwd=4, text.font=2)
dev.off()


#parametric accelerated failure-time models (not discussed in the report)

#Males
#-2logLik, # params
#exponential: 677.4204, 1
#extreme: 685.0366, 2
#logistic: 684.9501,2
#gaussian: 682.4213, 2
#weibull: 671.1462, 2
#rayleigh: 676.9741,1
#lognormal: 671.9517, 2
#loglogistic: 671.2038, 2
#t: 689.833, 2

#Among males: loglogistic, lognormal, weibull are an improvement over exponential distribution for hazard and may be a good choice of modelling.

summary(survreg(Surv(stroke.time, stroke.event)~1, data=flat.dat[flat.dat$SEX==1,], dist="t"))
-2*as.numeric(logLik(survreg(Surv(stroke.time, stroke.event)~1, data=flat.dat[flat.dat$SEX==1,], dist="t")))
#dist= should be one of “extreme”, “logistic”, “gaussian”, “weibull”, “exponential”, “rayleigh”, “loggaussian”, “lognormal”, “loglogistic”, “t”


#Females
#-2logLik, # params
#exponential: 861.4281, 1
#extreme: 877.4171, 2
#logistic: 877.3071,2
#gaussian: 874.1355, 2
#weibull: 859.8811, 2
#rayleigh: 881.5393,1
#lognormal: 865.2464, 2
#loglogistic: 860.0413, 2
#t: 883.4625, 2

#Among females: exponential distribution for hazard appears to be the best choice

summary(survreg(Surv(stroke.time, stroke.event)~1, data=flat.dat[flat.dat$SEX==2,], dist="t"))
-2*as.numeric(logLik(survreg(Surv(stroke.time, stroke.event)~1, data=flat.dat[flat.dat$SEX==2,], dist="t")))
#dist= should be one of “extreme”, “logistic”, “gaussian”, “weibull”, “exponential”, “rayleigh”, “loggaussian”, “lognormal”, “loglogistic”, “t”



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
  form <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ ", vari))
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
  
  
  plot(m, lwd=4, xlab="Time (years)", ylab="Stroke Probability", col=1:4, main="", xaxt='n', ylim=c(0.8,1), cex.lab = 1.5)
  mtext(paste0("Men: Time to Stroke by ", lab), side = 3, line = 0, font=2, adj=0, cex=1)
  axis(1,at=seq(0,10,1))
  mtext(m.lab1, side = 3, line = -23, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab3, side = 3, line = -24, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab2, side = 3, line = -25, font=2, adj=0, col='blue', cex=.8)
  mtext(m.lab4, side = 3, line = -26, font=2, adj=0, col='blue', cex=.8)
  legend('bottomleft', paste0(sort(unique(flat.dat.km[flat.dat.km$SEX==1, vari]))), lwd=4, col=1:4, bty='n')

  plot(f, lwd=4, xlab="Time (years)", ylab="Stroke Probability", col=1:4, main="", xaxt='n', ylim=c(0.8,1), cex.lab = 1.5)
  mtext(paste0("Women: Time to Stroke by ", lab), side = 3, line = 0, font=2, adj=0, cex=1)
  axis(1,at=seq(0,10,1))
  mtext(f.lab1, side = 3, line = -23, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab3, side = 3, line = -24, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab2, side = 3, line = -25, font=2, adj=0, col='blue', cex=.8)
  mtext(f.lab4, side = 3, line = -26, font=2, adj=0, col='blue', cex=.8)
  legend('bottomleft', paste0(sort(unique(flat.dat.km[flat.dat.km$SEX==2, vari]))), lwd=4, col=1:4, bty='n')
}


pdf(file = "./Project3/Reports/table_figures/Supp Figure 1a.pdf", width = 30, height = 15)
par(mar=c(5.1, 5.1, 4.1, 2.1))
par(mfrow=c(2, 4))
uni.km.plot("age.cat", "Age (>=median vs < median)")
uni.km.plot("DIABETES", "Diabetes status")
uni.km.plot("SYSBP.high", "Systolic BP (>=160 vs <160 mmmHg)")
uni.km.plot("BPMEDS", "Anti-hyper. meds use status")
dev.off()

pdf(file = "./Project3/Reports/table_figures/Supp Figure 1b.pdf", width = 30, height = 15)
par(mar=c(5.1, 5.1, 4.1, 2.1))
par(mfrow=c(2, 4))
uni.km.plot("PREVCHD", "Prevalent CHD status")
uni.km.plot("CURSMOKE", "Smoking status (current vs not-current)")
uni.km.plot("totchol.cat", "Serum total cholesterol status (>=median vs < median)")
uni.km.plot("BMI.cat", "BMI categories")
dev.off()

uni.km.plot("anymiss", "Missingness") #Not included in the report, it is not very meaningful or pertinent to analysis


##########################
# Supp Figure 2: Martingale analysis for functional form
#########################
#Continuous variables: AGE SYSBP TOTCHOL BMI
martingale.plot<- function(vari, lab){
  
  form <- as.formula("Surv(stroke.time, stroke.event) ~ 1")
  
  m <- coxph(form, data=flat.dat.complete[flat.dat.complete$SEX==1,])
  resid.m <- resid(m, type = "martingale")
  plot(flat.dat.complete[flat.dat.complete$SEX==1, vari], resid.m, xlab=lab, ylab="Martingale Residual", main = paste0("Men: Martingale Residual Plot for\n", lab), cex.main=1.5, cex.lab = 1.5)
  lines(lowess(flat.dat.complete[flat.dat.complete$SEX==1, vari], 
               resid.m, iter=0),lty=2, col=2, lwd=4)
  
  f <- coxph(form, data=flat.dat.complete[flat.dat.complete$SEX==2,])
  resid.f <- resid(f, type = "martingale")
  plot(flat.dat.complete[flat.dat.complete$SEX==2, vari], resid.f, xlab=lab, ylab="Martingale Residual", main = paste0("Women: Martingale Residual Plot for\n", lab), cex.main=1.5, cex.lab = 1.5)
  lines(lowess(flat.dat.complete[flat.dat.complete$SEX==2, vari], 
               resid.f, iter=0),lty=2, col=2, lwd=4)
}

pdf(file = "./Project3/Reports/table_figures/Supp Figure 2.pdf", width = 30, height = 15)
par(mar=c(5.1, 5.1, 4.1, 2.1))
par(mfrow=c(2,4))
martingale.plot("AGE", "Age at exam (years)")
martingale.plot("SYSBP", "Average systolic blood pressure (mmHg)") #This is being analyzed as binary anyways
martingale.plot("TOTCHOL", "Serum total cholesterol (mg/dL)")
martingale.plot("BMI", "Body Mass Index (kg/m^2)")
dev.off()


#Iteratively use AGE, SYSBP, TOTCHOL, BMI in the models below
mart.test.m <- c(-2*as.numeric(logLik(coxph(Surv(stroke.time, stroke.event)~BMI, data = flat.dat.complete[flat.dat.complete$SEX==1,]))),
              -2*as.numeric(logLik(coxph(Surv(stroke.time, stroke.event)~BMI+I(BMI^2), data = flat.dat.complete[flat.dat.complete$SEX==1,]))))
1-pchisq(mart.test.m[1] - mart.test.m[2],1)

mart.test.f <- c(-2*as.numeric(logLik(coxph(Surv(stroke.time, stroke.event)~BMI, data = flat.dat.complete[flat.dat.complete$SEX==2,]))),
               -2*as.numeric(logLik(coxph(Surv(stroke.time, stroke.event)~BMI+I(BMI^2), data = flat.dat.complete[flat.dat.complete$SEX==2,]))))
1-pchisq(mart.test.f[1] - mart.test.f[2],1)

#Alternative formulations of interaction terms in R
#coxph(Surv(stroke.time, stroke.event)~BMI+I(BMI^2), data = flat.dat.complete[flat.dat.complete$SEX==2,])
#coxph(Surv(stroke.time, stroke.event)~poly(BMI,2, raw = T), data = flat.dat.complete[flat.dat.complete$SEX==2,])

#Martingale did not reveal any major non-linearity, at least none that fits a quadratic term
#Other non-linear approaches, e.g. restricted splines may be a better fit, but not pursuing that here
#Besides, the very obvious one is systolic BP which is already being analyzed as binary at a similar threshold


##########################
# Primary analysis: Build the final model (use model/selection data, then use data with non-missing values for final variables in the model)
#########################
flat.dat.comp2 <- flat.dat.complete[,c("RANDID", "stroke.time", "stroke.event", "SEX", "AGE", "DIABETES", "SYSBP.high", "BPMEDS", "PREVCHD", "CURSMOKE", "TOTCHOL", "BMI")]

#backwards selection, then test for interaction (knowing that the number of events will prevent a very robust model)

#Full model for males and females
male.full.model <-   coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + BPMEDS + PREVCHD + CURSMOKE + TOTCHOL + BMI, data = flat.dat.comp2[flat.dat.comp2$SEX==1, ])
female.full.model <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + BPMEDS + PREVCHD + CURSMOKE + TOTCHOL + BMI, data = flat.dat.comp2[flat.dat.comp2$SEX==2, ])

# Perform backward selection
male_backward <- stepAIC(male.full.model, direction = "backward", trace = 0)
summary(male_backward)$coefficients
names(male_backward$coefficients) #Variables in the final backward model for males

female_backward <- stepAIC(female.full.model, direction = "backward", trace = 0)
summary(female_backward)$coefficients
names(female_backward$coefficients) #Variables in the final backward model for females

##########################
##Functions to assess interaction between a candidate variable and variables already in the model indicated above
#########################

#Function for males
#Modified function from Microsoft Copilot
male.interaction <- function(var, data) {
  
  # Base model
  m0 <- as.formula("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + CURSMOKE")
  mf0 <- coxph(m0, data = data)
  
  # Main-effects
  m1 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES+ SYSBP.high + CURSMOKE +", var))
  mf1 <- coxph(m1, data = data)
  
  # Interaction with AGE
  m2 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE * ", var, " + DIABETES + SYSBP.high + CURSMOKE"))
  mf2 <- coxph(m2, data = data)
  
  # Interaction with DIABETES
  m3 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ DIABETES * ", var, " + AGE + SYSBP.high + CURSMOKE"))
  mf3 <- coxph(m3, data = data)
  
  # Interaction with SYSBP
  m4 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ SYSBP.high * ", var, " + AGE + DIABETES + CURSMOKE"))
  mf4 <- coxph(m4, data = data)
  
  # Interaction with CURSMOKE
  m5 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ CURSMOKE * ", var, " + AGE + DIABETES + SYSBP.high"))
  mf5 <- coxph(m5, data = data)
  
  
  # Extract -2 log-likelihoods
  res <- data.frame(
    model = c(
      "Baseline (AGE + DIABETES + SYSBP.high + CURSMOKE)",
      paste0("Main effects (+ ", var, ")"),
      paste0("AGE:", var, " int."),
      paste0("DIABETES:", var, " int."),
      paste0("SYSBP.high:", var, " int."),
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
m1 <- male.interaction("BPMEDS", flat.dat.comp2[flat.dat.comp2$SEX==1, ]) #complaints about convergence for one of the component models
m2 <- male.interaction("PREVCHD", flat.dat.comp2[flat.dat.comp2$SEX==1, ]) #complaints about convergence for one of the component models
m3 <- male.interaction("TOTCHOL", flat.dat.comp2[flat.dat.comp2$SEX==1, ])
m4 <- male.interaction("BMI", flat.dat.comp2[flat.dat.comp2$SEX==1, ])

rbind(m1, m2, m3, m4)

#Function for females, even though backwards selection identified AGE and SYSBP only, DIABETES is added because this was specified by the PI
female.interaction <- function(var, data) {
  
  # Base model
  m0 <- as.formula("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + BPMEDS")
  mf0 <- coxph(m0, data = data)
  
  # Main-effects
  m1 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + BPMEDS +", var))
  mf1 <- coxph(m1, data = data)
  
  # Interaction with AGE
  m2 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ AGE * ", var, " + DIABETES + SYSBP.high + BPMEDS"))
  mf2 <- coxph(m2, data = data)
  
  # Interaction with DIABETES
  m3 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ DIABETES * ", var, " + AGE + SYSBP.high + BPMEDS"))
  mf3 <- coxph(m3, data = data)
  
  # Interaction with SYSBP
  m4 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ SYSBP.high * ", var, " + AGE + DIABETES + BPMEDS"))
  mf4 <- coxph(m4, data = data)
  
  # Interaction with SYSBP
  m5 <- as.formula(paste0("Surv(stroke.time, stroke.event) ~ BPMEDS * ", var, " + AGE + DIABETES + SYSBP.high"))
  mf5 <- coxph(m5, data = data)
  
  # Extract -2 log-likelihoods
  res <- data.frame(
    model = c(
      "Baseline (AGE + DIABETES + SYSBP.high + BPMEDS)",
      paste0("Main effects (+ ", var, ")"),
      paste0("AGE:", var, " int."),
      paste0("DIABETES:", var, " int."),
      paste0("SYSBP.high:", var, " int."),
      paste0("BPMEDS:", var, " int.")
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

f1 <- female.interaction("PREVCHD", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f2 <- female.interaction("CURSMOKE", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f3 <- female.interaction("TOTCHOL", flat.dat.comp2[flat.dat.comp2$SEX==2, ])
f4 <- female.interaction("BMI", flat.dat.comp2[flat.dat.comp2$SEX==2, ])

rbind(f1, f2, f3, f4)

#Save interaction assessment models
int.models<- rbind(cbind(Sex = "Male", rbind(m1, m2, m3, m4)),
                   cbind(Sex = "Female", rbind(f1, f2, f3, f4)))
int.models <- flextable(int.models)
int.models <- autofit(int.models)
save_as_docx(int.models, path = "./Project3/Reports/table_figures/Supp. Table 2.docx")


##########################
# Final model
#########################
#The final model for males comprises: AGE + DIABETES + SYSBP * BPMEDS
#The final model for females comprises: DIABETES + SYSBP + AGE * TOTCHOL

#Define final data for men and women based on the variables that are included in the final model
flat.dat$anymiss.male <-   rowSums(is.na(flat.dat[,c("AGE", "DIABETES", "SYSBP.high", "BPMEDS")]))
flat.dat$anymiss.female <- rowSums(is.na(flat.dat[,c("AGE", "DIABETES", "SYSBP.high", "TOTCHOL", "BPMEDS")]))

flat.dat.male <- flat.dat[flat.dat$SEX==1 & flat.dat$anymiss.male==0,] #Final data containing non-missing values for the 4 variables included in the final model for males
flat.dat.female <- flat.dat[flat.dat$SEX==2 & flat.dat$anymiss.female==0,] #Final data containing non-missing values for the 4 variables included in the final model for females


male.final.model <- coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high * BPMEDS, data = flat.dat.male)
summary(male.final.model)
cox.zph(male.final.model)
plot(cox.zph(male.final.model))

m.model.out <- tbl_regression(male.final.model, 
                              exponentiate = TRUE,
                              estimate_fun = function(x) style_number(x, digits = 3),
                              pvalue_fun = function(x) style_pvalue(x, digits = 3)
                              )
#Table 2, part a
m.model.out %>%
  as_gt() %>%
  gt::gtsave(filename = "./Project3/Reports/table_figures/Table 2a.docx")

#female.final.model <- coxph(Surv(stroke.time, stroke.event) ~ DIABETES + SYSBP.high + AGE * TOTCHOL, data = flat.dat.female) #Initial model before binarizing SYSBP
female.final.model <- coxph(Surv(stroke.time, stroke.event) ~ DIABETES + SYSBP.high + BPMEDS + AGE * TOTCHOL, data = flat.dat.female)
summary(female.final.model)
cox.zph(female.final.model)
plot(cox.zph(female.final.model))

f.model.out <- tbl_regression(female.final.model, 
                              exponentiate = TRUE,
                              estimate_fun = function(x) style_number(x, digits = 3),
                              pvalue_fun = function(x) style_pvalue(x, digits = 3)
                              )
#Table 2, part b
f.model.out %>%
  as_gt() %>%
  gt::gtsave(filename = "./Project3/Reports/table_figures/Table 2b.docx")



#Risk profiles and 10-year probability of stroke estimation
male.riskset <- rbind(expand.grid(AGE=c(40,50,60), DIABETES=c(0,1), SYSBP.high=c(0,1), BPMEDS=0),
                      expand.grid(AGE=c(40,50,60), DIABETES=c(1), SYSBP.high=c(1), BPMEDS=1))

female.riskset <-rbind(expand.grid(AGE=c(40,50,60), DIABETES=c(0,1), SYSBP.high=c(0,1), BPMEDS=0, TOTCHOL=as.numeric(quantile(flat.dat.female$TOTCHOL, probs = .1))),
                       expand.grid(AGE=c(40,50,60), DIABETES=c(1), SYSBP.high=c(1), BPMEDS=1, TOTCHOL=as.numeric(quantile(flat.dat.female$TOTCHOL, probs = .90))))

#Interaction terms should not (or need not) be included in the newdata dataframe
#https://www.rdocumentation.org/packages/survival/versions/2.36-5/topics/survfit.coxph

for (i in 1:nrow(male.riskset)) print(summary(survfit(male.final.model, newdata=male.riskset[i,], conf.type="log-log"), times=10))
for (i in 1:nrow(female.riskset)) print(summary(survfit(female.final.model, newdata=female.riskset[i,], conf.type="log-log"), times=10))

for (i in 1:nrow(male.riskset)) print(round(100*(1-as.numeric(summary(survfit(male.final.model, newdata=male.riskset[i,], conf.type="log-log"), times=10)[c("surv", "upper", "lower")])),1))
for (i in 1:nrow(female.riskset)) print(round(100*(1-as.numeric(summary(survfit(female.final.model, newdata=female.riskset[i,], conf.type="log-log"), times=10)[c("surv", "upper", "lower")])),1))

#Modified Copilot codes to tabulate the outputs from above
get_10yr_risk <- function(fit, newdata_row) {
  s <- summary(
    survfit(fit, newdata = newdata_row, conf.type = "log-log"),
    times = 10
  )
  
  # Event probability = 1 - survival
  est <- round(100 * (1 - s$surv), 1)
  lo  <- round(100 * (1 - s$upper), 1)
  hi  <- round(100 * (1 - s$lower), 1)
  
  paste0(est, "% (", lo, ", ", hi, ")")
}


build_table <- function(fit, riskset) {
  
  ages <- c(40, 50, 60)
  n_age <- length(ages)
  n_profiles <- nrow(riskset) / n_age
  
  # Step 1: compute all risks in row order
  risks <- character(nrow(riskset))
  for (i in seq_len(nrow(riskset))) {
    risks[i] <- get_10yr_risk(fit, riskset[i, , drop = FALSE])
  }
  
  # Step 2: reshape into wide format
  mat <- matrix(
    risks,
    nrow = n_profiles,
    ncol = n_age,
    byrow = TRUE
  )
  
  colnames(mat) <- paste0("Age ", ages)
  
  as.data.frame(mat, stringsAsFactors = FALSE)
}

risk.profile.names <- c("Baseline", "Diabetes only", "High BP", "Diabetes + High BP", "My profile")

male.table   <- build_table(male.final.model, male.riskset)
female.table <- build_table(female.final.model, female.riskset)

rownames(male.table)   <- risk.profile.names
rownames(female.table) <- risk.profile.names

male.table$profile <- rownames(male.table)
female.table$profile <- rownames(female.table)

final.table <- rbind(
  cbind(Sex = "Male",   male.table),
  cbind(Sex = "Female", female.table)
)
final.table <- flextable(final.table)
#final.table <- autofit(final.table)
save_as_docx(final.table, path = "./Project3/Reports/table_figures/Table 3.docx")



##########################
# Secondary analysis of time-varying status of Diabetes and SYSBP
#########################
#Perfect alignment between SYSBP data availability and follow-up period! This means these variables were measured at each follow-up period
#This variable is indeed of importance to the Framingham study team!
table(flat.dat$period.ltfu123, flat.dat$sysbp.miss.fu) 
table(flat.dat$period.ltfu123, flat.dat$diabetes.miss.fu)

#Number of men and women included in this analysis
with(flat.dat[flat.dat$period.ltfu123==0,], (table(SEX)))


#Crude transition probabilities
diab.12.m <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
                  prop.table(table(DIABETES.1, DIABETES.2),1)), 2)

diab.23.m <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
                  prop.table(table(DIABETES.2, DIABETES.3),1)), 2)

diab.12.f <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
                  prop.table(table(DIABETES.1, DIABETES.2),1)), 2)

diab.23.f <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
                  prop.table(table(DIABETES.2, DIABETES.3),1)), 2)

diabetes.T.matrices <- list(
  "Men: P1 to P2"   = diab.12.m,
  "Men: P2 to P3"   = diab.23.m,
  "Women: P1 to P2" = diab.12.f,
  "Women: P2 to P3" = diab.23.f)
diabetes.T.matrices

with(flat.dat[flat.dat$period.ltfu123==0,], prop.table(table(DIABETES.3, SEX),2)) #Prevalence of diabetes at period 3


sysb.12.m <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
                  prop.table(table(SYSBP.high.p1, SYSBP.high.p2),1)), 2)

sysb.23.m <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
                  prop.table(table(SYSBP.high.p2, SYSBP.high.p3),1)), 2)

sysb.12.f <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
                  prop.table(table(SYSBP.high.p1, SYSBP.high.p2),1)), 2)

sysb.23.f <- round(with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
                  prop.table(table(SYSBP.high.p2, SYSBP.high.p3),1)), 2)


systolic.T.matrices <- list(
  "Men: P1 to P2"   = sysb.12.m,
  "Men: P2 to P3"   = sysb.23.m,
  "Women: P1 to P2" = sysb.12.f,
  "Women: P2 to P3" = sysb.23.f)
systolic.T.matrices

with(flat.dat[flat.dat$period.ltfu123==0,], prop.table(table(SYSBP.high.p3, SEX),2)) #Prevalence of high sysbp at period 3



#Scatter plot of systolic BP as a continuous marker showing change from baseline to year 2, and from year 2 to year 3
pdf(file = "./Project3/Reports/table_figures/Figure 2.pdf", width = 20, height = 17)
par(mar=c(5.1, 5.1, 4.1, 2.1))
par(mfrow=c(2,2))
#Males
with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
     plot(SYSBP.1, SYSBP.diff.12, xlab = "Baseline systolic BP (mmHg)", ylab = "Change in systolic BP (mmHg)", pch=16, xlim=c(80, 270), ylim=c(-80, 120), axes=F,
          cex.lab=1.5, main="Males: Baseline vs Change in Systolic BP (Period 1 to 2)"))
axis(1, at=seq(80,270,20)); axis(2, at=seq(-80,120, 20))
abline(h = 0, lty = 2, lwd=4, col = "blue")      # no change
abline(v = 160, lty = 2, lwd=4, col = "firebrick") # clinical threshold (optional)

with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==1,],
     plot(SYSBP.2, SYSBP.diff.23, xlab = "2nd Follow-up systolic BP (mmHg)", ylab = "Change in systolic BP (mmHg)", pch=16, xlim=c(80, 270), ylim=c(-80, 120), axes=F,
          cex.lab=1.5, main="Males: 2nd Follow-up vs Change in Systolic BP (Period 2 to 3)"))
axis(1, at=seq(80,270,20)); axis(2, at=seq(-80,120, 20))
abline(h = 0, lty = 2, lwd=4, col = "blue")      # no change
abline(v = 160, lty = 2, lwd=4, col = "firebrick") # clinical threshold (optional)

#Females
with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
     plot(SYSBP.1, SYSBP.diff.12, xlab = "Baseline systolic BP (mmHg)", ylab = "Change in systolic BP (mmHg)", pch=16, xlim=c(80, 270), ylim=c(-80, 120), axes=F,
          cex.lab=1.5, main="Females: Baseline vs Change in Systolic BP (Period 1 to 2)"))
axis(1, at=seq(80,270,20)); axis(2, at=seq(-80,120, 20))
abline(h = 0, lty = 2, lwd=4, col = "blue")      # no change
abline(v = 160, lty = 2, lwd=4, col = "firebrick") # clinical threshold (optional)

with(flat.dat[flat.dat$period.ltfu123==0 & flat.dat$SEX==2,],
     plot(SYSBP.2, SYSBP.diff.23, xlab = "2nd Follow-up systolic BP (mmHg)", ylab = "Change in systolic BP (mmHg)", pch=16, xlim=c(80, 270), ylim=c(-80, 120), axes=F,
          cex.lab=1.5, main="Females: 2nd Follow-up vs Change in Systolic BP (Period 2 to 3)"))
axis(1, at=seq(80,270,20)); axis(2, at=seq(-80,120, 20))
abline(h = 0, lty = 2, lwd=4, col = "blue")      # no change
abline(v = 160, lty = 2, lwd=4, col = "firebrick") # clinical threshold (optional)
dev.off()

