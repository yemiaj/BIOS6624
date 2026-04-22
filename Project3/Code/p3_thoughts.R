#Notes from class, 04/01/2026
Interested variables: age, diabetes, and BP (systolic) (want for sure to be in the model)
Not sure if should be included: prevalent CHD, BP meds (anti-hypertensive meds), smoking status (current or not), total cholesterol, and BMI
Interested in the baseline version of these.
No, dont look at competing risk (you can comment on that as limitations)
Truncate data at 10 years (not interested in time beyond that)
Not interested in events outside stroke
Only fixed covariates only, not time-varying covariates
Descriptive of whether variables are changing over time, which will impact decision of whether future analysis should be time-varying
Descriptive on whether diabetes and systolic BP changes as time-varying, not others
Separate models by gender, interested in factors of stroke for both sexes
Exclude those who have had stroke at baseline

#Notes from class on 04/06 and 04/08
The variables in both models may be the same, but if theyre different you may need to explain
Clarify that the purpose of the model is variable selection, vs prediction
As much n in the model as possible
Estimates for 10 year survival for ages 40, 50, 60; at 0 risk factors, diabetes, high BP, high BP & diabetes + your own scenario (5 scenarios)
	baseline, diabetes, and someone with 0 risk factors, and someone with high BP (>160 systolic), someone with diabetes, high BP & diabetes, + additional profile you think is important.
In coming up with baseline risk profile, average out the continuous.




Good to dos and analysis plan (see Preliminary Analysis Plan document too):
KM plot of time to stroke by risk factors
Work on a flow chart diagram, see past notes and feedback on difference between exclusion and missingness
Plot hazard
Univariable analysis with lograngk test
Asses for non-PH in univariable, if non-PH the decision to make change would depend on how much skewed things are or how early or late the plots cross
Assess functional form in univariable
Could use AFT
Table of events by levels of each predictor
Regardless of which variable selection you use, notice its pitfall in the limitations
4 by 4 for change overtime, including NA. And the cell will be proportion of cases in the category (probability transition matrix)
Complete case for all variables that pass the first screen (logrank test), and then final analysis will bring back those with non-missing. 
Collapsing variables
Univariable analysis will have KM plot, Cox PH HR, log-rank test statistic, Schoenfeld test p-value
Other model fit statistics

Median and quartile follow-up estimates
Censored rate
Some form of best subset model
#Include number of events in Table 1
Is there difference in survival by sex? Before and after 10 year cutoff?

Use more than 1 method for CPH test, influenced by events and sample size 
Describe the profiles, indicate who has higher probability than the other





#Questions to ask during office hours or on Canvas
Generally, can we go beyond the 3+5 variables of interest?
  Can we combine CURSMOKE and CIGPDAY variables? CIGPDAY is a continuous variable.
Also, for modelling only (not time varying descriptive analysis), can we use blood glucose (GLUCOSE) instead of DIABETES, although the 10-year probability estimate for those with diabetes will be challenging if this is done.
Should we consider 'educ' variable? Variable description details is not the data dictionary for this variable and we don't know what the 4 levels of this variable mean.
	Should diastolic BP (DIAB), heart rate (HEARTRTE) be considered? What about prevalent angina (PREVAP), myocardial infarction (PREVMI), hypertensive (PREVHYP)? How about HDLC and LDLC, although these 2 variables will be strongly correlated with  total cholesterol (TOTCHOL)?
In the project description on Canvas, you wrote that "Your analysis should only be conducted using the first 10 years of follow-up." And this is implemented as recoding FU longer than 10years to 10 years with censoring and event status coded appropriately. Is this exactly what is intended or we can use the full follow-up data but only get 10-year survival probability at 10-years?

Response (from attending office hours on 04/10)
1)Focus on binary variable, CURSMOKE/CIGPDAY. Same applies to diabetes/glucose
2)Do not consider other variables. Just the 3+5 specified
3)Code the FU time using the ifelse() function where times higher than 10years is coded back to 10 years. Code the events accordingly.
'
























###############################################################################################
#
# Rough codes and thoughts moved over from p3_clean_and_analyses.R
#
##############################################################################################

#Delete after here

diab.23 <- with(flat.dat[flat.dat$period.ltfu123==0,],
                prop.table(table(DIABETES.2, DIABETES.3),1)
                ) 
  
  prop.table(table(flat.dat$DIABETES.1, flat.dat$DIABETES.2), 1)


prop.table(table(flat.dat$DIABETES.2, flat.dat$DIABETES.3), 1)


table(flat.dat$PERIOD.1, exclude=NULL)
table(flat.dat$PERIOD.2, exclude=NULL)
table(flat.dat$PERIOD.3, exclude=NULL)

table(flat.dat$PERIOD.2, flat.dat$PERIOD.3, flat.dat$SEX, exclude=NULL)




1-as.numeric(summary(survfit(male.final.model, newdata=p1[1,]), times=10)[c("surv", "upper", "lower")])

summary(survfit(male.final.model, newdata=p1[1,]), times=10)









summary(coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high * BPMEDS, data = flat.dat.comp2[flat.dat.comp2$SEX==1, ]))
a<-coxph(Surv(stroke.time, stroke.event) ~ AGE + DIABETES + SYSBP.high + BPMEDS, data = flat.dat.comp2[flat.dat.comp2$SEX==2, ])
b<-coxph(Surv(stroke.time, stroke.event) ~ AGE*TOTCHOL + DIABETES + SYSBP.high + BPMEDS, data = flat.dat.comp2[flat.dat.comp2$SEX==2, ])
summary(a)
summary(b)

1-pchisq(
  -2*(as.numeric(logLik(a)) - as.numeric(logLik(b))), 2
  )

DIABETES + SYSBP + AGE * TOTCHOL




#Reproduce the p-values in Table 1
#Reason for backwards
#Multiple testing as limitation
#Proximity of collection of risk factors
#Limited number of events, and secondly I could not check simultaneous pairwise comparisons because the full model will require 21 variables
#Interpet coefficients on the HR and percent scale
#risk profiles as a table
#grpahical log-log analysis of PH test
#BMI is better as continuous, SYSBP is categorical because of model specifics

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
#Talk about tie handling
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