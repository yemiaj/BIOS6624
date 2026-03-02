#######################

#Codes related to the cleaning and preliminary analysis of Project 1: Multicenter AIDS Cohort Study

######################
library(gtsummary) #Needed for creating publication-ready tables
library(psych) #Needed for correlation matrix plot
library(car) #Needed to calculated variance inflation factor (VIF) from a multivariable linear model

library(cmdstanr)
library(bayesplot)  # diagnostic plots of the MCMC chains
library(posterior)  # for summarizing posterior draws
library(bayestestR) # for calculating higest density posterior intervals
library(mcmcse)     # for calculating MCMCSE's
library(loo)        # for getting model fit statistics (WAIC and LOO-IC)
library(dplyr)      # data manipulation tricks
library(tibble)     # for fancy data frames

library(knitr)      # For kable table

# Import .csv file for Project 1
hiv.dat0 <- read.csv('./Project1/DataRaw/hiv_6624_final.csv') 

# See variable types and values
str(hiv.dat0) 
#None of these variables is character/factor

#Create log10 transformed VLOAD, and also keep the original version.
hiv.dat0$lg.VLOAD <- log10(hiv.dat0$VLOAD)

# Select variables of interest to this project, and also limit the data to years of interest, i.e. year=0 and year=2
hiv.dat <- hiv.dat0[hiv.dat0$years %in% c(0,2), c("newid", "years", "AGG_MENT", "AGG_PHYS", "LEU3N", "VLOAD","lg.VLOAD", 
                                                  "hard_drugs","ADH", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")]

# Labels of variables of interest in this analysis. This was copied from the codebook provided and found on Canvas.
# 
# newid: deidentified ID
# years: years since initiating ART | 0=baseline visit (before ART), 1=1 year, ..., 8=8years, ''=Missing
# 
# AGG_MENT: SF36 MCS score 
# AGG_PHYS: SF36 PCS score 
# LEU3N: Number of CD4 positive cells (helpers), (in cells)
# VLOAD: Standardized viral load, (in copies/ml)
# 
# hard_drugs: Hard drug use (either injection drugs or illicit heroin/opiate use) since last visit | 0=No, 1=Yes, ''=Missing
# ADH: Adherence to meds taken since last visit | 1=100%, 2=95-99%, 3=75-94%, 4= <75%, ''=Missing
# 
# BMI: Body Mass Index (in kg/meter**2) based on earliest non-missing height
# RACE: Race | 1=white nH, 2=White H, 3=Black nH, 4=Black H, 5=AIAN, 6=Asian or PI, 7=Other, 8=Other Hispanic, ''=Missing
# EDUCBAS: Baseline or earliest reported education (highest grade or level) | 1=8th grade or less, 2=9 to 11th grade, 3=12th grade, 
          # 4=at least 1 year college (but no degree), 5=four years college (got degree), 6=some graduate work, 7=post-graduate degree, ''=missing
# age: Age at visit 
# SMOKE: Smoking status | 1=Never smoked,2=Former smoker, 3=Current smoker, ''=Missing

#Assign and label levels for categorical variables
hiv.dat$hard_drugs <- factor(hiv.dat$hard_drugs, levels = c(0, 1), labels = c("No", "Yes"))
hiv.dat$RACE <- factor(hiv.dat$RACE, levels = c(1, 2, 3, 4, 7, 8), labels = c("White, non-Hispanic", "White, Hispanic", "Black, non-Hispanic",
                                                                                  "Black, Hispanic", "Other", "Other Hispanic"))
hiv.dat$EDUCBAS <- factor(hiv.dat$EDUCBAS, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("8th grade or less", "9th to 11th grade", "12th grade",
                                                                                           "1+ year college (but no degree)", "Four years college", 
                                                                                           "Some graduate work", "Post-graduate degree"))
hiv.dat$SMOKE <- factor(hiv.dat$SMOKE, levels = c(1, 2, 3), labels = c("Never smoked", "Former smoker", "Current smoker"))
hiv.dat$ADH <- factor(hiv.dat$ADH, levels = c(1, 2, 3, 4), labels = c("100%", "95 - 99%", "75 - 94%", "<75%"))


# Separate out year0 and year2
hiv.dat.y0 <- hiv.dat[hiv.dat$years==0, ]
hiv.dat.y2 <- hiv.dat[hiv.dat$years==2, ]

table(duplicated(hiv.dat.y0$newid)) #Year 0 data is flat and consists of 715 unique observations
table(duplicated(hiv.dat.y2$newid)) #Year 2 data is flat and consists of 506 unique observations

#Rename the variables in the hiv.dat.y2 dataframe and add a suffix of ".2" to the variable names to differentiate from the same variables found in hiv.dat.y0
names(hiv.dat.y2)[-1] <- paste0(names(hiv.dat.y2), '.', 2)[-1]
head(hiv.dat.y2)

#Merge year0 and year2, and keep all rows of data, including those in year0 but not year2
hiv.dat3 <- merge(hiv.dat.y0, hiv.dat.y2, by.x = 'newid', all.x = TRUE, all.y = TRUE)

#Because 'years' is a variable without NAs in the earlier hiv.dat data frame, values of year.2 == NA can be used as indicator for lost to follow-up
#table(hiv.dat$years, exclude=NULL)
table(hiv.dat3$years, hiv.dat3$years.2, exclude=NULL) #
hiv.dat3$lost.fu <- ifelse(is.na(hiv.dat3$years.2), 1, 0) #1 == Yes, lost to follow-up
hiv.dat3$lost.fu <- factor(hiv.dat3$lost.fu, levels = c(0, 1), labels = c("No", "Yes")) #Create factor variable and label the catergories


#######################
# Data cleaning ####
######################

# Select needed variables so that complete.cases() function does not exclude needed rows when unneeded variable with missing is included
hiv.dat4 <- hiv.dat3[!is.na(hiv.dat3$years.2) & hiv.dat3$years.2==2, 
                     c("newid", "AGG_PHYS", "AGG_PHYS.2", "AGG_MENT", "AGG_MENT.2", "LEU3N", "LEU3N.2", "lg.VLOAD", "lg.VLOAD.2",
                       "hard_drugs", "ADH.2", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")]

#Rename some of the variables 
names(hiv.dat4)[-1] <- c("phy.qol", "phy.qol.y2", "ment.qol", "ment.qol.y2", "cd4.count", "cd4.count.y2", "lg10.vload", "lg10.vload.y2", 
                         "drug.use", "adhere.y2", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")

#Recode factor to numeric variables again
hiv.dat4$hd.use <- ifelse(hiv.dat4$drug.use == "Yes", 1, 0)
hiv.dat4$adh.y2 <- ifelse(hiv.dat4$adhere.y2 %in% c("95 - 99%", "100%"), 1, 0)
#The limit of plausible values for BMI used below was obtained from the codebook (it is appropriate to use this here and excludes -1(n=7), 514(n=1), and 999 (n=5))
hiv.dat4$bmi <- ifelse(hiv.dat4$BMI >= 10.8 & hiv.dat4$BMI <= 70.1, hiv.dat4$BMI, NA) 
hiv.dat4$white.nh <- ifelse(hiv.dat4$RACE == "White, non-Hispanic", 1, 0)
hiv.dat4$college <- ifelse(hiv.dat4$EDUCBAS %in% c("Four years college", "Some graduate work", "Post-graduate degree"), 1, 0) 
hiv.dat4$curnt.smkr <- ifelse(hiv.dat4$SMOKE == "Current smoker", 1, 0) 

#Select needed variables, and newly created binary ones
hiv.dat5 <- hiv.dat4[, c("newid", "phy.qol", "phy.qol.y2", "ment.qol", "ment.qol.y2", "cd4.count", "cd4.count.y2", "lg10.vload", "lg10.vload.y2", 
                         "bmi", "age", "hd.use", "adh.y2", "white.nh", "college", "curnt.smkr")]

#Create a variable that sums the rows of NAs as a new variable
hiv.dat5$sum.miss <- rowSums(is.na(hiv.dat5))

#See the observations with NAs 
hiv.dat5[hiv.dat5$sum.miss>0, ]

#See them (observations with NAs ) in the more raw/earlier data version
hiv.dat4[hiv.dat4$newid %in% hiv.dat5[hiv.dat5$sum.miss>0, ]$newid, ]

#Complete case and final data for analysis 
hiv.dat6 <- hiv.dat5[complete.cases(hiv.dat5), ]
hiv.dat6 <- hiv.dat6[, -ncol(hiv.dat6)] #Remove unneeded sum.miss variable

#Create and store final clean dataset
write.csv(hiv.dat6, "./Project1/DataProcessed/hiv_6624_clean.csv", row.names = FALSE)
