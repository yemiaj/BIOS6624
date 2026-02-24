#######################

#Codes related to the cleaning and preliminary analysis of Project 1: Multicenter AIDS Cohort Study

######################
library(gtsummary)

# Import .csv file for Project 1
hiv.dat0 <- read.csv('./Project1/DataRaw/hiv_6624_final.csv') 

# See variable types and values
str(hiv.dat0) 
#None of these variables is character/factor

#Create log10 transformed VLOAD, and keep the original version.
hiv.dat0$lg.VLOAD <- log10(hiv.dat0$VLOAD)

# Select variables of interest to this project, and at the same time limit data to years of interest, i.e. year=0 and year=2
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
hiv.dat3$lost.fu <- factor(hiv.dat3$lost.fu, levels = c(0, 1), labels = c("No", "Yes"))

#No character variable among these list of variables, so NAs will behave as expected 
str(hiv.dat3)



#Work on Table 1, Figure 1, and other descriptives
with(hiv.dat[hiv.dat$years==0,], plot(LEU3N, VLOAD))
with(hiv.dat[hiv.dat$years==0,], plot(AGG_MENT, AGG_PHYS))
summary(lm(AGG_MENT ~ AGG_PHYS, data=hiv.dat[hiv.dat$years==0,]))
#Box plot or histogram of all outcomes



#Create Table 1
#Credits & excellent resource: https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
tab1 <- hiv.dat3[hiv.dat3$years==0, ] |> #Select for year 0 in the merged year0 and year2 data
  
  tbl_summary(by = hard_drugs,
              
              include = c("AGG_MENT", "AGG_PHYS", "LEU3N", "VLOAD", "lg.VLOAD", "BMI", "age", "RACE", "EDUCBAS", "SMOKE", "lost.fu"),
              
              label = list(AGG_MENT ~ "Aggregate Mental Quality of Life Score", AGG_PHYS ~ "Aggregate Physical Quality of Life Score", LEU3N ~ "CD4+ T cell count",
                           VLOAD ~ "Standardized Viral Load (HIV copies per mL of blood)", lg.VLOAD ~ "Log10 Standardized Viral Load (HIV copies per mL of blood)", 
                           BMI ~ "Body Mass Index (kg/m^2)", age ~ "Age at visit (years)", RACE ~ "Race, Ethinicity Category", EDUCBAS ~ "Highest Level of Education", 
                           SMOKE ~ "Smoking Status", lost.fu ~ "Lost to follow-up (between Year 0 and 2)"),
              
              type = list(c("AGG_MENT", "AGG_PHYS", "LEU3N", "VLOAD", "lg.VLOAD", "BMI", "age") ~ "continuous",
                         c("RACE", "EDUCBAS", "SMOKE", "lost.fu") ~ "categorical"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),

              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 0),
              #missing="ifany",
              missing_text = "NA (missing)",
              missing_stat = "{N_miss} ({p_miss}%)") |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) |> #Argument within add_p() is needed for exact test to run without an error message
  add_overall(last=TRUE)
tab1

#Add headers and footnote as appropriate
#Add difference in percentages/means (not necessary)

tab2 <- hiv.dat3[!is.na(hiv.dat3$years.2) & hiv.dat3$years.2==2, ] |> #Select for year 2 in the merged year0 and year2 data
  
  tbl_summary(by = hard_drugs,
              
              include = c("AGG_MENT.2", "AGG_PHYS.2", "LEU3N.2", "VLOAD.2", "lg.VLOAD.2", "BMI.2", "age.2", "RACE.2", "EDUCBAS.2", "SMOKE.2", "ADH.2"),
              
              label = list(AGG_MENT.2 ~ "Aggregate Mental Quality of Life Score", AGG_PHYS.2 ~ "Aggregate Physical Quality of Life Score", LEU3N.2 ~ "CD4+ T cell count", 
                           VLOAD.2 ~ "Standardized Viral Load (HIV copies per mL of blood)", lg.VLOAD.2 ~ "Log10 Standardized Viral Load (HIV copies per mL of blood)",
                           BMI.2 ~ "Body Mass Index (kg/m^2)", age.2 ~ "Age at visit (years)", RACE.2 ~ "Race, Ethinicity Category", EDUCBAS.2 ~ "Highest Level of Education", 
                           SMOKE.2 ~ "Smoking Status", ADH.2 ~ "Adherence to meds taken since last visit"),
              
              type = list(c("AGG_MENT.2", "AGG_PHYS.2", "LEU3N.2", "VLOAD.2", "lg.VLOAD.2", "BMI.2", "age.2") ~ "continuous",
                          c("RACE.2", "EDUCBAS.2", "SMOKE.2", "ADH.2") ~ "categorical"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              
              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 0),
              #missing="ifany",
              missing_text = "NA (missing)",
              missing_stat = "{N_miss} ({p_miss}%)") |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) |>
  add_overall(last=TRUE)
tab2


#For both gtsummary objects to merge properly into a single table, rename the variable name in tab2 so all the variables have a unique name
#Use a duplicate of tab2 for this task
tab2b <- tab2
tab2b$table_body$variable <- substr(tab2b$table_body$variable, 1, nchar(tab2b$table_body$variable) - 2)

tab1.full <- tbl_merge(tbls = list(tab1, tab2b), tab_spanner = c("**Baseline**", "**Follow-up (Year 2)**"))
#Note that NA (missing) is split for smoking status and pasted into lost to FU
#Make the ltfu variable work for year=2 too


#Data cleaning
#Select needed variables so that complete case function does not exclude needed rows when unneeded variable with missing is included
hiv.dat4 <- hiv.dat3[!is.na(hiv.dat3$years.2) & hiv.dat3$years.2==2, 
                     c("newid", "AGG_PHYS", "AGG_PHYS.2", "AGG_MENT", "AGG_MENT.2", "LEU3N", "LEU3N.2", "lg.VLOAD", "lg.VLOAD.2",
                       "hard_drugs", "ADH.2", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")]

names(hiv.dat4)[-1] <- c("phy.qol", "phy.qol.y2", "ment.qol", "ment.qol.y2", "cd4.count", "cd4.count.y2", "lg10.vload", "lg10.vload.y2", 
                         "drug.use", "adhere.y2", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")

hiv.dat4$adh.y2 <- ifelse(hiv.dat4$adhere.y2 <=2, 1, 0) #>=95% vs <95% 


# ADH: Adherence to meds taken since last visit | 1=100%, 2=95-99%, 3=75-94%, 4= <75%, ''=Missing


#The limit of plausible values for BMI used below was obtained from the codebook (it is appropriate here and excludes -1(n=7), 514(n=1), and 999 (n=5))
hiv.dat4$bmi <- ifelse(hiv.dat4$BMI >= 10.8 & hiv.dat4$BMI <= 70.1, hiv.dat4$BMI, NA) 

hiv.dat4$white.nh <- ifelse(hiv.dat4$RACE==1, 1, 0) 

hiv.dat4$college <- ifelse(hiv.dat4$EDUCBAS >= 5, 1, 0) 

hiv.dat4$frmr.smkr <- ifelse(hiv.dat4$SMOKE == 2, 1, 0) 
hiv.dat4$curnt.smkr <- ifelse(hiv.dat4$SMOKE == 3, 1, 0) 

hiv.dat4$adh2 <- ifelse(hiv.dat4$adhere.y2 == 2, 1, 0) 
hiv.dat4$adh3 <- ifelse(hiv.dat4$adhere.y2 == 3, 1, 0) 
hiv.dat4$adh4 <- ifelse(hiv.dat4$adhere.y2 == 4, 1, 0) 


#Preliminary models 
summary(fit0 <- lm(phy.qol.y2 ~ drug.use, hiv.dat4))
summary(fit1 <- lm(phy.qol.y2 ~ phy.qol + drug.use, hiv.dat4))
summary(fit2 <- lm(phy.qol.y2 ~ phy.qol + drug.use + adh2 + adh3 + adh4, hiv.dat4))

summary(fit0 <- lm(ment.qol.y2 ~ drug.use, hiv.dat4))
summary(fit1 <- lm(ment.qol.y2 ~ ment.qol + drug.use, hiv.dat4))
summary(fit2 <- lm(ment.qol.y2 ~ ment.qol + drug.use + adh2 + adh3 + adh4, hiv.dat4))

summary(fit0 <- lm(cd4.count.y2 ~ drug.use, hiv.dat4))
summary(fit1 <- lm(cd4.count.y2 ~ cd4.count + drug.use, hiv.dat4))
summary(fit2 <- lm(cd4.count.y2 ~ cd4.count + drug.use + adh2 + adh3 + adh4, hiv.dat4))

summary(fit0 <- lm(lg10.vload.y2 ~ drug.use, hiv.dat4))
summary(fit1 <- lm(lg10.vload.y2 ~ lg10.vload + drug.use, hiv.dat4))
summary(fit2 <- lm(lg10.vload.y2 ~ lg10.vload + drug.use + adh2 + adh3 + adh4, hiv.dat4))

#Complete case data


summary(fit3 <- lm(phy.qol.y2 ~ phy.qol + drug.use + age + bmi + white.nh + college + frmr.smkr + curnt.smkr + adh2 + adh3 + adh4, hiv.dat4))


 

#Keep a dataframe containing the original variable so you can describe the exclusions
# Work on NAs, 
# Review outliers and plausible values for continuous variables (ask instructor)
# Recode categorical variables as appropriate for analysis
# Apply transformations as appropriate
























#table & proportion of hard drug use over time
table(hiv.dat$hard_drugs, hiv.dat$years)
round(prop.table(table(hiv.dat$hard_drugs, hiv.dat$years), 2),3)

with(hiv.dat[hiv.dat$years %in% c(0,2),], table(hard_drugs, years, exclude=NULL))