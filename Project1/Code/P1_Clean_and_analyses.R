#######################

#Codes related to the cleaning and preliminary analysis of Project 1: Multicenter AIDS Cohort Study

######################
library(gtsummary)

# Import .csv file for Project 1
hiv.dat0 <- read.csv('./Project1/DataRaw/hiv_6624_final.csv') 

# See variable types and values
str(hiv.dat0) 

#Create log10 transformed VLOAD, and drop the original version.
hiv.dat0$lg.VLOAD <- log10(hiv.dat0$VLOAD)

# Select variables of interest to this project, and at the same time limit data to years of interest (year=0 and year=2)
hiv.dat <- hiv.dat0[hiv.dat0$years %in% c(0,2), c("newid", "years", "AGG_MENT", "AGG_PHYS", "LEU3N", "lg.VLOAD", 
                                                  "hard_drugs", "ADH", "BMI", "RACE", "EDUCBAS", "age", "SMOKE")]



# Labels of variables of interest in this analysis, copied from the codebook provided and found on Canvas.
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

# Separate out year0 and year2
hiv.dat.y0 <- hiv.dat[hiv.dat$years==0, ]
hiv.dat.y2 <- hiv.dat[hiv.dat$years==2, ]

#Rename the variables in the hiv.dat.y2 dataframe and add a suffix of ".2" to the variable names to differentiate from hiv.dat.y0
names(hiv.dat.y2)[-1] <- paste0(names(hiv.dat.y2), '.', 2)[-1]
#names(hiv.dat.y2) <- paste0(names(hiv.dat.y2), '.', 2)
head(hiv.dat.y2)

#Merge year0 and year2
hiv.dat3 <- merge(hiv.dat.y0, hiv.dat.y2, by.x = 'newid', all.x = TRUE, all.y = TRUE)

#Since 'years' is a variable without NAs in the earlier hiv.dat dataframe, values of year.2 == NA can be used as indicator for lost to follow-up
table(hiv.dat$years, exclude=NULL)
hiv.dat3$ltfu <- ifelse(is.na(hiv.dat3$years.2), 1, 0) #1 == Yes, lost to follow-up

#No character variable among these list of variables, so NAs will behave as expected 
str(hiv.dat3)

#Work on Table 1, Figure 1, and other descriptives
with(hiv.dat[hiv.dat$years==0,], plot(LEU3N, VLOAD))
with(hiv.dat[hiv.dat$years==0,], plot(AGG_MENT, AGG_PHYS))
summary(lm(AGG_MENT ~ AGG_PHYS, data=hiv.dat[hiv.dat$years==0,]))

#Create Table 1
#Credits & excellent resource: https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
tab1 <- hiv.dat3[hiv.dat3$years==0, ] |> #Select for year 0 in the merged year0 and year2 data (this code is redundant)
  
  tbl_summary(by = hard_drugs,
              
              include = c("AGG_MENT", "AGG_PHYS", "LEU3N", "lg.VLOAD", "BMI", "age", "RACE", "EDUCBAS", "SMOKE"),
              
              label = list(AGG_MENT ~ "SF36 MCS score", AGG_PHYS ~ "SF36 PCS score", LEU3N ~ "Number of CD4+ cells", 
                         lg.VLOAD ~ "Log10 Standardized viral load (copies/ml)", BMI ~ "Body Mass Index (kg/m^2)", age ~ "Age", 
                         RACE ~ "Race/Ethinicity", EDUCBAS ~ "Highest level of education attained", SMOKE ~ "Smoking Status"),
              
              type = list(c("AGG_MENT", "AGG_PHYS", "LEU3N", "lg.VLOAD", "BMI", "age") ~ "continuous",
                         c("RACE", "EDUCBAS", "SMOKE") ~ "categorical"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),

              digits = list(c("AGG_MENT", "AGG_PHYS", "LEU3N", "lg.VLOAD", "BMI", "age") ~ 1,
                            c("RACE", "EDUCBAS", "SMOKE") ~ 0),
              
              missing_text = "NA (Missing)") |>
  add_overall(last=TRUE)
tab1
#Include N and % for the missing/NA
#Label the levels of the categorical variables
#Add headers and footnote as appropriate


tab2 <- hiv.dat3[!is.na(hiv.dat3$years.2) & hiv.dat3$years.2==2, ] |> #Select for year 2 in the merged year0 and year2 data (this code is necessary)
  tbl_summary(by=hard_drugs,
              include=names(hiv.dat)[c(-1, -2)],
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing_text = "NA (Missing)") |>
  add_overall(last=TRUE)
tab2

tbl_merge(
  tbls = list(tab1, tab2),
  tab_spanner = c("**Baseline**", "**Follow-up (Year 2)**")
  )

#To have a variable of number lost to follow-up, separate and merge year=0 and year=2 into each other 
#  but merge such that cases without data in both datasets are represented in the final merge
#  create a variable based on this and include it in the descriptives



#Then recode variables to binary or categorical as appropriate for analysis
for (i in 1:ncol(hiv.dat)) print(table(is.na(hiv.dat[,i]))) #See the observations with NAs
# Work on NAs, 
# Review outliers and plausible values for continuous variables (ask instructor)
# Recode categorical variables as appropriate for analysis
# Apply transformations as appropriate























#table & proportion of hard drug use over time
table(hiv.dat$hard_drugs, hiv.dat$years)
round(prop.table(table(hiv.dat$hard_drugs, hiv.dat$years), 2),3)

with(hiv.dat[hiv.dat$years %in% c(0,2),], table(hard_drugs, years, exclude=NULL))