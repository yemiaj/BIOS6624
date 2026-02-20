#######################

#Codes related to the cleaning and preliminary analysis of Project 1: Multicenter AIDS Cohort Study

######################

#Import .csv file for Project 0
hiv.dat <- read.csv('./Project1/DataRaw/hiv_6624_final.csv') 

str(hiv.dat) #See variable types and values


#Separate out year0 and year2
hiv.dat1 <- hiv.dat[hiv.dat$years==0, ]
hiv.dat2 <- hiv.dat[hiv.dat$years==2, ]


#dropout from year 0 to year 1, a 2 by 2 table of dropout/missing from year 0 to year 2



#table & proportion of hard drug use over time
table(hiv.dat$hard_drugs, hiv.dat$years)
round(prop.table(table(hiv.dat$hard_drugs, hiv.dat$years), 2),3)

with(hiv.dat[hiv.dat$years %in% c(0,2),], table(hard_drugs, years, exclude=NULL))













