#######################

#Codes related to the cleaning and preliminary analysis of Project 1: Multicenter AIDS Cohort Study

######################

#Import .csv file for Project 0
hiv.dat <- read.csv('./Project1/DataRaw/hiv_6624_final.csv') 

str(hiv.dat) #See variable types and values

#Extract the label of all variables and paste here.

#table & proportion of hard drug use over time
table(hiv.dat$hard_drugs, hiv.dat$years)

round(prop.table(table(hiv.dat$hard_drugs, hiv.dat$years), 2),3)


