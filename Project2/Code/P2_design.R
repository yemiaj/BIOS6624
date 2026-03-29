#######################

#Codes related to the analysis and sample size calculation for a grant application

######################

#Read in raw preliminary data provided by the study PI
prelim <- read.csv("./Project2/DataRaw/PrelimData.csv")

names(prelim)
head(prelim)
cor(prelim)
for (i in 1:4) print(sd(prelim[,i]))

#Here are variable labels provided by the PI

#"CVLT_CNG3", change in CVLT
#"CORT_CNG3", change in cortical thickness
#"IL_6", inflammatory marker
#"MCP_1", inflammatory marker


prelim$CVLT_CNG3.2 <- -1*prelim$CVLT_CNG3
prelim$CORT_CNG3.2 <- -1*prelim$CORT_CNG3
cor(prelim[,c(3:6)])

summary(lm(CVLT_CNG3~IL_6,prelim))
summary(lm(CVLT_CNG3~MCP_1,prelim))

summary(lm(CORT_CNG3~IL_6,prelim))
summary(lm(CORT_CNG3~MCP_1,prelim))


library(powertools)
mlrF.partial(N = 175, 
             p = 4,
             q = 1,
             Rsq.red = 0.35,
             Rsq.full = 0.55,
             alpha = 0.05,
             v = TRUE)


mlrF.partial(N = 175, 
             p = 4,
             q = 1,
             Rsq.red = 0.5,
             Rsq.full = 0.55,
             alpha = 0.05,
             v = TRUE)

#Literature refernce for Rsquared in the realm of alzheirmers disease

#Add reference to textbook from CU Anschutz


