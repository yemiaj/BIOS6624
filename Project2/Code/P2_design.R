#######################

#Codes related to the analysis and sample size calculation for a grant application

######################

library(powertools) #Required package for power analysis

#Read in raw preliminary data provided by the study PI
prelim <- read.csv("./Project2/DataRaw/PrelimData.csv")

names(prelim)
#Here are variable labels provided by the PI via Canvas announcement
#"CVLT_CNG3", change in CVLT
#"CORT_CNG3", change in cortical thickness
#"IL_6", inflammatory marker
#"MCP_1", inflammatory marker

head(prelim)

cor(prelim) #Matrix of Pearson correlation coefficient

for (i in 1:4) print(sd(prelim[,i])) #standard deviation estimate for each variable

#Run simple linear regression to check the sign and magnitude of beta for each outcome and inflammation marker pair
summary(lm(CVLT_CNG3~IL_6,prelim))
summary(lm(CVLT_CNG3~MCP_1,prelim))
summary(lm(CORT_CNG3~IL_6,prelim))
summary(lm(CORT_CNG3~MCP_1,prelim))

#Power calculation based on partial F-test using adjusted R-squared as key parameters
mlrF.partial(N = 175, 
             p = 4, #p = 4 means the reduced model has 4 predictors
             q = 1, #q = 1 means power calculation for the partial F-test is for just 1 variable added
                    #therefore, the difference in number of predictors between the full and reduced models is 1
             Rsq.red = 0.35, #The proportion of variation (R^2) explained by the 4 predictors in the reduced model
             Rsq.full = 0.375, #R^2 for the full model, that is, the single added variable explained 0.375-0.35 = 0.025 proportion of variation
             alpha = 0.05,
             v = TRUE)

#See code below, p only has a modest effect on statistical power. Power reduced by less than 1%, for this procedure, for a reduced model
  #with 0 predictor vs 110 predictors in the reduced model. In practice this is not the case, such model will become saturated and standard errors unstable
p.range <- seq(0, 170, 10)
for (w in 1:length(p.range)) print(c(p.range[w], mlrF.partial(N = 175, p = p.range[w], q = 1, Rsq.red = 0.35, Rsq.full = 0.375)))

#See code below, q does have a significant impact on statistical power!
  #Unlike p, a change in q from 1 to 2 decreased power by  10%
q.range <- seq(1, 10, 1)
for (w in 1:length(q.range)) print(c(q.range[w], mlrF.partial(N = 175, p = 4, q = q.range[w], Rsq.red = 0.35, Rsq.full = 0.375)))


# Aim 1a and 1b Power calculation proper

#Add literature reference for Rsquared in the realm of Alzheimers disease
#Add reference to textbook from CU Anschutz detailing Partial F-test

reduced.R2 <- seq(0.05, 0.45, .05) #Range of R^2 in the reduced model
delta.R2 <- seq(0.02, 0.50, 0.02) #These range of delta for R^2 covers the range of values (0.07 to 0.48) estimated from the preliminary data

pow.mat <- matrix(ncol = length(reduced.R2), nrow = length(delta.R2)) #Empty matrix of NAs that will be filled with estimated power estimate

for (i in 1:ncol(pow.mat)) {
  for (j in 1:nrow(pow.mat)) {
    #p = 8 was specified to cover a max of 5 covariates + 2 for age and sex, + 1 baseline measure for each outcome, BL-cort for example
    pow.mat[j,i] <- mlrF.partial(N = 175, p = 8, q = 1, Rsq.red = reduced.R2[i], Rsq.full = reduced.R2[i]+delta.R2[j], alpha = 0.05) * 100
  }
}

#Graph the data in pow.mat
plot(pow.mat[,1], ylim=c(40, 100), type='o', xlim=c(0,8), lwd=2, xaxt='n', 
     ylab = 'Statistical Power (%)', xlab ="R^2 Delta", main = "Attained statistical power by effect size for \ndifferent reduced R.Square values")
abline(h=seq(0, 100, 5), v = 1:8, col='grey90')
for (i in 2:9) lines(pow.mat[,i], type='o', col=i, lwd=2, lty=i)
axis(side = 1, at = c(1:8), labels = delta.R2[1:8])
legend("topleft", paste0(rev(reduced.R2)), bty='n', col=9:1, lwd=3, lty=9:1, cex=1.2,
       title = "Reduced R.Square")









