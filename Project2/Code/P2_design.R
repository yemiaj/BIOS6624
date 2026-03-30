#######################

#Codes related to the analysis and sample size calculation for a grant application

######################

#Required packages for power analysis
library(powertools) 
library(InteractionPoweR)

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

reduced.R2 <- seq(0.01, 0.15, .02) #Range of R^2 in the reduced model (See Supplementary section)
delta.R2 <- seq(0.02, 0.50, 0.02) #These range of delta for R^2 covers the range of values (0.07 to 0.48) estimated from the preliminary data
                                    #See the Preliminary data heading of Power Analysis section

pow.mat <- matrix(ncol = length(reduced.R2), nrow = length(delta.R2)) #Empty matrix of NAs that will be filled with estimated power estimate

for (i in 1:ncol(pow.mat)) {
  for (j in 1:nrow(pow.mat)) {
    #p = 8 was specified to cover a max of 5 covariates + 2 for age and sex, + 1 baseline measure for each outcome, BL-cort for example
    pow.mat[j,i] <- mlrF.partial(N = 175, p = 8, q = 1, Rsq.red = reduced.R2[i], Rsq.full = reduced.R2[i]+delta.R2[j], alpha = 0.05) * 100
  }
}
pow.mat

par(mfrow=c(1,2))
#Graph the data in pow.mat
plot(pow.mat[,1], ylim=c(40, 100), type='o', xlim=c(1,8), lwd=2, xaxt='n',  #plot is truncated because power is 100% with delta >0.16
     ylab = 'Statistical Power (%)', xlab = "R.Squared effect size", main = "Aim 1a & 1b: Attained statistical power by effect size for \nvarious R.Squared values")
abline(h=seq(0, 100, 2), v = seq(1, 8, .5), col='grey90')
for (i in 2:8) lines(pow.mat[,i], type='o', col=i, lwd=2, lty=i)
abline(h=90, col="blue", lty=2, lwd=2)
axis(side = 1, at = 1:8, labels = delta.R2[1:8])
legend(2.7, 85, paste0(rev(reduced.R2)), bty='n', col=8:1, lwd=3, lty=8:1, cex=1.5, title = "R.Squared in Reduced Model")
points(x=3, y=95, pch="+", col="blue", cex=2.5) #Example power calculation scenario described in report

#Get citation
citation("powertools")

#Aim 2
#Package webpage: https://dbaranger.github.io/InteractionPoweR/
#Package tutorial peer-reviewed paper: https://journals.sagepub.com/doi/10.1177/25152459231187531
#Notable features of this package that is relevant to the current project is
#  (1) the ability to compute power for interactions between two continuous variables
#  (2) effect sizes are all specified as the cross-sectional Pearson’s correlation (which we have some preliminary data for)
#power_interaction calculates power via simulation while power_interaction_r2 does the same by solving for Cohen's f^2 (f-square)
  #my preference is for power_interaction_r2 because the hypothesis test (partial F-test) follows the same as Aim 1a & 1b


power_interaction_r2(
  N = 175,
  r.x1.y = .26, #preliminary data provided, rho between x1 (inflammation) and outcome (CVLT and cortical thickness)
  r.x2.y = .1, #preliminary data NOT available (fixed at 0.1 to represent low correlation)
    #This parameter is directly proportion to power, until a certain point, especially when the value of r.x2.y and r.x1.x2 is similar
  r.x1.x2 = .1, #preliminary data NOT available (fixed at 0.1 to represent low correlation)
  
  r.x1x2.y = .15, #main parameter of interest (interaction effect)
    #it is the correlation between the interaction term and outcome
  
  alpha = 0.05
)
#pertub r.x1.y to range between 0.25, 0.35, 0.45 for cognitive measures outcome
#pertub r.x1.y to range between 0.55, 0.60, 0.65, 0.70 for cortical thickness outcome
#Observe that range of correlation coefficients merges together nicely
  #therefore, put them together as seq(0.25, 0.75, .05)
#perturb interaction effect r.x1x2.y as seq(0.15, 0.50, 0.05)

#Note that correlation coefficient is mirrored on the y-axis such that you will get the same power estimate if a negative correlation coefficient (of the same magnitude) is specified
  #for (a pair of related) the parameters in power_interaction_r2(), for example, power is the same for r.x1.x2=r.x2.y = .1 and r.x1.x2=r.x2.y = -.1
  #This is also applicable to the detectable interaction effect size. 
  #This can be confirmed by specifying int.eff.rho <- -1* seq(0.15, 0.50, 0.05) in the code below

# Aim 2 Power calculation proper
x1.y.rho <- seq(0.25, 0.70, .05) #Range of rho for the relationship between x1 (inflammation) and outcome
int.eff.rho <- seq(0.15, 0.50, 0.05) #interaction effect, rho between interaction term and outcome

rho.pow.mat <- matrix(ncol = length(x1.y.rho), nrow = length(int.eff.rho)) #Empty matrix of NAs that will be filled with estimated power estimate

for (i in 1:ncol(rho.pow.mat)) {
  for (j in 1:nrow(rho.pow.mat)) {
    rho.pow.mat[j,i] <- as.numeric(power_interaction_r2(N = 175, r.x1.y = x1.y.rho[i], r.x2.y = .1, r.x1.x2 = .1, r.x1x2.y = int.eff.rho[j], alpha = 0.05)) * 100
  }
}
rho.pow.mat

#Graph the data in pow.mat
plot(rho.pow.mat[,1], ylim=c(40, 100), type='o', lwd=2, xaxt='n', 
     ylab = 'Statistical Power (%)', xlab ="Effect size for interaction effect (rho)", 
     main = "Aim 2: Attained statistical power by interaction effect size for \n various rho between predictor and outcome")
abline(h = seq(0, 100, 2), v = seq(1, 8, .5), col='grey90')
abline(h=90, col="blue", lty=2, lwd=2)
for (i in 2:10) lines(rho.pow.mat[,i], type='o', col=i, lwd=2, lty=i)
axis(side = 1, at = c(1:8), labels = int.eff.rho[1:8])
legend(2.7, 85, paste0(rev(x1.y.rho)), bty='n', col=10:1, lwd=3, lty=10:1, cex=1.5, title = "Rho for X1 (InfMrk) and Y (CortThck)")
points(x=3, y=92, pch="+", col="blue", cex=2.5) #Example power calculation scenario described in report

#Get citation
citation("InteractionPoweR")

#Cohen's f^2 as supplementary. No space. It is a measure of strength of association on the R-squared scale? f2 >= 0.02, >= 0.15, and >= 0.35 represents small, medium, and large effects respectively
#Explain bell shape of correlation coefficient in the report. No space, already explained above (this is mirroring on the Y-asix depending on the sign of rho)


#Output Figure 1 to file

pdf("./Project2/Reports/p2_Figure 1.pdf", width = 15, height = 8)

par(mfrow=c(1,2))
#Graph the data in pow.mat
plot(pow.mat[,1], ylim=c(40, 100), type='o', xlim=c(1,8), lwd=2, xaxt='n',  #plot is truncated because power is 100% with delta >0.16
     ylab = 'Statistical Power (%)', xlab = "R.Squared effect size", main = "Aim 1a & 1b: Attained statistical power by effect size for \nvarious R.Squared values")
abline(h=seq(0, 100, 2), v = seq(1, 8, .5), col='grey90')
for (i in 2:8) lines(pow.mat[,i], type='o', col=i, lwd=2, lty=i)
abline(h=90, col="blue", lty=2, lwd=2)
axis(side = 1, at = 1:8, labels = delta.R2[1:8])
legend(2.7, 85, paste0(rev(reduced.R2)), bty='n', col=8:1, lwd=3, lty=8:1, cex=1.5, title = "R.Squared in Reduced Model")
points(x=3, y=95, pch="+", col="blue", cex=2.5) #Example power calculation scenario described in report

plot(rho.pow.mat[,1], ylim=c(40, 100), type='o', lwd=2, xaxt='n', 
     ylab = 'Statistical Power (%)', xlab ="Effect size for interaction effect (rho)", 
     main = "Aim 2: Attained statistical power by interaction effect size for \n various rho between predictor and outcome")
abline(h = seq(0, 100, 2), v = seq(1, 8, .5), col='grey90')
abline(h=90, col="blue", lty=2, lwd=2)
for (i in 2:10) lines(rho.pow.mat[,i], type='o', col=i, lwd=2, lty=i)
axis(side = 1, at = c(1:8), labels = int.eff.rho[1:8])
legend(2.7, 85, paste0(rev(x1.y.rho)), bty='n', col=10:1, lwd=3, lty=10:1, cex=1.5, title = "Rho for InfMrk and CortThck")
points(x=3, y=92, pch="+", col="blue", cex=2.5) #Example power calculation scenario described in report

dev.off()
