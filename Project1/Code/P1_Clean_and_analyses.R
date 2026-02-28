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
library(dplyr)
library(tibble)

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

#Assign and label levels for categorical variables
hiv.dat$hard_drugs <- factor(hiv.dat$hard_drugs, levels = c(0, 1), labels = c("No", "Yes"))
hiv.dat$RACE <- factor(hiv.dat$RACE, levels = c(1, 2, 3, 4, 7, 8), labels = c("White, non-Hispanic", "White, Hispanic", "Black, non-Hispanic",
                                                                                  "Black, Hispanic", "Other", "Other Hispanic"))
hiv.dat$EDUCBAS <- factor(hiv.dat$EDUCBAS, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("8th grade or less", "9th to 11th grade", "12th grade",
                                                                                           "1+ year college (but no degree)", "Four years college", 
                                                                                           "Some graduate work", "Post-graduate degree"))
hiv.dat$SMOKE <- factor(hiv.dat$SMOKE, levels = c(1, 2, 3), labels = c("Never smoked", "Former smoker", "Current smoker"))
hiv.dat$ADH <- factor(hiv.dat$ADH, levels = c(1, 2, 3, 4), labels = c("100%", "95 - 99%", "75 - 94%", "<75%"))


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


# Data cleaning ####
# Select needed variables so that complete case function does not exclude needed rows when unneeded variable with missing is included
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



#Model template, models from which coefficient estimates will be estimated and which will be used to check for mediator status of Adherence
mod.summary <- function(data, y) {
  
  x1="hd.use"
  x2="bmi + age + white.nh + college + curnt.smkr"
  modif="adh.y2"
  
  uni.frm <- as.formula(paste(y[2], "~", x1))
  crd.frm <- as.formula(paste(y[2], "~", x1, "+", y[1], "+", x2))
  adj.frm <- as.formula(paste(y[2], "~", x1, "+", y[1], "+", modif, "+", x2))
  
  univariate.model <- lm(uni.frm, data = data)
  crude.model <- lm(crd.frm, data = data)
  adjusted.model <- lm(adj.frm, data = data)
  
  u.mod.s <- summary(univariate.model)
  c.mod.s <- summary(crude.model)
  a.mod.s <- summary(adjusted.model)
  
  e1 <- (cbind(u.mod.s$coefficients, confint(univariate.model)))
  e1 <- cbind(e1, 
              rep(-2*logLik(univariate.model)[1], nrow(e1)),
              rep(attr(logLik(univariate.model), 'df'), nrow(e1)),
              rep(u.mod.s$sigma, nrow(e1)),
              rep(u.mod.s$r.squared, nrow(e1)),
              rep(u.mod.s$adj.r.squared, nrow(e1)),
              rep(u.mod.s$fstatistic[1], nrow(e1)),
              rep(u.mod.s$fstatistic[2], nrow(e1)),
              rep(u.mod.s$fstatistic[3], nrow(e1)),
              rep(1, nrow(e1)),
              rep(NA, nrow(e1))
              )
  e1 <- data.frame(e1)
  e1$vars <- noquote(names(univariate.model$coefficients))
  
  e2 <- (cbind(c.mod.s$coefficients, confint(crude.model)))
  e2 <- cbind(e2, 
              rep(-2*logLik(crude.model)[1], nrow(e2)),
              rep(attr(logLik(crude.model), 'df'), nrow(e2)),
              rep(c.mod.s$sigma, nrow(e2)),
              rep(c.mod.s$r.squared, nrow(e2)),
              rep(c.mod.s$adj.r.squared, nrow(e2)),
              rep(c.mod.s$fstatistic[1], nrow(e2)),
              rep(c.mod.s$fstatistic[2], nrow(e2)),
              rep(c.mod.s$fstatistic[3], nrow(e2)),
              rep(2, nrow(e2)),
              c(0, car::vif(crude.model))
  )
  e2 <- data.frame(e2)
  e2$vars <- noquote(names(crude.model$coefficients))
  
  e3 <- (cbind(a.mod.s$coefficients, confint(adjusted.model)))
  e3 <- cbind(e3, 
              rep(-2*logLik(adjusted.model)[1], nrow(e3)),
              rep(attr(logLik(adjusted.model), 'df'), nrow(e3)),
              rep(a.mod.s$sigma, nrow(e3)),
              rep(a.mod.s$r.squared, nrow(e3)),
              rep(a.mod.s$adj.r.squared, nrow(e3)),
              rep(a.mod.s$fstatistic[1], nrow(e3)),
              rep(a.mod.s$fstatistic[2], nrow(e3)),
              rep(a.mod.s$fstatistic[3], nrow(e3)),
              rep(3, nrow(e3)),
              c(0, car::vif(adjusted.model))
              )
  e3<- data.frame(e3)
  e3$vars <- noquote(names(adjusted.model$coefficients))
  
  mod.ests <- rbind(e1, e2, e3)
  
  colnames(mod.ests) <- c("Estimates", "StdErr", "t.value", "p-value", "LL 95% CI", "UL 95% CI", "-2LogL", "LogL.df", "MSE", "Rsquared", "Adj.Rsquared", "F", "n.df", "d.df", "kind", "VIF", "vars")
  mod.ests$model.type <- ifelse(mod.ests$kind == 1, "Univariable", ifelse(mod.ests$kind == 2, "Crude", "Adjusted"))
  
  return(mod.ests)
}

med.summary <- function(mod, resp) {
  mediation <- data.frame(crude = mod[mod$model.type=="Crude" & mod$vars=='hd.use', c("Estimates", "StdErr", "p-value")],
             adjusted = mod[mod$model.type=="Adjusted" & mod$vars=='hd.use', c("Estimates", "StdErr", "p-value")])
  
  mediation$indirect.effect <- mediation$crude.Estimates - mediation$adjusted.Estimates
  
  mediation$percent.mediation <- (mediation$indirect.effect / mediation$crude.Estimates)*100
  
  return(mediation)
}

phy.qol.mod <- mod.summary(data = hiv.dat6, y=c("phy.qol", "phy.qol.y2"))
ment.qol.mod <- mod.summary(data = hiv.dat6, y=c("ment.qol", "ment.qol.y2"))
cd4.mod <- mod.summary(data = hiv.dat6, y=c("cd4.count", "cd4.count.y2"))
vload.mod <- mod.summary(data = hiv.dat6, y=c("lg10.vload", "lg10.vload.y2"))

phy.med <- round(med.summary(phy.qol.mod),3)
ment.med <- round(med.summary(ment.qol.mod),3)
cd4.med <- round(med.summary(cd4.mod),3)
vload.med <- round(med.summary(vload.mod),3)

mediation.analysis <- rbind(phy.med, ment.med, cd4.med, vload.med)
rownames(mediation.analysis) <- c("Physical QoL", "Mental QoL", "CD4+ Counts", "Log10 Viral Load")
mediation.analysis

#VLOAD: +/- 0.5log10 change, CD4: +/- 50 cells/ml, QoL: +/- 2pts

#mediation models
#anova, Extract R^2, -2log lik, nparameters, dist of residuals. normality test
#observed and predicted plots


Confounder and mediation goes into supplementary
Baseline as a precision variable
Combine confounders with the precision variable and proceed as usual


#Keep a dataframe containing the original variable so you can describe the exclusions
# Work on NAs, 
# Review outliers and plausible values for continuous variables (ask instructor)
# Recode categorical variables as appropriate for analysis
# Apply transformations as appropriate




###########################################################################
# CREDITS: This Bayesian analysis code block was adapted from instructor's 
#   (Camille Moore, PhD) sample code provided on Canvas
###########################################################################


####Bayesian Analysis
###########################################################################
# STEP 1: Define the model
# This is a general linear regression set up for STAN
# It is using a half normal prior on sigma
# and normal priors on the regression coefficients
# once the stan file is written it can be reused
###########################################################################

stan_file <- write_stan_file("data {
  int<lower=0> N;                  // number of observations
  int<lower=0> P;                  // number of predictors including intercept
  matrix[N, P] X;                  // design matrix (first column = intercept)
  vector[N] y;                     // outcome

  vector[P] prior_mean;            // prior means for each beta
  vector<lower=0>[P] prior_sd;     // prior SDs for each beta

  real<lower=0> sigma_prior_sd;    // SD for half-normal prior on sigma
}

parameters {
  vector[P] beta;                  // regression coefficients
  real<lower=0> sigma;             // residual SD
}

model {
  // Vectorized priors for regression coefficients
  beta ~ normal(prior_mean, prior_sd);

  // Half-normal prior for sigma
  sigma ~ normal(0, sigma_prior_sd);

  // Likelihood
  y ~ normal(X * beta, sigma);

}

generated quantities {
  // log likelihood for each observation for calculating model fit stats
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n] * beta, sigma);}
  }", dir="./Project1/Code", basename='linear_regression_half_normal'
)

#directory for .Rmd "../Code"
#directory for regular .R "./Project1/Code"


###########################################################################
# STEP 2: Compile the Stan program
###########################################################################
mod <- cmdstan_model('./Project1/Code/linear_regression_half_normal.stan')


###########################################################################
# STEP 3: Prepare your data to pass to Stan
###########################################################################

#Physical QoL
y <- hiv.dat6$phy.qol.y2
Xuni <- model.matrix(~ hd.use, data = hiv.dat6) 
Xcrd <- model.matrix(~ hd.use + phy.qol + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 
Xadj <- model.matrix(~ hd.use + phy.qol + adh.y2 + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 

Nuni <- nrow(Xuni); Puni <- ncol(Xuni)
m <- rep(0, Puni); s <- rep(1000, Puni)
sigma_sd <- 1000
data_list <- list(N = Nuni, P = Puni, X = Xuni, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.phy.uni <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Ncrd <- nrow(Xcrd); Pcrd <- ncol(Xcrd)
m <- rep(0, Pcrd); s <- rep(1000, Pcrd)
sigma_sd <- 1000
data_list <- list(N = Ncrd, P = Pcrd, X = Xcrd, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.phy.crd <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Nadj <- nrow(Xadj); Padj <- ncol(Xadj)
m <- rep(0, Padj); s <- rep(1000, Padj)
sigma_sd <- 1000
data_list <- list(N = Nadj, P = Padj, X = Xadj, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.phy.adj <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)


#Mental QoL
y <- hiv.dat6$ment.qol.y2
Xuni <- model.matrix(~ hd.use, data = hiv.dat6) 
Xcrd <- model.matrix(~ hd.use + ment.qol + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 
Xadj <- model.matrix(~ hd.use + ment.qol + adh.y2 + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 

Nuni <- nrow(Xuni); Puni <- ncol(Xuni)
m <- rep(0, Puni); s <- rep(1000, Puni)
sigma_sd <- 1000
data_list <- list(N = Nuni, P = Puni, X = Xuni, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.ment.uni <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Ncrd <- nrow(Xcrd); Pcrd <- ncol(Xcrd)
m <- rep(0, Pcrd); s <- rep(1000, Pcrd)
sigma_sd <- 1000
data_list <- list(N = Ncrd, P = Pcrd, X = Xcrd, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.ment.crd <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Nadj <- nrow(Xadj); Padj <- ncol(Xadj)
m <- rep(0, Padj); s <- rep(1000, Padj)
sigma_sd <- 1000
data_list <- list(N = Nadj, P = Padj, X = Xadj, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.ment.adj <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)


#CD4 Count
y <- hiv.dat6$cd4.count.y2
Xuni <- model.matrix(~ hd.use, data = hiv.dat6) 
Xcrd <- model.matrix(~ hd.use + cd4.count + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 
Xadj <- model.matrix(~ hd.use + cd4.count + adh.y2 + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 

Nuni <- nrow(Xuni); Puni <- ncol(Xuni)
m <- rep(0, Puni); s <- rep(1000, Puni)
sigma_sd <- 1000
data_list <- list(N = Nuni, P = Puni, X = Xuni, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.cd4.uni <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Ncrd <- nrow(Xcrd); Pcrd <- ncol(Xcrd)
m <- rep(0, Pcrd); s <- rep(1000, Pcrd)
sigma_sd <- 1000
data_list <- list(N = Ncrd, P = Pcrd, X = Xcrd, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.cd4.crd <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Nadj <- nrow(Xadj); Padj <- ncol(Xadj)
m <- rep(0, Padj); s <- rep(1000, Padj)
sigma_sd <- 1000
data_list <- list(N = Nadj, P = Padj, X = Xadj, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.cd4.adj <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)


#vload Count
y <- hiv.dat6$lg10.vload.y2
Xuni <- model.matrix(~ hd.use, data = hiv.dat6) 
Xcrd <- model.matrix(~ hd.use + lg10.vload + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 
Xadj <- model.matrix(~ hd.use + lg10.vload + adh.y2 + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 

Nuni <- nrow(Xuni); Puni <- ncol(Xuni)
m <- c(1, rep(0, (Puni-1))); s <- rep(1000, Puni)
sigma_sd <- 1000
data_list <- list(N = Nuni, P = Puni, X = Xuni, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.vload.uni <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Ncrd <- nrow(Xcrd); Pcrd <- ncol(Xcrd)
m <- c(1, rep(0, (Pcrd-1))); s <- rep(1000, Pcrd)
sigma_sd <- 1000
data_list <- list(N = Ncrd, P = Pcrd, X = Xcrd, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.vload.crd <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)

Nadj <- nrow(Xadj); Padj <- ncol(Xadj)
m <- c(1, rep(0, (Padj-1))); s <- rep(1000, Padj)
sigma_sd <- 1000
data_list <- list(N = Nadj, P = Padj, X = Xadj, y = y, prior_mean = m, prior_sd = s, sigma_prior_sd = sigma_sd)
# 5000 burn-in and 10K iterations
bayes.vload.adj <- mod$sample(data = data_list, chains = 4, iter_warmup = 5000, iter_sampling = 20000, seed = 6624)


#Function for table of Bayesian estimates
bayes.key.ests <- function(fit){
  draws <- fit$draws()
  draws_mat <- as_draws_matrix(draws)
  params <- colnames(draws_mat)
  params <- params[!grepl("lp__|log_lik", params)]
  ests <- data.frame(fit$summary(variables = params))
  return(ests)
}
bayes.key.ests(bayes.phy.crd)
bayes.key.ests(bayes.phy.adj)

bayes.key.ests(bayes.ment.crd)
bayes.key.ests(bayes.ment.adj)

bayes.key.ests(bayes.cd4.crd)
bayes.key.ests(bayes.cd4.adj)

bayes.key.ests(bayes.vload.crd)
bayes.key.ests(bayes.vload.adj)



bayes.mediator <- function(fit1, fit2) {
  mediator.posterior <- as.numeric(as_draws_matrix(fit1$draws())[, "beta[2]"]) - as.numeric(as_draws_matrix(fit2$draws())[, "beta[2]"])
  mean <- mean(mediator.posterior);
  sd <- sd(mediator.posterior)
  quant <- quantile(mediator.posterior, probs=c(0.025, .975))
  ll <- quant[1]
  ul <- quant[2]
  return(c(mean, sd, ll, ul))  
}
bayes.mediator(bayes.phy.crd, bayes.phy.adj)
bayes.mediator(bayes.ment.crd, bayes.ment.adj)
bayes.mediator(bayes.cd4.crd, bayes.cd4.adj)
bayes.mediator(bayes.vload.crd, bayes.vload.adj)


#Another function for probability for clinically meaningful difference
#Another function for performance measures for clinically meaningful difference
#Another function for plots for clinically meaningful difference



#Extract details
draws <- bayes.vload.adj$draws()
draws_mat <- as_draws_matrix(draws)
params <- colnames(draws_mat)
params <- params[!grepl("lp__|log_lik", params)]
data.frame(bayes.vload.adj$summary(variables = params)) #Is this the same as the lapply function?
final_draws <- draws_mat[, params]

loglik_mat <- as_draws_matrix(bayes.vload.adj$draws("log_lik"))  # iterations x N
waic_res <- waic(loglik_mat)
print(waic_res)
waic_res$estimates[3,1]

draws <- as_draws_array(bayes.vload.adj$draws())  # dimensions: iterations x chains x parameters
draws_df <- as_draws_df(bayes.vload.adj$draws())  # tidy format for bayesplot / ggplot
#params <- c("beta[1]", "beta[2]", "beta[3]", "sigma")

#Print these for the final selected model for each endpoint
mcmc_trace(draws, pars = params)
mcmc_dens_overlay(draws, pars = params)
mcmc_acf(draws, pars = params)
bayes.vload.adj$cmdstan_diagnose()












bayes.cd4.adj$summary(variables=c("beta[1]", "beta[2]","beta[3]", "beta[4]","beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))
bayes.vload.adj$summary(variables=c("beta[1]", "beta[2]","beta[3]", "beta[4]","beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))

colnames(as_draws_matrix(bayes.cd4.adj$draws()))








# Outcome data
y <- hiv.dat6$phy.qol.y2

# Design matrix in our linear regression
X <- model.matrix(~ hd.use + phy.qol + adh.y2 + bmi + age + white.nh + college + curnt.smkr, data = hiv.dat6) 

# These are variables needed in Stan
# N is the number of observations in your data set 
# P is the number of columns in the design matrix
N <- nrow(X)
P <- ncol(X)

m <- rep(0, P) # mvnorm mean  (mean in the prior on the regression coefficients)
s <- rep(1000,P) # SD in the prior on regression coefficients --> variance 1000^2
sigma_sd <- 1000

m.vload <- c(1, rep(0, (P-1))) # adjusted the vector of means for vload to have an intercept of 1, 
# since the outcome was log10 transformed

# create data list to pass to STAN
data_list <- list(
  N = N,
  P = P,
  X = X,
  y = y,
  prior_mean = m,
  prior_sd = s,
  sigma_prior_sd = sigma_sd
)

###########################################################################
# STEP 4: Fit Model
###########################################################################

# 5000 burn-in and 10K iterations
bayes.phy <- mod$sample(
  data = data_list,
  chains = 4,
  iter_warmup = 5000,
  iter_sampling = 20000,
  seed = 6624
)

bayes.phy$summary(variables = c("beta[1]", "beta[2]","beta[3]", "beta[4]", "beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))
bayes.phy.draws <- bayes.phy$draws()  
bayes.phy.draws_mat <- as_draws_matrix(bayes.phy.draws)
bayes.phy.params <- colnames(bayes.phy.draws_mat)
bayes.phy.params <- bayes.phy.params[!grepl("lp__|log_lik", bayes.phy.params)]

mcmc_dens_overlay(as_draws_array(bayes.phy$draws()), pars = c("beta[1]", "beta[2]","beta[3]", "beta[4]", "beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))
mcmc_trace(as_draws_array(bayes.phy$draws()), pars = c("beta[1]", "beta[2]","beta[3]", "beta[4]", "beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))
mcmc_acf(as_draws_array(bayes.phy$draws()), pars = c("beta[1]", "beta[2]","beta[3]", "beta[4]", "beta[5]", "beta[6]","beta[7]", "beta[8]","beta[9]", "sigma"))
bayes.phy$cmdstan_diagnose()

#c("phy.qol","phy.qol.y2")
bay.mods <- function(data, y){
  y <- data[,y[2]]
  
  x1="hd.use"
  x2="bmi + age + white.nh + college + curnt.smkr"
  modif="adh.y2"
  
  #X.full <- model.matrix(as.formula(paste("~", x1, "+", y[1], "+", modif, "+", x2)) , data=data)
  X.full <- model.matrix(~ y[1], data=data)
  N <- nrow(X.full)
  P <- ncol(X.full)
  
  m <- rep(0, P) # mvnorm mean  (mean in the prior on the regression coefficients)
  s <- rep(1000,P) # SD in the prior on regression coefficients --> variance 1000^2
  sigma_sd <- 1000
  
  # create data list to pass to STAN
  data_list <- list(
    N = N,
    P = P,
    X = X.full,
    y = y,
    prior_mean = m,
    prior_sd = s,
    sigma_prior_sd = sigma_sd
  )
  
  bayes.fit <- mod$sample(
    data = data_list,
    chains = 4,
    iter_warmup = 5000,
    iter_sampling = 20000,
    seed = 6624
  )
  
  return(bayes.fit)
}

bay.mods(data=hiv.dat6, y=c("phy.qol","phy.qol.y2"))

