The goal of this project is to investigate different model selection techniques in linear regression.

#Layout of the required task for this project

Methods
  1) backward selection based on F-test and p-value
  2) AIC
  3) BIC
  4) LASSO, 2 choices of lambda (lambda: min and 1se) ==> lm fit, debiased LASSO, and/or post-selection inference
  5) Elastic net, 2 choices of lambda (lambda: min and 1se) ==> lm fit, debiased LASSO, and/or post-selection inference

Scenarios
  1) N = 250, P = 20; 5 of the 20 P would be considered important with
    BETA_1:5 = 0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3
    1a) Relationship among Xs: exchangeable correlation with rho c(0.00, 0.35, 0.70)

  2) N = 500, P = 20; 5 of the 20 P would be considered important with 
    BETA_1:5 = 0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3
    2a) Relationship among Xs: exchangeable correlation with rho c(0.00, 0.35, 0.70)

Results of interest
  1) True positives
  2) False positives: A confusion matrix for 1) and 2)
  
For Selected Variables
    3) Bias
    4) Coverage of 95% CI
    5) Type I error
    6) Type II error

For retained variables, there is a choice for doing testing on the final model 
  (refitting the model as if variable selection did not occur) OR you an alternative and more appropriate method
  for testing because the first approach of refitting the final model is biased.
    a) Look into debiassed (desparsified) LASSO,
    b) Post-selection inference
      a) and b) provides 95% CI, point estimate, and standard error


#Class discussion
exchangeable correlation with 3 different correlations: 0, 0.35, 0.70
Because cv.glmnet()is random, check to see if the model solution is sensitive to the seed. If yes, then check and tweak your settings, e.g. change the number of folds.
min and 1se as choice of lambda
Check whether the coefficient output is standardized or not
Pick the most important statistic of interest, and use the asymptotic/simulation SE of that statistic to determine the number of reps you need. Otherwise just use 10000


To be significant, it has to be selected AND the post-test has to be significant

Reach contextual conclusion of results
Write function to test that package is doing what it is suppossed to do for each combo of arguments
Trusted R package, but check that is doing what is expected to do based on the arguments specified!
Save seed for each run for dat_gen and cv.glmnet
Provide details of the packages used for each of the 5+2 variable selection methods
Do random testing to make sure performance measures is doing what is expected
In future, vectorize and parallelize for faster run times (although my run time now was not too bad)
            Use true random seed generator for better operating seed characteristics 
            Check debiassed lasso and post-selection inference methods
Include sampling distribution to show normality of estimates. E.g. hist(all.reuslts[all.reuslts$rho==.35,]$bias1)
Oracle, specific function arguments used, and paralellization
2dp for estimates
Do post-selection inference
What is the optimmal Monte-Carlo SE for bias, coverage, or rejection for example. Use the option that requires the most iterations, that is highest SE/
How often is X6 different from the other variables in terms of selection, and difference in performance measure
Comment on how doing selecitoninference is an extension of the current work
Unconditional: coverage is set to 0 when not selected, therefore coverage estimate will be low if not selected often
True signal, true noise
Seperate table for bias, rejection, tp & fp
Plot for coverage



#Questions for office hours 
1) Ask that the beta of 20-5 = 0 for the unimportant variables
2) Bias is just for important variables, correct? The espected value for unimportant variables is 0 and bias can still be estimated
    Ans: If they are being selected under correlation then they may be interested to talk about. True beta is 0 regardless since they are 0 in the package/function
3) Getting type I error requires simulating data under null. No? Or we are adopting a different meaning of type I error here?
4) What if the set of 5 variables are not selected in the model? 
5) What p-value cutoff to use for F-test?
6) We only care about bias, coverage, type I and II errors for the 5 selected variables. Yes?
7) Is bias and coverage for all 5 vars or for each of the 5 variables?
  
Look into doRNG R package for parallelization

#Insights from office hours  
If you have a greater correlation, should you increase the alpha? Choose multiple values of alpha to check that elastic net is doing what it is suppossed to do with respect to being better with correlation
0.05 for backwards selection
AIC/BIC is a larger p-value alternative to 0.05 for backwards
Unconditional (the focus and we are interested in variable selection) for bias and coverage is of interest.
Analyze but summarize heavily results from X6 - X20
Focus on unconditional bias and coverage (i.e. without regard for the variable being selected in a given model)
Type 1: Selected and p-value <= 0.05 (use X6 to X20 for this)
Type 2: not selected or p-value > 0.05 (use X1 to X5 for this)
Coverage is 0 and bias is equal to beta when not selected

#insights from presentations
Multicollinearity among predictors
Predictors range from weak to strong association
lambda chosen via k-fold cross-validation
error is N(0,sigma^2); see the class worksheet for other details to add
Type1 and 2 errors is per variable

Add TPR for each of X1--X5, and TPR for all of X1-X5

#More questions
Loss function for cv.glmnet? MSE?
I do not think I understand the true/false positive thing






  
