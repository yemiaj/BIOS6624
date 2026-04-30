The goal of this project is to investigate different model selection techniques in linear regression.

#Layout of the required task for this project

Methods
  1) backward selection based on F-test and p-value
  2) AIC
  3) BIC
  4) LASSO, 2 choices of lambda (lambda: min and 1se)
  5) Elastic net, 2 choices of lambda (lambda: min and 1se)

Scenarios
  1) N = 250, P = 20; 5 of the 20 P would be considered important with
    BETA_1:5 = 0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3
    1a) Indepence among Xs
    1b) Correlation among Xs

  2) N = 500, P = 20; 5 of the 20 P would be considered important with 
    BETA_1:5 = 0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3
    2a) Indepence among Xs
    2b) Correlation among Xs
  
Results of interest
  1) True positives
  2) False positives: A confusion matrix for 1) and 2)
  
For Selected Variables
    3) Bias
    4) Coverage of 95% CI
    5) Type I error
    6) Type II error

For retianed variables, there is a choice for doing testing on the final model 
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
