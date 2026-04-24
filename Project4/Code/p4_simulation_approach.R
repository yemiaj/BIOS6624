#Layout of the required task for this project

Methods
  1) F-test and p-value
  2) AIC
  3) BIC
  4) LASSO (assess 2 differnet choices of lambda)
  5) Elastic net (assess 2 differnet choices of lambda)

Scenarios
  1) N = 250, P = 20
    BETA_1:5 = 0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3
    1a) Indepence among Xs
    1b) Correlation among Xs

  2) N = 500, P = 20
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


