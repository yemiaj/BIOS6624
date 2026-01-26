

#Questions for investigators
1)Perhaps the study was prospectively designed. Can the study protocol be shared regardless (prospective or retrospective)?
  It should contain information on target effect size, alpha, and power, as well as pre-specified study design. This will guide whether
  there is, in general, an interest in inference (p-values) for this project

2)Can the research question for Q3 be more specific? Compare 30min to waking or lunch to waking or 10hr to 30min or all pairwise comparison

#Critique
1)A detailed/accurate quadratic or polynomial pattern may not be sussed out from just 4 sample collection timepoints. You technically only need
  3 data time points for a quadratic fit and 4 for a cubic fit (Which may be an overkill).

#Analysis ideas
1)Average over the 3 measurement days within each individual and timepoint. Justify this by checking for the presence or absence
  of day effect using varied approaches including checking for interaction between collection time and days 
  (remember to include polynomial terms in the assessment) or time specific intraclass correlation coefficient 
  (see BIOS 6643 notes) or just change in other fixed coefficients in the model between aggregated days and unaggregated.
  
2)Remember GAMs for non-linear effects

3)Bland and Altman plot for agreement [Q1], GLHT for adherence to 30min & 10hr [Q2], 
  modelling and some linear hypothesis test for changes in markers overtime [Q3]. If a hypothesis test is needed, then this may impact 
  choice of analytic method (and GAM may not be appropriate)

#Lookup
1)Bland and Altman plots for aggrement for continuous data (would need to convert time to numeric data though)
2)GLHT for covariates with non-linear (polynomial fits)
