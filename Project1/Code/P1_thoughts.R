Collation of Project 1 Class Discussions (on 02/09/2026, )

Read, aggregate, and incorporate comments from Project 1 Analysis Plan and Project 0 grading into this analysis/report

Outcomes: 
  viral load (VLOAD) [Untreated:300 to 500; the million VLOAD is plausible (usually high)]
  CD4+ T cell count (LEU3N), 
  aggregate physical quality of life score (AGG_PHYS), 
  aggregate mental quality of life score (AGG_MENT)
No ranking among the 4 outcomes of interest, they’re all needed.

Covariates (per investigator):
  age, BMI, smoking status, education, and race/eth, and adherence (important predictor)

Per instructor: 
    Year 0 (use), Year 1 (not use), focus on year 2, not year 2 above 
    Baseline drug use only (year=0)
    This is an observational, not randomized, study, so we care about baseline values.
    Clinically meaningful effect (2-sided) ==> VLOAD: +/- log10 0.5 change, CD4: +/- 50 cells/ml, QoL: +/- 2pts
    Use N with data for all outcomes ~ 476
    
Research question: 
    Response to HAART between baseline and year 2? Especially for VLOAD and LEU3N (Instructor said this is not of much interest)
  Key:  Difference in treatment outcomes at Year-2 between baseline hard drug users vs non-users? 
        If there is difference by hard drug status (in any of the 4 outcomes), is this difference explained by adherence status?
  2 Research questions

Question to investigator/instructor (these were clarified during 02/13 office hours):
    Clarify year=0 and year=2 are only of interest, yes focus on year=0 and year=2 only
    Intro section should be limited to what is on Canvas, yes
    Since Bayesian and non-Bayesian and both needs to be explained, are we still limited by the page requirements?, yes
    Use longitudinal data approaches or extract baseline values and analyze as flat, if left to me, then use longitudinal methods, you could do this but it is not necessary and makes the Bayesian analysis harder
    Do not go looking for variables to adjust for, just focus on the listed ones, yes
    To understand the rationale for year=2, is it the case that the duration of HAART is 2 years such that 1 is intermediate time and 3+ is follow-up? (not asked) again, focus on 1 and 2
    The point of frequentist is largely for sanity check
    
Next questions to ask investigators:
    Can codebook be on GitHub or add to .gititnore?
    Always adjust for specified variables, no model building for these predictors? How about pairwise interactions between covariate pairs?
    It makes sense to filter our outliers and/or unreasonable values before creating Table 1 (I think so), including log10 and other tranformations?
    

Analysis ideas per me:
    Model structure: 2-yr response = baseline_response + hard_drugs + covariates
    Model structure: 2-yr response = baseline_response + hard_drugs + hard_drugs:adherence + covariates
    For response/outcome variable, choice of modelling change (between year 0 and 2) vs modeing year=2 and adjusting for year=0 as covariate

Todos per me:
  Plot the relationships among the 4 outcomes, especially the CD4 and VLOAD
  Take a look at missing data and drop out, consider doing a missing data analysis.  
  Check how Bayesian/cmdstanr behaves when there are missing observations for one of the data points (i.e., does it do case-wise deletion)
  Geometric meaning of modelign difference between t1 and t0 vs t1 as outcome and adjust for t0
  Pick out year=0 and year=2; indicator function for baseline hard drug use
  Sample size flow chart, and missingness
  Comment in the report on how dropout may impact result, including dropout from baseline to year=2, and missing observations
  Differentiate between dropout and missingness
  
  
  #Extract the label of all variables and paste here.
  #newid: deidentified ID
  AGG_MENT: SF36 MCS score 
  AGG_PHYS: SF36 PCS score 
  HASHV: Hash/marijuana since last visit | 1=No, 2=Yes, Blank=Missing
  HASHF: Frequency used hash/marijuana since last visit | 0=Never,1=Daily,2=Weekly,3=Monthly,4=Less often,''=Missing 
  income: Individual gross income | 1=<10k, 2=10-19.9k, 3=20-29.9k, 4=30-39.9k, 5=40-49.9k, 6=50-59.9k, 7=60k+, 9=No answer
  BMI: Body Mass Index (in kg/meter**2) based on earliest non-missing height
  HBP: High blood pressure (SBP >=140 or DBP >=90 or diagnosed with hypertension & use of medications) | 1=No, 2=Yes, 3=No, based on data trajectory, 4=Yes from data trajectory, 9=insufficient data, -1=Improbable value
  DIAB: Diabetes (GLUC2 >= 126 or diagnosed with diabetes and use of medication) | Same label as HBP
  LIV34: Liver disease stage 3/4 (SGPT or SGOP > 150) | 1=No, 2=Yes, 9=Insufficient data
  KID: Kidney disease (EGFR < 60 or UPRCR >= 200) | Same label as HBP
  FRP: Frailty Related Phenotype (3 out of 4 conditions = YES: WTLOS, PHDWA, HLTWB, HLTVA) | 1=No, 2=Yes, 9=Insufficient data
  FP: Frailty Phenotype (3 out of 5 conditions = YES: WTLOS, PHWDA, HLTVA, SLOW, WEAK) | 1=No, 2=Yes, 9=Insufficient data
  TCHOL: Total Cholesterol (mg/dl)
  TRIG: Triglycerides (mg/dl)
  LDL: Low Density Lipoprotein (fasting) (mg/dl)
  DYSLIP: dyslipidemia at visit [many definitions] | Same label as HBP
  CESD: Depression scale (>=16 is depressed)
  SMOKE: Smoking status | 1=Never smoked,2=Former smoker, 3=Current smoker, ''=Missing
  DKGRP: Alcohol use since last visit | 0=None,1=1 to 3 drinks/week,2=4 to 13 drinks/week,3= >13 drinks/week,''=Missing
  HEROPIATE: Took heroin or other opiates since last visit | 1=No, 2=Yes, -9=Not specified in form, ''=Missing
  
  IDU: Took/used drugs with a needle since last visit? | 1=No, 2=Yes, ''=Missing
  LEU3N: Number of CD4 positive cells (helpers), (in cells)
  VLOAD: Standardized viral load, (in copies/ml)
  ADH: Adherence to meds taken since last visit | 1=100%, 2=95-99%, 3=75-94%, 4= <75%, ''=Missing
  RACE: Race | 1=white nH, 2=White H, 3=Black nH, 4=Black H, 5=AIAN, 6=Asian or PI, 7=Other, 8=Other Hispanic, ''=Missing
  EDUCBAS: Baseline or earliest reported education (highest grade or level) | 1=8th grade or less, 2=9 to 11th grade, 3=12th grade, 4=at least 1 year college (but no degree), 5=four years college (got degree), 6=some graduate work, 7=post-graduate degree, ''=missing
  hivpos: HIV Serostatus | 0=Negative, 1=Positive
  age Age at visit 
  everART: Ever Took ART | 0=No, 1=Yes
  years: years since initiating ART | 0=baseline visit (before ART), 1=1 year, ..., 8=8years, ''=Missing
  hard_drugs: Hard drug use (either injection drugs or illicit heroin/opiate use) since last visit | 0=No, 1=Yes, ''=Missing
  
  
  
  
Insights from PSA worksheet that may be helpful 
PSA outcome is skewed; prostate weight, cancer volume may be skewed
BPH lot of zeros; most assumptions is about the error. May attempt to binarize BPH
Right skew: log transform; big outlier (ask investigator; said remove)
Analysis plan, building strategies
•	Log(PSA) (1A) univariable association (1/2 normal sigma, non-informative for regression coefficients ~ N(0, big variance))
•	Then (1B) multivariable model
•	Then (2A) SVI by individual variable interactions
8 predictor (2 dummies for Gleason score); 97 obs
