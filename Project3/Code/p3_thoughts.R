#Notes from class, 04/01/2026
Interested variables: age, diabetes, and BP (systolic) (want for sure to be in the model)
Not sure if should be included: prevalent CHD, BP meds (anti-hypertensive meds), smoking status (current or not), total cholesterol, and BMI
Interested in the baseline version of these.
No, dont look at competing risk (you can comment on that as limitations)
Truncate data at 10 years (not interested in time beyond that)
Not interested in events outside stroke
Only fixed covariates only, not time-varying covariates
Descriptive of whether variables are changing over time, which will impact decision of whether future analysis should be time-varying
Descriptive on whether diabetes and systolic BP changes as time-varying, not others
Separate models by gender, interested in factors of stroke for both sexes
Exclude those who have had stroke at baseline

#Notes from class on 04/06 and 04/08
The variables in both models may be the same, but if theyre different you may need to explain
Clarify that the purpose of the model is variable selection, vs prediction
As much n in the model as possible
Estimates for 10 year survival for ages 40, 50, 60; at 0 risk factors, diabetes, high BP, high BP & diabetes + your own scenario (5 scenarios)
	baseline, diabetes, and someone with 0 risk factors, and someone with high BP (>160 systolic), someone with diabetes, high BP & diabetes, + additional profile you think is important.
In coming up with baseline risk profile, average out the continuous.




Good to dos and analysis plan (see Preliminary Analysis Plan document too):
KM plot of time to stroke by risk factors
Work on a flow chart diagram, see past notes and feedback on difference between exclusion and missingness
Plot hazard
Univariable analysis with lograngk test
Asses for non-PH in univariable, if non-PH the decision to make change would depend on how much skewed things are or how early or late the plots cross
Assess functional form in univariable
Could use AFT
Table of events by levels of each predictor
Regardless of which variable selection you use, notice its pitfall in the limitations
4 by 4 for change overtime, including NA. And the cell will be proportion of cases in the category (probability transition matrix)
Complete case for all variables that pass the first screen (logrank test), and then final analysis will bring back those with non-missing. 
Collapsing variables
Univariable analysis will have KM plot, Cox PH HR, log-rank test statistic, Schoenfeld test p-value
Other model fit statistics

Median and quartile follow-up estimates
Censored rate
Some form of best subset model
#Include number of events in Table 1
Is there difference in survival by sex? Before and after 10 year cutoff?

Use more than 1 method for CPH test, influenced by events and sample size 
Describe the profiles, indicate who has higher probability than the other





#Questions to ask during office hours or on Canvas
Generally, can we go beyond the 3+5 variables of interest?
  Can we combine CURSMOKE and CIGPDAY variables? CIGPDAY is a continuous variable.
Also, for modelling only (not time varying descriptive analysis), can we use blood glucose (GLUCOSE) instead of DIABETES, although the 10-year probability estimate for those with diabetes will be challenging if this is done.
Should we consider 'educ' variable? Variable description details is not the data dictionary for this variable and we don't know what the 4 levels of this variable mean.
	Should diastolic BP (DIAB), heart rate (HEARTRTE) be considered? What about prevalent angina (PREVAP), myocardial infarction (PREVMI), hypertensive (PREVHYP)? How about HDLC and LDLC, although these 2 variables will be strongly correlated with  total cholesterol (TOTCHOL)?
In the project description on Canvas, you wrote that "Your analysis should only be conducted using the first 10 years of follow-up." And this is implemented as recoding FU longer than 10years to 10 years with censoring and event status coded appropriately. Is this exactly what is intended or we can use the full follow-up data but only get 10-year survival probability at 10-years?

Response (from attending office hours on 04/10)
1)Focus on binary variable, CURSMOKE/CIGPDAY. Same applies to diabetes/glucose
2)Do not consider other variables. Just the 3+5 specified
3)Code the FU time using the ifelse() function where times higher than 10years is coded back to 10 years. Code the events accordingly.