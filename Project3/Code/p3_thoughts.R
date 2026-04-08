#Notes from class, 04/01/2026
Interested variables: age, diabetes, and BP (systolic) (want for sure to be in the model)
Not sure if should be included: presence of CHD, BP meds, smoking status (current or not), total cholesterol, and BMI
Interested in the baseline version of these.
No, dont look at competing risk (you can comment on that as limitations)
Truncate data at 10 years (not interested in time beyond that)
Not interested in events outside stroke
Only fixed covariates only, not time-varying covariates
Descriptive of whether variables are changing over time, which will impact decision of whether future analysis should be time-varying
Descriptive on whether diabetes and BP changes as time-varying, not others
Separate models by gender, interested in factors of stroke for both sexes
Exclude those who have had stroke at baseline

#Notes from class on 04/06 and 04/08
The variables in both models may be the same, but if theyre different you may need to explain
Clarify that the purpose of the model is variable selection, vs prediction
As much n in the model as possible
Estimates for 10 year survival for ages 40, 50, 60; at 0 risk factors, diabetes, high BP, high BP & diabetes + your own scenario (5 scenarios)
	baseline, diabetes, and someone with 0 risk factors, and someone with high BP (>160 systolic), someone with diabetes, high BP & diabetes, + additional profile you think is important.
In coming up with baseline risk profile, average out the continuous.
Variables to explore (that she is interested in): Coronary HD, BP meds, smoking status, total cholesterol, BMI
                                                  anti-hypertensive meds, prevalent CHD, smoke, total cholesterol, BMI


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

