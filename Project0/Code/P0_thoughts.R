

#Questions for investigators
1)Perhaps the study was prospectively designed. Can the study protocol be shared regardless (prospective or retrospective)?
  It should contain information on target effect size, alpha, and power, as well as pre-specified study design. This will guide whether
  there is, in general, an interest in inference (p-values) for this project

2)Can the research question for Q3 be more specific? Compare 30min to waking or lunch to waking or 10hr to 30min or all pairwise comparison

3)Ask and check whether the sample collection days are sequential (back-to-back).


#Critique
1)A detailed/accurate quadratic or polynomial pattern may not be sussed out from just 4 sample collection timepoints. You technically only need
  3 data time points for a quadratic fit and 4 for a cubic fit (Which may be an overkill).
2)Of all the sample collection time points, the lunch timepoint MAY be the most inconsistent because of the difference in individuals lunch time,
  unlike the other timepoints that is ancored to wake time. Unless there is a biological (metabolic/ism) reason for requesting the lunch timepoint
  instead of say, 5 hours after waking. The same patient can have lunch at different times over these days of sample collection
3)Heterogeneity in these markers by other unmeasured biological variables including age, gender
4)[comment in class]Repeated lab assay for the cortisol and DHEA levels in the saliva?
  

#Analysis ideas
1)Average over the 3 measurement days within each individual and timepoint. Justify this by checking for the presence or absence
  of day effect using varied approaches including checking for interaction between collection time and days 
  (remember to include polynomial terms in the assessment) or time specific intraclass correlation coefficient 
  (see BIOS 6643 notes) or just change in other fixed coefficients in the model between aggregated days and unaggregated.
  
2)Remember GAMs for non-linear effects

3)Bland and Altman plot for agreement [Q1], GLHT for adherence to 30min & 10hr [Q2], 
  modelling and some linear hypothesis test for changes in markers overtime [Q3]. If a hypothesis test is needed, then this may impact 
  choice of analytic method (and GAM may not be appropriate)

4)Decide between random intercept and/or trend using LRT. Then test for significance of nesting of day within subject, and then significance of 
  random effect for day within subject and if not significant, then day will be a fixed effect. 

5)Marginalize over day (DAYNUM) for visualization/plots, you can also see what plot looks like for min, max compared to average.


#Lookup
1)Bland and Altman plots for aggrement for continuous data (would need to convert time to numeric data though)
2)GLHT for covariates with non-linear (polynomial fits)


Class notes
#on 01/26
Q1: Correlation for the first objective (scatter plot) and also see if there is bias between the treatments
Q2: Descriptive for Q2, proportion/categories of the difference between actual and reported. Good adherence (7.5mins) and adequate (15mins) 
If the observed trend is similar to a quadratic curve. Rate of decline after peak and change from wake up to peak.
USe mmol/L measurement
Assume sample size is adequate the research questions
Expected ranges cortisol (nmol/L: >26 possible but not very likely [do not remove as is biologically pluasibl]; >80 is likely artefact)
Expected ranges DHEA (nmol/L: <= 5.205) Review and consider excluding cases with such high values. The 5.205 should be set to NA too.
Linear mixed effects model
Q1: Average over repeated measures but may not be a v good idea; include a scatter plot; tabulate missingness between booklet and c
Q3: Check booklet and cap time, see if results are similar (hopefully they agree)

#on 01/28
Q1: interpret the meaning of slope and intercept in a lmm, ignore days or check if significant
Q2: do per day and later combine if no difference
For adherence, straddle the time into 7.5mins early vs 7.5mins late (as defined in the last class) instead of putting it all on late. Do the latter and see how results compares
#instructor discussion
Q1: Scatter plot of CAP (y-var) by BOOK (x-var) with random intercept for subject
  Interpret the intercept and slope for the investigator
  Calculate CAP and BOOK times from sleep diary wake time 

Q2: Describe, maybe CI for proportion. 7.5minutes from either direction

Q3: Is there significant increase from 0 to 30, and rate of decline. 
  Piecewise linear with knot at 30mins using LMMs


#On 02/02/2026
DHEA values = 5.205 should be NA, this is the limit of detection of the assay.
Cortisol of >80 should be NA.
Plot with actual data points
Include confidence intervals 


#Coding
1)Collection smaple as categorical or recode to hours (challenge with lunch timepoint). I think recoding as hour is most appropriate.
2)Be cognizant of missingness and do so detailed assessment of it.


