# This code is mainly divided into predictive survival analysis and meta analysis
## 1、Predictive survival analysis
Summarize individual patient data from 7 clinical study cohorts and divide patients into 4 groups based on whether they have received immunotherapy and whether they have emotional distress: ED/ICI; NED/ICI; ED/CT; NED/CT.
Calculate the HR, 95% CI, and p values of ICI and CT patients within the two groups based on ED statu; and then calculate these of ED and NED patients within the two groups based on treatment.
Finally draw a survival curve to observe whether there are differences between the four groups.


Analyze OS and PFS separately and finally obtain two survival analysis figures.
## 2、Meta analysis
For each chosen randomized controlled trials we include, patients who received atezolizumab were assigned into the ICI cohort regardless of whether they had received platinum-based chemotherapy prior to that period,
patients who did not receive atezolizumab were assigned to the chemotherapy (CT) cohort. 
For each single-arm trials, divided all individual patients into ICI cohorts. A total of 12 cohorts were obtained.

Based on the HR and 95% CI of each cohort, conduct meta-analysis with treatment methods as subgroups. Analyze OS and PFS separately and finally obtain two forest figures.
