# IPD-meta

Welcome to the IPD-meta project repository. This project encompasses predictive survival analysis and meta-analysis components, designed to provide a comprehensive evaluation of clinical outcomes.

## Directory Structure

```
IPD-meta/
├── code/              # All source code is stored here.
└── figure/            # Core result figures are stored here.
```

## 1. Predictive Survival Analysis

This section involves summarizing individual patient data from 7 clinical study cohorts. Patients are categorized into 4 groups based on their receipt of immunotherapy and the presence of emotional distress: ED/ICI, NED/ICI, ED/CT, NED/CT.

- **Objective**: Calculate the Hazard Ratio (HR), 95% Confidence Interval (CI), and p-values for ICI and CT patients within the two emotional distress groups. Additionally, calculate these metrics for ED and NED patients within the two treatment groups.
- **Output**: Generate survival curves to assess differences between the four groups.
- **Deliverables**: Two separate survival analysis figures for Overall Survival (OS) and Progression-Free Survival (PFS).

## 2. Meta Analysis

In this section, we focus on selected randomized controlled trials and single-arm trials to categorize patients into ICI and CT cohorts.

- **Randomized Controlled Trials**: Patients receiving atezolizumab are assigned to the ICI cohort, irrespective of prior platinum-based chemotherapy.
- **Single-Arm Trials**: All individual patients are divided into ICI cohorts, resulting in a total of 12 cohorts.
- **Objective**: Utilize the HR and 95% CI from each cohort to perform a meta-analysis, with treatment methods as subgroups.
- **Output**: Conduct separate analyses for OS and PFS, culminating in two forest plot figures.

## LICENSE

Code and documents of this work are made available for non commercial research purposes only under __Apache License v2__ license.

## Contacts

- [Jian-Guo Zhou](https://github.com/JianGuoZhou3)

***

[**OncoHarmony Network**](https://zhoulab.ac.cn/)

