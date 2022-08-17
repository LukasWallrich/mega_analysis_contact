# Data search and preparation

## Search strategy

- Search Google Scholar with keywords: , assess all results since 2017 (full 5 years) on first 10 pages
- Search OSF
- Known social survey datasets
- Download codebooks and datafiles where open
- Otherwise, contact authors with x reminders

- Inclusion criteria similar to the seminal meta-analysis by Tropp & Pettigrew (2006): 
    - must measure actual contact (or contact opportunities in a small group - e.g., classroom or work team - not in a large group - neighbourhood or company)
    - must measure prejudice as individual-level DV (in addition, behavioural indicators considered)

## Data processing

- Identify all variables measuring contact, intergroup attitudes or behaviours
- Create scales as per original publication and/or questionnaire design
- Recode: 
    - gender into male/female/other

- Categorical data: Rovers et al. (2007) recode everything into dichotomous categories as this is least granular level of measurement available in IPD
- Different continuous scales: Karyotaki et al (2017) standardize different measures of depression and then combine
- Categorical data / groups: 
    - for usual d to r conversion, equal-sized groups are assumed - in which case standardisation is identical to contrast coding. So do that here (add simulation study) - and recode categorical accordingly?

- Can follow one-step or two-step approach - one step approach better for sparse data, but with many large samples, results will be very similar. For two stage approach, only effect size measures need to be commensurate, less standardisation needed (Brunner et al., 2022)

- Missing data: multiple imputation - at item level, since that substantially increases power and precision compared to imputing scale scores (Gottschall, West & Enders, 2012). Given that few datasets contained more than 10% missing data, 10 imputations run (CITE from PhD). Alternatives: proration (i.e. ignoring item-level missing data and averaging responses given - often leads to bias, Mazza et al., 2015)

## Analysis strategy

- Normal meta-analysis

- Multilevel model, accounting for nesting within participants and within studies

- Create forest plot

- For non-linear interactions, consider restricted cubic spline functions - possibly using two-stage model (i.e. fitting it to each dataset, then meta-analysing coefficients) - see Riley et al. (2020)
- Alternatively, calculate some exposure curves for each sample and then meta-analyse weighted averages (Darssan et al., 2021)

## Rationale

Key points from Riley (2009)

- Meta-analysis reliant on reported summary statistics subject to publication bias and selective reporting within studies (Riley, 2009)
- Greater transparency and uniformity regarding analytic choices (particularly missing data)
- Change to verify reproducibility of original studies
- Covariates can be consistently adjusted for
- Much greater power to test predictive power of individual-level characteristics
- Allow for modeling on non-linear trends
- Allows for interaction tests of participant-level characteristics - cannot be done at higher level due to ecological fallacy

## References

- Stewart & Parmar (1993): individual participant meta-analysis as the "gold standard"
- Stewart & Cochrane Working Group (1995): first systematic guidance
- Riley (2009): summary of benefits and approach
- PRISMA-IBP guidelines on reporting 
    - Flow diagram - requires full search: https://prisma-statement.org/documents/PRISMA%20IPD%20Flow%20Diagram.pdf (not efficient here - need to justify)
    - Checklist: https://prisma-statement.org/documents/PRISMA%20IPD%20checklist.pdf
- Riley et al (2015): complex article on estimating model with various outcomes - unclear what it adds over standard lmer model, but worth checking closely at some point
- Dewidar et al (2021): PRIME process for data preparation - including 5 imputations
- Presently moving beyond - into education (Brunner et al., 2022)
- Pustejovksy & Tipton (2022): add RVE estimation on top of multi-level model
- Viechtbauer W, Cheung MWL. Outlier and influence diagnostics for meta-analysis. Res Synth Methods. 2010;


## ToDos

- Systematic search for examples outside medicine - see terms by Riley, Simmons and Look (2007)
- Consider what to include in terms of quality assessment
- Decide on use of survey weights - little cost, but likely not relevant?
- Think: is there a difference between merging standardised scales (that are not fully harmonized) and averaging resulting effect sizes? Latter is what meta-analyses are all about - former seen as very problematic ... 
- Consider: imputation model must at least be as general as the target model (Brunner et al., 2022) - so interaction terms in imputation? Does that even apply to mice?
- Report prediction intervals
- timesaveR: consider missing data in make_scales (fiml?)

## Report

- No protocol for analyses at start, as procedure was unclear - but developed and registered after 20 datasets were included (logically similar to train/test approach where part of the data is used for model development and remainder for testing). Should then also report with datasets identified after first 20.
- 

## Discussion

- Standardised repositories
- Big data vs good data
- Potential to include aggregate data in multi-level model to get more precise estimates of average effect size (proposed by Riley, Simon and Locke, 2007, but not really of interest here)


## Notes on individual articles

- hon2021a and hon2021b filter Whites differently - a) White only, not mixed, while b) any White ... accident or p-hacking?