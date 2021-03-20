# Using the Proportional Hazards Cure Model to Improve the Study of International Relations
R code and data for replicating the analysis in: George W. Williford. 2021. ``Using the Proportional Hazards Cure Model to Improve the Study of International Relations.'' 

## Project Description

Survival analysis has become an essential tool used by political scientists to study the timing and onset of diverse phenomena. However, scholars often use these models without regard for one of the fundamental assumptions they make, namely, that all observed subjects eventually experience the event of interest. Political scientists are often interested in events that could only feasibly occur among a subset of the subjects in their samples. Subjects that are not at risk of experiencing the event are often described as "cured" or "immune" to the event. Using standard models to analyze such data clearly violates the assumption above and may result in biased and inefficient coefficient estimates and lead scholars to make incorrect inferences. 

Cure models account for the presence of cured observations by modeling the probability of being at risk of experiencing an event of interest and reweighting the estimates of the hazard rate accordingly. This dissertation chapter makes three primary contributions. First, it introduces political scientists to the proportional hazards cure model (PHCM). Compared to the parametric cure models that have been used in political science thus far, the PHCM provides a flexible alternative that does not depend on restrictive distributional assumptions. Second, I present new software that I developed to estimate these models in R using time-varying covariates. Third, I demonstrate the potential advantages of using cure models by replicating an analysis of civil conflict recurrence.

I revisit an article on civil war recurrence published by Loyle and Appel (2017). Estimation of the models was performed using my R package, tvcure, which can be found [here](github.com/gwilliford/tvcure). I replicate Loyle and Appel's (2017) analysis using a Cox proportional hazards model and a proportional hazards cure model in order to demonstrate that the use of a cure model leads to different substantive inferences. I demonstrate, among other things, that the use of a standard Cox proportional hazards model produces inflated estimates of the substantive effects of implementing postconflict justice processes in the wake of civil wars.

## Methods Used

- Cure models
- Survival Analysis

## File Descriptions

- ISQ_FINAL_DATA.dta - Stata file containing replication data from Loyle and Appel (2017).
- Loyle and Appel 2017 Replication Final.R - R code for replicating the analysis conducted in this article.
- Chapter2.pdf - A copy of the article

## References

Loyle, Cyanne E and Benjamin J Appel (2017). “Conflict Recurrence and Postconflict Justice: Addressing Motivations and Opportunities for Sustainable Peace”. International Studies Quarterly 61 (3), pp. 690–703.  

