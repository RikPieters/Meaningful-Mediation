This program estimates Statistical Power of Discriminant Validity between Mediator and Outcome
or between Mediator and Mediator.

It can also indicate the required sample size needed for Discriminant Validity, 
given the reliability of measures and their correlation

It assumes a two-factor - single-indicator CFA.

Required information:
1. sample size of the study
2. reliability estimate for Mediator (M)
3. reliability estimate for Outcome (Y)
4. "observed" (raw) correlation between the measures of Mediator and Outcome 

The same holds if Mediator-Mediator relationships are explored.

For Mediator (M) and Outcome (Y) measured with multiple items: 
Cronbach's alpha reliability is an estimate of the true-variance captured.

For Mediator (M) and Outcome (Y) measured with a single item:
reasonable estimates of "single-item reliability" for the specific item and research context are needed.
A range of estimates can be used to explore the sensitivity of the findings.
See "Meaningful Mediation Analysis" for some pointers.

DO:

1. Install the Mplus program (trial or full version) from www.statmodel.com
2. Change the four indicated input values in the Mplus input file (".inp")
3. Run the Mplus program (e.g., by opening the ".inp" file and then running it)
4. Inspect the output

Note: Statistical power is "the percentage of replication samples that is statistically significant at p < .05." 

Note: Changing the sample size until this "percentage of significant samples" is larger than the 80% cutoff 
indicates the required sample size for statistical power at this power cutoff.
