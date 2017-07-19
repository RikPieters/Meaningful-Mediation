##################################################################################
# Kupor and Tormala (2015, Study 3) - Using SSD
##################################################################################

# Clean the workspace
rm(list=ls())

# load package lavaan (if not installed yet, do so first)
library(lavaan) 

##################################################################################
# TWO PARTS
# PART 1: PROVIDE SUMMARY STATISTICS DATA
# PART 2: SPECIFY AND ESTIMATE THE MODEL
##################################################################################

##################################################################################
# PART 1: PROVIDE SUMMARY STATISTICS DATA (SSD) IN FIVE STEPS
##################################################################################
# Step 1: provide correlation matrix
# Step 2: provide vector of means
# Step 3: provide vector of standard deviations
# Step 4: provide names of the variables - in order
# Step 5: provide sample size
##################################################################################

##################################################################################
# four models are estimated:
# Model 1a is the model reported in K&T (2015). It is the unstandardized solution
# Model 1b is Model 1 from Appendix in Meaningful Mediation Analysis. 
#          It is partially standardized (ps), see later
# Model 2  is Model 2 from Appendix in Meaningful Mediation Analysis (ps)
# Model 3  is Model 3 from Appendix in Meaningful Mediation Analysis (ps)
##################################################################################

##################################################################################
# Model 1a
##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
##################################################################################

lowercor <- "
1.000
 .161 1.000
 .159  .207 1.000
 .213  .356  .415 1.000
"
##################################################################################
# Step 2: Provide vector of means 
##################################################################################

means1 = c(0.510, 6.583, 0.594, 0.025)

##################################################################################
# Step 3: Provide vector of standard deviations 
##################################################################################

stdev1 = c(.501, 2.181, 0.357, 0.796)

##################################################################################
# Step 4: Provide variable names, each between ""
##################################################################################

namesvar = c("X", "M1", "M2", "Y")

##################################################################################
# Step 5: Provide sample size
##################################################################################

sampsiz = c(223)

##################################################################################
# Lavaan compiles SSD
##################################################################################

KT_exp3_M1a.ssd <- 
  getCov(lowercor, sds = stdev1, names = namesvar)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL 1a
##################################################################################

# Specify the model (Model 1a unstandardized)
KT_M1a.model <- "
  # Regressions
    M1 ~ a1*X
    M2 ~ a2*X + d*M1
    Y ~ b1*M1 + b2*M2 + cp*X
    # Intercepts
    M1 ~ 1
    M2 ~ 1
    Y ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b1      := a1*b1
      a2b2      := a2*b2
      a1db2     := a1*d*b2
    # Total indirect effect of Condition
      total_ind := a1*b1 + a2*b2 + a1*d*b2
    # Total effect of Condition
      total     := a1*b1 + a2*b2 + a1*d*b2 + cp
    # Proportion Mediated    
      pm        := a1db2 / total      # proportion mediated of total treatment effect
      pm_ind    := a1db2 / total_ind  # proportion mediated of indirect treatment effect
"
# Estimate model
fit1a <- sem(KT_M1a.model, 
             sample.cov = KT_exp3_M1a.ssd, 
             sample.mean = means1,
             sample.nobs = sampsiz)               
summary(fit1a, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)  # note that ci = Normal CIs

##################################################################################
# Model 1b
##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
#         Same as before -- one could just refer to previous (identical corr matrix)
##################################################################################

lowercor <- "
1.000
.161 1.000
.159  .207 1.000
.213  .356  .415 1.000
"
##################################################################################
# Step 2: Provide vector of means (for partially standardized): Ms and Ys are 0 
##################################################################################

means2 = c(0.510, 0, 0, 0)

##################################################################################
# Step 3: Provide vector of stdevs (for partially standardized): Ms and Ys are 1
##################################################################################

stdev2 = c(.501, 1.00, 1.00, 1.00)

##################################################################################
# Step 4: Provide variable names between " -- Same as before
##################################################################################

namesvar = c("X", "M1", "M2", "Y")

##################################################################################
# Step 5: Provide sample size -- Same as before
##################################################################################

sampsiz = c(223)

##################################################################################
# Lavaan compiles SSD 
##################################################################################

KT_exp3_M1b.ssd <- 
  getCov(lowercor, sds = stdev2, names = namesvar)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL 1b
##################################################################################

# Specify the model (Model 1b partially standardized. M1 -> M2)
KT_M1b.model <- "
  # Regressions
    M1 ~ a1*X
    M2 ~ a2*X + d*M1
    Y ~ b1*M1 + b2*M2 + cp*X
    # Intercepts
    M1 ~ 1
    M2 ~ 1
    Y ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b1      := a1*b1
      a2b2      := a2*b2
      a1db2     := a1*d*b2
    # Total indirect effect of Condition
      total_ind := a1*b1 + a2*b2 + a1*d*b2
    # Total effect of Condition
      total     := a1*b1 + a2*b2 + a1*d*b2 + cp
    # Proportion Mediated    
      pm        := a1db2 / total      # proportion mediated of total treatment effect
      pm_ind    := a1db2 / total_ind  # proportion mediated of indirect treatment effect
"
# Estimate model
fit1b <- sem(KT_M1b.model, 
             sample.cov = KT_exp3_M1b.ssd, 
             sample.mean = means2,
             sample.nobs = sampsiz)               
summary(fit1b, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)  # note that ci = Normal CIs

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL 2
##################################################################################

# Specify the model (Model 2 partially standardized. reverse M2 -> M1)
KT_M2.model <- "
  # Regressions
    M1 ~ a1*X + d*M2
    M2 ~ a2*X
    Y ~ b1*M1 + b2*M2 + cp*X
    # Intercepts
    M1 ~ 1
    M2 ~ 1
    Y ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b1      := a1*b1
      a2b2      := a2*b2
      a1db2     := a1*d*b2
    # Total indirect effect of Condition
      total_ind := a1*b1 + a2*b2 + a1*d*b2
    # Total effect of Condition
      total     := a1*b1 + a2*b2 + a1*d*b2 + cp
    # Proportion Mediated    
      pm        := a1db2 / total      # proportion mediated of total treatment effect
      pm_ind    := a1db2 / total_ind  # proportion mediated of indirect treatment effect
"
# Estimate model
fit2 <- sem(KT_M2.model, 
             sample.cov = KT_exp3_M1b.ssd, 
             sample.mean = means2,
             sample.nobs = sampsiz)               
summary(fit2, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)  # note that ci = Normal CIs


##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL 3
##################################################################################

# Specify the model (Model 2 partially standardized. reverse M2 -> M1)
KT_M3.model <- "
  # Regressions
    M1 ~ a1*X
    M2 ~ a2*X
    M1 ~~ M2   # correlated errors
    Y ~ b1*M1 + b2*M2 + cp*X
    # Intercepts
    M1 ~ 1
    M2 ~ 1
    Y ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b1      := a1*b1
      a2b2      := a2*b2
    # Total indirect effect of Condition
      total_ind := a1*b1 + a2*b2
    # Total effect of Condition
      total     := a1*b1 + a2*b2 + cp
"
# Estimate model
fit3 <- sem(KT_M3.model, 
            sample.cov = KT_exp3_M1b.ssd, 
            sample.mean = means2,
            sample.nobs = sampsiz)               
summary(fit3, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)  # note that ci = Normal CIs
##################################################################################
# Kupor and Tormala (2015)
# Statistical Power to obtain the Serial Mediation Effect from Model 1b
##################################################################################

# Package lavaan already loaded

# Load package simsem (SIMulated Structural Equation Modeling)
library(simsem)

# Note estimates in population model are based on SSD not the raw data
# Thus, results are approximate not exact

popModel <- "
  # Regressions
    X  ~  .510*1                      # mean of X (note: value * 1)
    X ~~  .251*X                      # variance of X (= sd^2)
    M1 ~  .321*X                      # estimate from M1b
    M2 ~  .258*X + .186*M1            # estimates from M1b
    Y  ~  .267*M1 + .341*M2 + .231*X  # estimates from M1b
    M1 ~~ .970*M1                     # residual variance of M1 from M1b
    M2 ~~ .937*M2                     # residual variance of M2 from M1b
    Y ~~  .735*Y                      # residual variance of Y  from M1b
"
analysisModel <- "
  # Regressions
    X  ~ .510*1 
    X ~~ .251*X
    M1 ~ a1*X 
    M2 ~ a2*X + d*M1
    Y  ~ b1*M1 + b2*M2 + cp*X
    M1 ~~ .970*M1
    M2 ~~ .937*M2
    Y ~~  .735*Y
  # Conditional Effects
    # Indirect Effect
      a1b1      := a1*b1
      a2b2      := a2*b2
      a1db2     := a1*d*b2
    # Total indirect effect of Condition
      total_ind := a1*b1 + a2*b2 + a1*d*b2
    # Total effect of Condition
      total     := a1*b1 + a2*b2 + a1*d*b2 + cp
    # Proportion Mediated    
      pm        := a1db2 / total      # proportion mediated of total treatment effect
      pm_ind    := a1db2 / total_ind  # proportion mediated of indirect treatment effect
"
Output1 <- sim(1000,               # Number of replications
              analysisModel,       # Name of the analysis model
              n = 223,             # Sample size
              estimator = "ml",   
              generate=popModel,    
              lavaanfun="sem",      
              seed=12345) 
summaryPopulation(Output1)
summaryConverge(Output1)
summary(Output1, alpha=0.05) 

##################################################################################
# Re-direct output from the console to path/file
##################################################################################

sink("C:/Kupor&Tormala(2015)_SSD_Model_1_3_power.txt")  # save output in this path
# header
cat("######################################################################")
cat("\n")
cat("Kupor and Tormala (2015, Study 3) - SSD - Model 1a")    # Model 1a
cat("\n")
cat("######################################################################")
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 1a is sequential mediation 1 (unstandardized solution)")
cat("\n")
cat("This model 1a is reported in the Kupor and Tormala (2015) paper")
cat("\n")
summary(fit1a, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
cat("\n")
# header
cat("######################################################################")
cat("\n")
cat("Kupor and Tormala (2015, Study 3) - SSD - Model 1b")    # Model 1b
cat("\n")
cat("This Model 1b is in the Appendix of Meaningful Mediation")    # Model 1b
cat("\n")
cat("######################################################################")
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 1b is sequential mediation 1 (partially standardized solution)")
cat("\n")
summary(fit1b, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
# header
cat("######################################################################")
cat("\n")
cat("Kupor and Tormala (2015, Study 3) - SSD - Model 2")    # Model 2
cat("\n")
cat("######################################################################")
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 2 - sequential mediation 2 (partially standardized solution)")
cat("\n")
summary(fit2, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
# header
cat("######################################################################")
cat("\n")
cat("Kupor and Tormala (2015, Study 3) - SSD - Model 3")    # Model 3
cat("\n")
cat("######################################################################")
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 3 - parallel mediation (partially standardized solution)")
cat("\n")
summary(fit3, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
cat("\n")
# header
cat("######################################################################")
cat("\n")
cat("Kupor and Tormala (2015, Study 3) - SSD - Statistical Power of Model 1b")
cat("\n")
cat("######################################################################")
cat("\n") # adds extra blank line 
cat("\n")
cat("Statistical Power of sequential mediation effect in model 1b")
cat("\n")
summaryPopulation(Output1)
summaryConverge(Output1)
summary(Output1, alpha=0.05) 

##################################################################################
# Stop sinking; Re-direct output from path/file to the console again
##################################################################################

sink()
