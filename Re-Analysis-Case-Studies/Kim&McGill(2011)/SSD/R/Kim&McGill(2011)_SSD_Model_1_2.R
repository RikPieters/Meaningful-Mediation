##################################################################################
# Kim and McGill (2011, Experiment 2) Model 1 and 2
##################################################################################

# This code estimates Model 1 and Model 1 in the Appendix of Meaningful Mediation
# It also estimates (Compositie) Relaibility and Discriminant Validity 
# Highlight the code:
# step 1: right-click "select all"
# step 2: "run" from RStudio, see right-top the "run" symbol

# Clean the workspace)
rm(list=ls())

# load the lavaan package
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
# Step 4: provide names of the variables
# Step 5: provide sample size
##################################################################################

##################################################################################
# For exploration
# Model 1 is estimated three times: 
# Model 1a: unstandardized
# Model 1b: partially standardized (= all endogenous (Ms and Ys) standardized)
# Model 1c: standardized (all variables standardized)
##################################################################################

##################################################################################
# Model 1a: Unstandardized
##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
##################################################################################

lowercor <- "
 1.000
-.048 1.000
 .007  .143 1.000
-.029 -.003  .322 1.000
 .145 -.037 -.271 -.377 1.000
"
##################################################################################
# Step 2: Provide vector of means (for unstandardized solution)
##################################################################################

means1a = c(0.143, 0, -0.480, 4.074, 6.837)

##################################################################################
# Step 3: Provide vector of standard deviations (for unstandardized solution)
##################################################################################

stdev1a = c(.996, 1.006, 1.005, 1.117, 1.634)

##################################################################################
# Step 4: Provide variable names between ""
##################################################################################

namesvar = c("x1_p", "x2_h", "x1x2", "control", "risk")

##################################################################################
# Step 5: Provide sample size
##################################################################################

sampsiz = c(84)

##################################################################################
# Lavaan compiles SSD
##################################################################################

KMCG_exp2.ssd <- 
  getCov(lowercor, sds = stdev1a, names = namesvar)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL
##################################################################################

# Specify model
KMCG_Model1a.model <- "
  # Regressions
    control ~ a1*x1_p + a2*x2_h + a3*x1x2
    risk    ~ cp1*x1_p + cp2*x2_h + cp3*x1x2 + b*control
    # Intercepts 
    control ~ 1
    risk ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b         := a1*b
      a2b         := a2*b
      a3b         := a3*b
    # Total effect of x1x2
      total_X1X2  := a3*b + cp3
    # Proportion mediated of x1x2
      pm          := a3b/(a3*b+cp3)
    # Difference of indirect versus conditional direct effect     
      diff        := a3*b - cp3 
"     ## end model formulation with an " as here in first column

# Estimate model
fit1a <- sem(KMCG_Model1a.model, 
           sample.cov = KMCG_exp2.ssd, 
           sample.mean = means1a,
           sample.nobs = sampsiz)               
summary(fit1a, fit.measures=TRUE, ci=TRUE, rsquare=TRUE) # note that ci = Normal CIs

##################################################################################
# Model 1b: Partially standardized
##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
##################################################################################

lowercor <- "
1.000
-.048 1.000
.007  .143 1.000
-.029 -.003  .322 1.000
.145 -.037 -.271 -.377 1.000
"
##################################################################################
# Step 2: Provide vector of means; 0's for endogenous variables (Ms and Ys)
##################################################################################

means1b = c(0.143, 0, -0.480, 0, 0)

##################################################################################
# Step 3: Provide vector of stand devs; 1's for endogenous variables (Ms and Ys) 
##################################################################################

stdev1b = c(.996, 1.006, 1.005, 1, 1)

##################################################################################
# Step 4: Provide variable names between ""
##################################################################################

namesvar = c("x1_p", "x2_h", "x1x2", "control", "risk")

##################################################################################
# Step 5: Provide sample size
##################################################################################

sampsiz = c(84)

##################################################################################
# Lavaan compiles SSD
##################################################################################

KMCG_exp2.ssd <- 
  getCov(lowercor, sds = stdev1b, names = namesvar)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL
##################################################################################

# Specify model
KMCG_Model1b.model <- "
  # Regressions
    control ~ a1*x1_p + a2*x2_h + a3*x1x2
    risk    ~ cp1*x1_p + cp2*x2_h + cp3*x1x2 + b*control
    # Intercepts
    control ~ 1
    risk ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b         := a1*b
      a2b         := a2*b
      a3b         := a3*b
    # Total effect of x1x2
      total_X1X2  := a3*b + cp3
    # Proportion mediated of x1x2
      pm          := a3b/(a3*b+cp3)
    # Difference of indirect versus conditional direct effect     
      diff        := a3*b - cp3 
"   ## do not forget to end model specification with "

# Estimate the model
fit1b <- sem(KMCG_Model1b.model, 
           sample.cov = KMCG_exp2.ssd, 
           sample.mean = means1b,
           sample.nobs = sampsiz)                
summary(fit1b, fit.measures=TRUE, ci=TRUE, rsquare=TRUE) # note that ci = Normal CIs


##################################################################################
# Model 1c: Standardized
##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
##################################################################################

lowercor <- "
1.000
-.048 1.000
.007  .143 1.000
-.029 -.003  .322 1.000
.145 -.037 -.271 -.377 1.000
"
##################################################################################
# Step 2: Provide vector of means. For standardized solution all 0's
##################################################################################

means1c = c(0, 0, 0, 0, 0)

##################################################################################
# Step 3: Provide vector of stand devs. For standardized solution all 1's 
##################################################################################

stdev1c = c(1, 1, 1, 1, 1)

##################################################################################
# Step 4: Provide variable names between ""
##################################################################################

namesvar = c("x1_p", "x2_h", "x1x2", "control", "risk")

##################################################################################
# Step 5: Provide sample size
##################################################################################

sampsiz = c(84)

##################################################################################
# Lavaan compiles SSD
##################################################################################

KMCG_exp2.ssd <- 
  getCov(lowercor, sds = stdev1c, names = namesvar)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL
##################################################################################

# Specify model
KMCG_Model1c.model <- "
  # Regressions
    control ~ a1*x1_p + a2*x2_h + a3*x1x2
    risk    ~ cp1*x1_p + cp2*x2_h + cp3*x1x2 + b*control
    # Intercepts
    control ~ 1
    risk ~ 1
  # Conditional Effects
    # Indirect Effect
      a1b         := a1*b
      a2b         := a2*b
      a3b         := a3*b
    # Total effect of x1x2
      total_X1X2  := a3*b + cp3
    # Proportion mediated of x1x2
      pm          := a3b/(a3*b+cp3)
    # Difference of indirect versus conditional direct effect     
      diff        := a3*b - cp3 
"   ## do not forget to end model specification with "

# Estimate the model
fit1c <- sem(KMCG_Model1c.model, 
             sample.cov = KMCG_exp2.ssd,
             sample.mean = means1c,
             sample.nobs = sampsiz)                
summary(fit1c, fit.measures=TRUE, ci=TRUE, rsquare=TRUE) # note that ci = Normal CIs

##################################################################################
# Model 2: Structural Equation Model (SEM)
##################################################################################

##################################################################################
# SSD are on item level
# M (Rontrol, aka power in the paper) has four indicators
# Y (Risk) has three indicators
##################################################################################

##################################################################################
# Step 1: Provide lower half of correlation matrix, include 1's on the diagonal
##################################################################################

lowercor2 <- "
1.000
0.515 1.000
0.478 0.327 1.000
0.107 0.192 0.432 1.000
-.188 -.240 -.276 -.234 1.000
-.189 -.142 -.347 -.159 0.776 1.00
-.352 -.180 -.407 -.213 0.705 0.789 1.000
-.060 0.005 -.047 0.023 0.178 0.112 0.109 1.000
-.026 0.041 -.033 0.024 -.060 -.007 -.032 -.048 1.000
0.184 0.113 0.353 0.286 -.166 -.254 -.321 0.007 0.143 1.000 
"
##################################################################################
# Step 2: Provide vector of means (for unstandardized solution)
##################################################################################

means2 = c(4.095, 3.702, 3.810, 4.679, 6.560, 7.274, 6.679, 0.143, 0.000, -0.048)

##################################################################################
# Step 3: Provide vector of standard deviations (for unstandardized solution)
##################################################################################

stdev2 = c(1.836, 1.453, 1.459, 1.489, 1.775, 1.686, 1.865, 0.996, 1.006, 1.005)

##################################################################################
# Step 4: Provide variable names between ""
##################################################################################

namesvar2 = c("pow1", "pow2", "pow3", "pow4", "risk1", "risk2", "risk3",
              "x1_p", "x2_h", "x1x2")

##################################################################################
# Step 5: Provide sample size
##################################################################################

sampsiz = c(84)

##################################################################################
# Lavaan compiles SSD
##################################################################################

KMCG_exp2.cov <- 
  getCov(lowercor2, sds = stdev2, names = namesvar2)

##################################################################################
# PART 2: SPECIFY AND ESTIMATE MODEL
#         Model identification: fixing var(LVs)=1, loadings free
##################################################################################

# Specify model
KMCG_Model2.model <- "
  # Latent Variables
    control =~ NA*pow1 + pow2 + pow3 + pow4  # NA* = free loading of indicator
    control ~~ 1*control                     # LV ~~ 1*LV = fix LV variance to 1
    risk    =~ NA*risk1 + risk2 + risk3
    risk    ~~ 1*risk
  # Regressions
    control ~ a1*x1_p + a2*x2_h + a3*x1x2
    risk    ~ cp1*x1_p + cp2*x2_h + cp3*x1x2 + b*control
  # Conditional Effects
    # Indirect Effect
      a1b         := a1*b
      a2b         := a2*b
      a3b         := a3*b
    # Total effect of x1x2
      total_X1X2  := a3*b + cp3
    # Proportion mediated of x1x2
      pmx1x2      := a3b/(a3*b+cp3)
    # Difference of indirect versus conditional direct effect     
      diff        := a3*b - cp3
"
# Estimate model
fit2 <- sem(KMCG_Model2.model, 
            sample.cov = KMCG_exp2.cov, 
            sample.mean = means2,
            sample.nobs = sampsiz)

summary(fit2, fit.measures=TRUE, ci=TRUE, rsquare=TRUE) # note that ci = Normal CIs

##################################################################################
# RELIABILITY AND DISCRIMINANT VALIDITY
##################################################################################

# Kim and McGill (2011, Experiment 2, Model 2) - Measurement Model
KMCG_Model2.model <- "
  # Latent Variables
    control =~  NA*pow1 + l1*pow1 + l2*pow2 + l3*pow3 + l4*pow4 # multiple modifiers requires multiple terms 
    control ~~ 1*control            # fix variance of LV to 1 to standardize 
    pow1  ~~  e1*pow1
    pow2  ~~  e2*pow2
    pow3  ~~  e3*pow3
    pow4  ~~  e4*pow4
    risk  =~ NA*risk1 + l5*risk1 + l6*risk2 + l7*risk3 # multiple modifiers requires multiple terms 
    risk  ~~ 1*risk                  # fix variance of LV to 1 to standardize                               
    risk1 ~~ e5*risk1
    risk2 ~~ e6*risk2
    risk3 ~~ e7*risk3
  # Correlations
    control ~~ cor*risk              # cor = label
  # Reliability and Discriminant Validity 
    # Composite Reliability of control
    CR_cont   := ((l1+l2+l3+l4)^2)/((l1+l2+l3+l4)^2+e1+e2+e3+e4) 
    # Average Variance Extracted (AVE) control
    AVE_cont  := ((l1^2+l2^2+l3^2+l4^2))/((l1^2+l2^2+l3^2+l4^2)+e1+e2+e3+e4) 
    CR_risk   := ((l5+l6+l7)^2)/((l5+l6+l7)^2+e5+e6+e7)
    AVE_risk  := ((l5^2+l6^2+l7^2))/((l5^2+l6^2+l7^2)+e5+e6+e7) 
    rtrue     := (cor*cor)^(1/2)     # obtain absolute value; (abs() gives a constant)
    # Discriminant Validity Index _ lenient (dvi_l)
    dvi_l     := 1 - rtrue           # discriminant validity index: lenient
    # Discriminant Validity Index _ strict (dvi_s) for control 
    dvi_s_con := AVE_cont - rtrue^2  # discriminant validity index: strict for LV1
    # Discriminant Validity Index _ strict (dvi_s) for risk 
    dvi_s_ris := AVE_risk - rtrue^2  # discriminant validity index: strict for LV2
"
fit3 <- sem(KMCG_Model2.model, 
           sample.cov = KMCG_exp2.cov,
           sample.mean = means2,
           sample.nobs = sampsiz)  
summary(fit3, ci=TRUE)

##################################################################################
# Re-direct output from the console to path/file
##################################################################################

# save output in this path
sink(paste(getwd(),"/Kim&McGill(2011)_SSD_Model_1_2.txt",sep = ""))

# header
cat("Kim and McGill (2011, Study 2) - SSD - Model 1a")    # Model 1a
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 1a is unstandardized solution")
cat("\n")
summary(fit1a, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
cat("\n")
# header
cat("Kim and McGill (2011, Study 2) - SSD - Model 1b")    # Model 1b
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 1b is partially standardized solution -- see Appendix 2 - Table A2.2")
cat("\n")
summary(fit1b, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
# header
cat("Kim and McGill (2011, Study 2) - SSD - Model 1c")    # Model 1c
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 1c is standardized solution")
cat("\n")
summary(fit1c, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
# header
cat("Kim and McGill (2011, Study 2) - SSD - Model 2 -- See Appendic A2.2") 
cat("\n") # adds extra blank line 
cat("\n")
cat("Model 2 is SEM, partially standardized")
cat("\n")
summary(fit2, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
cat("\n")
# header
cat("Kim and McGill (2011, Study 2) - SSD - Reliability and Discriminant Validity")
cat("\n") # adds extra blank line 
cat("\n")
cat("\n")
summary(fit3, fit.measures=TRUE, ci=TRUE, rsquare=TRUE)
cat("\n")

##################################################################################
# Stop sinking; Re-direct output from path/file to the console again
##################################################################################

sink()

paste("The output has been saved at: ", getwd(),"/Kim&McGill(2011)_SSD_Model_1_2.txt",sep = "")
