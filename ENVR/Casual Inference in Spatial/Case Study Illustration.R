# --- Need to install a few pakcages first
# Installing 'arepa' breaks because the package 'zipcode' is archived.  Need to fix.
# Install remotes if you don't already have it
# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Install the archived zipcode package from CRAN archive
# remotes::install_version("zipcode", version = "1.0")

# Install the arepa and DAPSm packages from github
# devtools::install_github("czigler/arepa")
# devtools::install_github("gpapadog/DAPSm")


# --- Need to set up all of the functions that come from https://github.com/gpapadog/DAPSm-Analysis
#     Need to have followed some of the instructions there about directories, etc.
config_path <- 'config.R' #/Users/corwinzigler/Dropbox/Teaching/Short Courses/Spatial Causal Inference/2026 ENVR Workshop/case studies/Power Plant SCR Controls/config.R'

# Loading libraries.
library(data.table)
library(corrplot)
library(MatchIt)
library(Matching)
library(fields)
library(ggplot2)
library(DAPSm)
library(stringr)
library(arepa)
library(optmatch)
library(gbm)

source(config_path)
# Setting the working directory
#setwd(wd)

# Sourcing the functions that I will be using.
source('CaliperEst_function.R')
source('Data_analysis_functions.R')
source('Data_analysis_models_functions.R')
source('dist.caliper_function.R')
source('expit.R')
source('GBMPropScores_function.R')
source('OptPSmatch_function.R')
source('PSmatchEst_function.R')
source('StandDiff_function.R')


#' --------------------------------------------------------------------------
#'     Traditional Propensity Score Analysis of the effects of SnCR Systems
#'     on ambient ozone pollution 
#' -------------------------------------------------------------------------- 
#' 
# Load the data used in the Papadogeorgou et al 2019 paper, this is an object called analysis_dat
load(paste0(data_dir, 'analysis_dat.Rdata'))

# --------- SETTING UP THE ANALYSIS --------- #
outcome_names <- c('mean4maxOzone', 'totNOxemissions')
out.col <- which(names(analysis_dat) == 'mean4maxOzone') # The outcome will be the average 4th highest max Ozone
trt.col <- which(names(analysis_dat) == 'SnCR') # The treatment is a binary indicator of whether a power plant
coord.cols <- which(names(analysis_dat) %in% c('Fac.Longitude', 'Fac.Latitude'))
drop_col <- which(names(analysis_dat) %in% setdiff(outcome_names, outcome_analysis))
conf <- setdiff(1:ncol(analysis_dat), c(out.col, trt.col, coord.cols, drop_col))

# Create a subset of the data corresponding only to the treatment, outcome, location cordinates, and confounders
subdta <- analysis_dat[, unique(c(trt.col, out.col, coord.cols, conf)), with = FALSE]

trt.col <- 1
out.col <- 2
coord.cols <- c(4, 3)

# ----  Some Basic Descriptives
# The data consist of 473 power plants
nrow(analysis_dat) 
head(analysis_dat)

# 18 confounders *measured* on each power plant
names(analysis_dat)[conf]

#152 of the 473 power plants have a SnCR system installed (i.e., are "treated")
with(subdta, table(SnCR))

# ---- A completely unadjusted analysis that does no confounding adjustment
# A naive comparison of mean ozone suggests that ozone is higher among power plants with SnCR installed
unadjusted = with(subdta, t.test(mean4maxOzone~SnCR))
unadjusted

#store the results for later
unadjusted.result = c(-1*unadjusted$conf.int[2], unadjusted$estimate[2] - unadjusted$estimate[1], -1*unadjusted$conf.int[1])

#' -------------------------------------------------------
#'  STEP 2: Calculate Distance Metrics
#' -------------------------------------------------------

# ---- A traditional propensity score analysis
# First, estimate the propensity score with a simple logistic regression
# this model includes all 18 of the confounders
naive_ps <- glm(as.formula(paste('SnCR ~ . - Fac.Latitude - Fac.Longitude -',
                                 outcome_analysis)), data = subdta, family = 'binomial')
# Estimated propensity score is just the fitted value from the above model
subdta[, prop.scores := fitted(naive_ps)]
ignore.cols <- which(names(subdta) == 'prop.scores')
ignore.cols.coords <- c(ignore.cols, coord.cols)
cols.balance <- setdiff(1:dim(subdta)[2],
                        c(trt.col, out.col, ignore.cols.coords,
                          which(names(subdta) == 'prop.scores')))

#' -------------------------------------------------------
#'  STEP 3: Restructure the Data
#' -------------------------------------------------------

# Now, use the estimated propensity scores to do 1:1 propensity score matching
# This is the "naive" analysis described in Papadogeorgou et al (2019)
naive.match <- NaiveModel(subdta, trt.col = trt.col, out.col = out.col,
                          caliper = caliper, coord.cols = coord.cols,
                          cols.balance = cols.balance,
                          matching_algorithm = matching_algorithm,
                          remove_unmatchables = remove_unmatchables)

# 137 treated power plants had a good enough match to be retained for the analysis
# These are the ones retained for the analysis
naive.match$num_match

#' -------------------------------------------------------
#'  STEP 4: Diagnose Balance
#' -------------------------------------------------------

# Compare the covariate distribution across the SnCR plants and the matched controls
# Visually
naive.match$balance_plots
# Mean differences (SMD)
naive.match$balance

# Check whether any covariates have standardized mean difference > 0.15
# Before matching 10 out of 18 covariates are "imbalanced"
sum(abs(naive.match$balance[1,]) > cutoff)
# After matching 3 out of 18 covariates is "imbalanced"
sum(abs(naive.match$balance[2,]) > cutoff)
# The covariates that are not balanced
which(abs(naive.match$balance[2,])>cutoff)

## Create a simple balance plot comparing balance before/after matching
bal_t <- t(naive.match$balance)
bal_t <- bal_t[order(bal_t[,1]),]

# Extra left margin for long covariate names
par(mar = c(5, 8, 4, 2)) 

matplot(
  bal_t, 
  1:nrow(bal_t), 
  type = "b", 
  pch = c(16, 17), 
  col = c("#D95F02", "#7570B3"), 
  lty = 1, 
  yaxt = "n", 
  xlab = "Standardized Mean Difference", 
  ylab = "", 
  main = "Standardized Mean Covariate Differences"
)

axis(2, at = 1:nrow(bal_t), labels = rownames(bal_t), las = 2)

abline(v = 0, col = "gray30", lty = 1)
abline(v = c(-cutoff, cutoff), col = "gray60", lty = 2)

legend(
  "topright", 
  legend = c("Before Matching", "After Matching"), 
  col = c("#D95F02", "#7570B3"), 
  pch = c(16, 17), 
  lty = 1
)


# Alternative (more flexible) propensity score model specification
# Replace simple logistic regression with gradient boosting
GBM.match <- GBMmodel(subdta, trt.col, out.col, gbm.caliper, coord.cols,
                      cols.balance = cols.balance, seed = 1234,
                      matching_algorithm = matching_algorithm,
                      remove_unmatchables = remove_unmatchables)

# Only 54(!) treated power plants had a good enough match to be retained for the analysis
# These are the ones retained for the analysis
GBM.match$num_match

# Compare the covariate distribution across the SnCR plants and the matched controls
# Mean differences
GBM.match$balance
# After matching wiith the GBM estimated propensity score, 3 out of 18 covariates is "imbalanced"
sum(abs(GBM.match$balance[2,]) > cutoff)
# So in this instance, the "fancier" propensity score model was counterproductive - many fewer
# matches and middling covariate balance. 


#' -------------------------------------------------------
#'  STEP 5: Estimate Causal Effects
#' -------------------------------------------------------

# Plotting the results of the unadjusted analysis vs the (naive) traditional propensity score analysis
methods = c("unadjusted", "(naive) PS")
result <- array(NA, dim = c(length(methods), 3))
dimnames(result) <- list(method = methods,
                         statistic = c('LB', 'Estimate', 'UB'))
result[1,] = unadjusted.result
result[2,] = naive.match$result

PlotResults(result, color = c(rep('grey65', 2)),
            title = paste(outcome_analysis, paste(month, collapse = ','), '/', year))


# Plot the locations of the matched pairs
MatchedDataMap(naive.match$pairs, trt_coords = c(3, 4), con_coords = c(7, 8),
               plot.title = 'Naive pairs', point_data = FALSE)

# PS matching generates matches that are ~1000mi apart on average!
naive.match$distance


#' --------------------------------------------------------
#' Now do the analysis using the DAPSm method, as done in the Papadogeorgou et al (2019) paper
#' --------------------------------------------------------

# First, let's arbitrarily pick a value of w=0.2 to do an analysis that puts most of the
# weight on geographic distance, but some weight on propensity score distance
dapsm_eq <- CalcDAPSWeightBalance(subdta, weights=0.2, cols.balance, trt.col, out.col,
                               coords.columns = coord.cols, caliper,
                               coord_dist = TRUE,
                               matching_algorithm = matching_algorithm,
                               remove.unmatchables = remove_unmatchables)

# DAPSm with this value of w found 124 matches
dapsm_eq$num_match_DAPS

# Check covariate balance - add a line to our previous balance plot
dim(dapsm_eq$balance)
bal_t = cbind(t(naive.match$balance), dapsm_eq$balance[1,2,])
bal_t = bal_t[order(bal_t[,1]), ]
par(mar = c(5, 8, 4, 2)) 

matplot(
  bal_t, 
  1:nrow(bal_t), 
  type = "b", 
  pch = c(16, 17, 18), 
  col = c("#D95F02", "#7570B3", "chartreuse"), 
  lty = 1, 
  yaxt = "n", 
  xlab = "Standardized Mean Difference", 
  ylab = "", 
  main = "Standardized Mean Covariate Differences"
)

axis(2, at = 1:nrow(bal_t), labels = rownames(bal_t), las = 2)

abline(v = 0, col = "gray30", lty = 1)
abline(v = c(-cutoff, cutoff), col = "gray60", lty = 2)

legend(
  "topleft", 
  legend = c("Before Matching", "Naive Matching", "DAPS Matching"), 
  col = c("#D95F02", "#7570B3" ,"chartreuse"), 
  pch = c(16, 17, 18), 
  lty = 1
)

# Balance doesn't look that different between naive PS and DAPSm (for w=0.2)
# but let's check the average geographic distance between matches
# --- Naive PS matching matches locations that are ~1000mi away from each other on average
naive.match$distance
# --- DAPSm reduces this distance to ~87mi, should better adjust for U
dapsm_eq$distance

# Let's look at the distances on a map
MatchedDataMap(naive.match$pairs, trt_coords = c(3, 4), con_coords = c(7, 8),
               plot.title = 'Naive pairs', point_data = FALSE)
MatchedDataMap(dapsm_eq$full_pairs, trt_coords = c(3, 4), con_coords = c(7, 8),
               plot.title = 'DAPSm pairs', point_data = FALSE)





#' -- Instead of picking w=0.2 arbitrarily, let's do as in Papasogeorgou et al (2019)
#'    and search across a grid of w and pick the min value we can get before getting matches
#'    where observed covariate balance is bad

# This function does the same as above, but repeated across a vector of values for 'weights'
w_bal <- CalcDAPSWeightBalance(subdta, weights, cols.balance, trt.col, out.col,
                               coords.columns = coord.cols, caliper,
                               coord_dist = TRUE,
                               matching_algorithm = matching_algorithm,
                               remove.unmatchables = remove_unmatchables)

# Let's check covariate balance across all the different values of w to see how putting more
# weight on proximity impacts balance of observed covariates
# Power plant and area-level characteristics separately.

cov_names <- c('% Operating Capacity', 'ARP Phase 2', '4th Max Temp', '% Urban',
               '% White', '% Black', '% Hispanic', '% High School',
               'Household Income', '% Poor', '% Occupied', '% 5-year residents',
               'House Value', 'Heat Input', 'Population / square mile',
               'Gas facility', 'Small sized facility', 'Medium sized facility')
dimnames(w_bal$balance)[[3]] <- cov_names

cols <- paste0('gray', c(80, 60, 30, 5))
cols <- c('orange1', 'tomato', 'mediumvioletred', 'lightslateblue',
          'skyblue2', 'midnightblue')
PlotWeightBalance(abs(w_bal$balance[, , c(1, 2, 14, 16, 17, 18)]),
                  full_data = -5, weights, cutoff, axis_cex = 0.8,
                  mar = c(4, 4, 2, 3), leg_cex = 0.7, inset = - 0,
                  cols = cols)
title(main = 'Power plant characteristics')
PlotWeightBalance(abs(w_bal$balance[, , - c(1, 2, 14, 16, 17, 18)]),
                  full_data = -5, weights, cutoff, axis_cex = 0.8,
                  mar = c(4, 4, 2, 3), inset = - 0, leg_cex = 0.7,
                  cols = cols)
title(main = 'Area level characteristics')



# This function takes all of those DAPSm matched data sets, picks the smallest w that still achieves balance on observed covariates
# and calculates effect estimates, etc.
dapsm <- DAPSchoiceModel(balance = w_bal$balance, cutoff = cutoff,
                         dataset = subdta, pairs = w_bal$pairs,
                         full_pairs = w_bal$full_pairs,
                         distance_DAPS = w_bal$distance_DAPS,
                         out.col = out.col, weights = weights,
                         trt.col = trt.col)

# The chosen value of w is 0.513
dapsm$weight

# Compare covariate balance the same as before, but just for this value of w
#  - add a line to our previous balance plot
bal_t = cbind(t(naive.match$balance), dapsm$balance[2,])
bal_t = bal_t[order(bal_t[,1]), ]
par(mar = c(5, 8, 4, 2)) 

matplot(
  bal_t, 
  1:nrow(bal_t), 
  type = "b", 
  pch = c(16, 17, 18), 
  col = c("#D95F02", "#7570B3", "chartreuse"), 
  lty = 1, 
  yaxt = "n", 
  xlab = "Standardized Mean Difference", 
  ylab = "", 
  main = "Standardized Mean Covariate Differences"
)

axis(2, at = 1:nrow(bal_t), labels = rownames(bal_t), las = 2)

abline(v = 0, col = "gray30", lty = 1)
abline(v = c(-cutoff, cutoff), col = "gray60", lty = 2)

legend(
  "topleft", 
  legend = c("Before Matching", "Naive Matching", "DAPS Matching"), 
  col = c("#D95F02", "#7570B3" ,"chartreuse"), 
  pch = c(16, 17, 18), 
  lty = 1
)

# Check the distance between matches
# DAPSm selects matches that are ~140mi apart on average
dapsm$distance

# Let's look at the distances on a map
MatchedDataMap(naive.match$pairs, trt_coords = c(3, 4), con_coords = c(7, 8),
               plot.title = 'Naive pairs', point_data = FALSE)
MatchedDataMap(dapsm$pairs, trt_coords = c(3, 4), con_coords = c(7, 8),
               plot.title = 'DAPSm pairs', point_data = FALSE)

# Plotting the results.
result = rbind(result, dapsm$est + c(- 1, 0, 1) * 1.96 * dapsm$se)
rownames(result)[3] = "DAPSm"

PlotResults(result, color = c(rep('grey65', 2), 'grey35'),
            title = paste(outcome_analysis, paste(month, collapse = ','), '/', year))

# Just to investigate the impact of different DAPSm weights, we could also look  at the DAPSm results as a function of w.
# but note that this is only for illustrating and building intuition about what increasing emphasis on spatial proximity 
# gets us in this analysis - you should definitely NOT use effect estimates across w to pick w.
DAPSWeightCE(dataset = subdta, out.col = out.col, trt.col = trt.col,
             weights = weights, pairs = w_bal$pairs, chosen_w = dapsm$weight)$plot





