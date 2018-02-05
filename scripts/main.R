# ================================================================================================
# Description: main script to run the entire analysis
#
# Input: 
# TBA
#
# Output: 
# TBA
#
# Author: E Walsh
#
# Dependencies: 
# config.R = file that specifies the absolute path used to setwd
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 01 Feb 2018 EW v1
# ================================================================================================

# ========================================= setup ================================================

# maybe required for reproducibility
set.seed(12345);

# versions given to ensure contributors do not run into compatability issues
library(dplyr)    # 0.7.4    data manipulation  
library(tidyr)    # 0.8.0    used for data tidying in conjunction with dplyr
library(reshape2) # 1.4.3    used for the melt function
library(mice)     # 2.46.0   multivariate imputation
library(caret)    # 6.0-78   classification and regression training
library(ggplot2)  # 2.2.1    nice plots
library(xgboost)  # 0.6.4.1  gradient boosting
library(h2o)      # 3.16.0.2 deep learning    
# library(e1071)  # 1.6-8    support vector machines
library(Hmisc)    # 4.1-1    labelling columns of dataset
library(data.table) # 1.10.4-3 faster data extraction functions
library(xlsx)     # 0.5.7   reading excel data
library(mlr)      # 2.11

# this config must define folder_path the absolute path of the repo
source("scripts/config.R")

setwd(folder_path)

source("scripts/get_helper_functions.R")

# ====================================== analysis ============================================

source("scripts/get_data.R")

source("scripts/get_features.R")

source("scripts/get_xgboost_solution.R")

source("scripts/get_h2o_solution.R")



