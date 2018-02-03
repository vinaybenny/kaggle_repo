# ================================================================================================ #
# Description: Perform extensive cleaning and application of business rules on training and test
#   datasets, and perform a few checks on distribution.
# 
# Author: V Benny
#
# ================================================================================================ #

library(VIM)
library(mice)
library(xlsx)
library(stringr)
library(vtreat)
library(FactoMineR)
library(reshape2)
library(unbalanced)


# Plot the list of empty columns
ggplot(missing_values  , aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()

# Obtain all combination of missingness patterns and their percentages
# missing_pattern <- aggr(train[, !names(train) %in% missing_values[which(missing_values$missing_pct == 0), 1]]
#                         ,col = mdc(1:2)
#                         ,numbers = TRUE
#                         ,labels = names(train[, !names(train) %in% missing_values[which(missing_values$missing_pct == 0), 1]])
#                         ,cex.axis=.7
#                         ,gap=3
#                         ,ylab=c("Proportion of missingness","Missingness Pattern"))
# missing_comb <- data.frame(missing_pattern$tabcomb)
# names(missing_comb)  <- names(missing_pattern$x)
# missing_comb$percent <- missing_pattern$percent
# write.xlsx2(missing_comb, file = "../output/missing_values_combinations.xlsx", row.names = FALSE)


############################### Data Cleaning & Imputation ################################################

# Use vtreat package to perform data cleaning/imputation
cfeencoder <- vtreat::mkCrossFrameCExperiment(dframe = train, varlist = c(catcols, numcols, intcols)
                                              ,outcomename = targetcol, outcometarget = "1")
treatplan <- cfeencoder$treatments
train <- cfeencoder$crossFrame

####################################### Apply variable transformations #################################################

# Apply on all datasets
# train_y <- train[, names(train) %in% targetcol]
# valid_y <- valid[, names(valid) %in% targetcol]
# test_ids <- test[, names(test) %in% idcol]
# weightvector <- ifelse(train_y == "0", length(train_y)/length(train_y[train_y == "0"]), length(train_y)/length(train_y[train_y == "1"]) )
# 
# train_x <- train %>% select(-one_of(idcol, targetcol))
# valid_x <- valid %>% select(-one_of(idcol, targetcol))
# test <- valid %>% select(-one_of(idcol))
# valid_x <- vtreat::prepare(treatplan, valid_x, pruneSig = NULL, varRestriction = treatplan$scoreFrame$varName)
# test <- vtreat::prepare(treatplan, test, pruneSig = NULL, varRestriction = treatplan$scoreFrame$varName)
# 
# # Apply PCA for dimension reduction
# pcaencoder <- PCA(X = train_x, scale.unit = TRUE, ncp = 70, graph = FALSE)
# 
# # Apply on all datasets
# train_x <- data.frame((predict.PCA(pcaencoder, train_x))$coord)
# valid_x <- data.frame((predict.PCA(pcaencoder, valid_x))$coord)
# test <- data.frame((predict.PCA(pcaencoder, test))$coord)
# 
# # Apply a bit of balancing to the target classes using minority oversampling technique SMOTE
# data <- ubBalance(X = train_x, Y = train_y, type="ubSMOTE", percOver=300, percUnder=100, verbose=TRUE)
# train_x <- data$X
# train_y <- data$Y
# 
# ############################### Post-transformation Data Exploration ################################################
# 
# corrmat <- cor(train %>% select(-one_of(targetcol)))
# corrmat <- reorder_cormat(corrmat)
# upper_tri <- get_upper_tri(corrmat)
# # Melt the correlation matrix
# melted_cormat <- melt(upper_tri, na.rm = TRUE)
# 
# # Create list of columns that need to be excluded from any modelling, based on correlation being above a threshold value
# exclude_columns <- (melted_cormat %>% filter(Var1 != Var2, value > 0.9))[,1] %>% as.character()













