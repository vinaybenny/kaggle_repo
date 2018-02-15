# ================================================================================================ #
# Description: Perform extensive cleaning and application of business rules on training and test
#   datasets, and perform a few checks on distribution.
# 
# Author: V Benny
#
# ================================================================================================ #

library(VIM)
library(mice)
library(stringr)
library(vtreat)
library(FactoMineR)
library(reshape2)
library(unbalanced)
library(fastcluster)
library(parallel)

# Set up parallelisation  parameters
cl <- makeCluster(4) # Use judiciously


# Plot the list of empty columns
ggplot(missing_values  , aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()


# The big question is how to treat NULLs, and this can be extremely context sensitive for each variable.
# It would be a good idea to do a hierarchical variable clustering for missingness in data.
# For this, let's create a dataset that is just indicative of whether each cell is NULL or not.
dummy_train <- lapply(train, function(x) ifelse(is.na(x), 1, 0) ) %>% data.frame()
# missingness_cluster <- fastcluster::hclust(d = dist(dummy_train), method = "ward.D2")
# plot(missingness_cluster)




# Obtain all combination of missingness patterns and their percentages- too many possible combos in this case.
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

# We apply WOE to encode the categorical variables into numeric values
# I've found a bug in the WOE function for categorical levels with very small number of occurences, but 
# we'll go with it now until I fix it up.
# train[catcols] <- lapply(train[catcols], function(x) WOE(X = x, Y = train$is_female)) %>% as.data.frame()



# Use vtreat package to create a treatment plan for impact coding of dataset
treatencoder <- vtreat::mkCrossFrameCExperiment(dframe = train %>% select(-id), varlist = c(catcols, numcols, intcols)
                                              ,outcomename = targetcol, outcometarget = "1", minFraction = 0.1
                                              ,ncross=5
                                              # ,customCoders =list('c.woeC.center' = woeCoderC)
                                              ,codeRestriction = c('clean', 'isBAD', 'catB')
                                              ,parallelCluster = cl
                                              )
treatplan <- treatencoder$treatments
#print(treatplan$scoreFrame[,c('varName','sig')])

# Apply the treatments to train, valid and test
train <- cbind(id = train$id, vtreat::prepare(treatplan, train %>% select(-id)) )
valid <- cbind(id = valid$id, vtreat::prepare(treatplan, valid %>% select(-id)) )
test <- cbind(id = test$id, vtreat::prepare(treatplan, test %>% select(-id)) )




# Apply principal components analysis to extract features from the dataset
#pcadata <- PCA(train %>% select(-id, -is_female), scale.unit=TRUE, ncp=5, graph=T)
pca <- prcomp(train %>% select(-id, -is_female), retx=TRUE, center=TRUE, scale=TRUE)
expl.var <- pca$sdev^2/sum(pca$sdev^2)
plot(expl.var[1:300], xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


# Apply PCA to all datasets
train.pca <- cbind(id = train$id, predict(pca, newdata=train %>% select(-id, -is_female))[,1:400], 
               is_female = as.numeric(as.character(train$is_female)) ) %>% as.data.frame()
valid.pca <- cbind(id = valid$id, predict(pca, newdata=valid %>% select(-id, -is_female))[,1:400], 
               is_female = as.numeric(as.character(valid$is_female)) ) %>% as.data.frame()
test.pca <- cbind(id = test$id, predict(pca, newdata=test %>% select(-id))[,1:400] ) %>% as.data.frame()


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

stopCluster(cl)









