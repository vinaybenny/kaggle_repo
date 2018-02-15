# ================================================================================================ #
# Description: Do a lasso model on the data.
# 
# Author: V Benny
#
# ================================================================================================ #

library(glmnet)
library(Metrics)
library(boot)

lasso_model <- cv.glmnet(as.matrix(train %>% select(-id, -is_female)), train$is_female, family="binomial")
# lasso_preds <- inv.logit(predict.cv.glmnet(lasso_model, newx = as.matrix(train %>% select(-id, -is_female)), s = "lambda.min"))
# print(auc(as.numeric(as.character(train$is_female)), lasso_preds))

## Predictions
lasso_preds <- inv.logit(predict.cv.glmnet(lasso_model, newx = as.matrix(valid %>% select(-id, -is_female)), s = "lambda.min"))
print(auc(as.numeric(as.character(valid$is_female)), lasso_preds))



lasso_predictions<- data.frame(test_id = test$id, 
                               predictions = inv.logit(predict.cv.glmnet(lasso_model, newx = as.matrix(test %>% select(-id)), s = "lambda.min")))