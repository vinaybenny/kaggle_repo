# ================================================================================================
# Description: place to store any functions needed for the analysis
#
# Input: 
# TBA
#
# Output: 
# TBA
#
# Author: E Walsh, V Benny
#
# Dependencies: 
# TBA
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 01 Feb 2018 EW v1
# ================================================================================================

print("Creating helper functions...")

# ===================================== functions ================================================

#' Function to assign labels to each column in the dataframe
#' 
#' @dataset  A dataframe for which labels need to be added to columns.
#' @var_label_mapping A dataframe which holds a mapping between column names, and labels to be assigned to those column names.
#' @return A dataframe for which the lables are added as an attribute to the columns.
#' @examples
#' assignLabels(data.frame(age=c(2,5,7,9),sex=c("F","M","M","F")), data.frame(col_name=c("age","sex"),label=c("Age of person","Sex of person")))
#' 
assignLabels <- function(dataset, var_label_mapping){
  require(Hmisc)
  label(dataset) = lapply(names(dataset), 
                          function(x) {
                            labelval <- as.character(var_label_mapping[which(var_label_mapping[,1] == x), 2])
                            # print(x)
                            # print(labelval)
                            label(dataset[,x]) = ifelse(length(labelval) == 0, "Unknown", labelval  )
                            }
                          )
  return(dataset)
}

#' Function to get the predictions in the correct formation for submission
#' 
#' @predictions A dataframe containing only the id and the prediction
#' @filename Name of the file defaults to submission_<date_stamp>.csv
#' @return A csv with the name as specified in filename
#' @examples
#' submit_predictions(predictions.df)
#' 
submit_predictions <- function(predictions, 
                               filename= paste("../data/submission",
                                               format(Sys.time(), "%Y%m%d"),
                                               ".csv", 
                                               sep = "")){
  
  write.table(predictions
              ,file = filename 
              ,row.names = FALSE
              ,col.names = c("test_id","is_female")
              ,sep = ",")
}


#' Function to create the WOE coding for datasets
#' @param v character scalar: variable name
#' @param vcol chracter, independent or input variable values
#' @param y logical, dependent or outcome variable to predict
#' @param weights row/example weights
#' @return scored training data column
woeCoderC <- function(v, vcol, 
                     y, 
                     weights) {
  # classification case y ~ vcol
  d <- data.frame(x = vcol,
                  y = y,
                  stringsAsFactors = FALSE)
  return(WOE(X = temp$gender, Y= temp$outcome))
}

print("Finished loading: assignLabels, submit_predictions, woeCoderC")