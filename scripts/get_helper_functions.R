# ================================================================================================
# Description: place to store any functions needed for the analysis
#
# Input: 
# TBA
#
# Output: 
# TBA
#
# Author: V Benny
#
# Dependencies: 
# TBA
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 01 Feb 2018 VB v1
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
  
  
  
  
  
  
  
  