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
