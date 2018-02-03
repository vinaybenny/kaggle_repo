# ================================================================================================ #
# Description: Extract the train and test datasets and apply basic transformation
# 
# Author: V Benny
#
# ================================================================================================ #

library(data.table)
library(xlsx)


############################### Data Extraction ################################################

# Read files into memory
train <- fread('../data/train.csv', stringsAsFactors = FALSE) %>% as.data.frame()
test <- fread('../data/test.csv', stringsAsFactors = FALSE)  %>% as.data.frame()
dict <- read.xlsx("../data/data_dictionary.xlsx", sheetName = "Codebook")

############################### Data Pre-cleaning ################################################

# Check missingness in training dataset
missing_values <- train %>% 
  summarize_all(funs(sum(is.na(.))/n())) %>% 
  gather(key="feature", value="missing_pct") %>% 
  arrange(missing_pct)

# Drop all columns which are completely empty
dropcols <- missing_values %>% 
  filter(missing_pct == 1.0) %>% 
  select(feature) %>% 
  as.vector()
train <- train %>% 
  select(-one_of(dropcols$feature))
test <- test %>% 
  select(-one_of(dropcols$feature))


# Classify column variables
idcol <- "trainid"
targetcol <- "is_female"
catcols <- names(train[, sapply(train, is.character)])
intcols <- names(train[, sapply(train, is.integer) & !( names(train) %in% c(idcol, catcols, targetcol))])
numcols <- names(train[, !names(train) %in% c(catcols, intcols, idcol, targetcol) ])

# Train-Validation split
validation_size <- 0.7
train_indices <- createDataPartition(train$is_female, times = 1, p = validation_size, list = TRUE)
valid <- train[-train_indices$Resample1,]
train <- train[train_indices$Resample1,]

# Convert target into a factor variable
train$is_female <- factor(train$is_female)
valid$is_female <- factor(valid$is_female)


############################### Data Exploration ################################################

# Plot histograms of all variables after filtering NA
# train %>% 
#   select_if(is.numeric) %>% 
#   select(-one_of(idcol)) %>% 
#   melt() %>% 
#   filter(!is.na(value)) %>%
#   ggplot(aes(x = value)) + facet_wrap(~variable,scales = "free") + geom_histogram()
# ggsave(file = "../output/plots/histograms_before_imputation.png", device = "png", width = 16, height = 8, units = "in")



