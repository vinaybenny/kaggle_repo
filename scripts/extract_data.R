# ================================================================================================ #
# Description: Extract the train and test datasets and apply basic transformations
# 
# Author: V Benny
#
# ================================================================================================ #


############################### Data Extraction ################################################

# Read files into memory
train <- fread('data/train.csv', stringsAsFactors = FALSE) %>% as.data.frame()
test <- fread('data/test.csv', stringsAsFactors = FALSE)  %>% as.data.frame()
dict <- read.xlsx("data/data_dictionary.xlsx", sheetName = "Codebook")

############################### Data Pre-cleaning ################################################

# Rename id columns in train and test
train <- rename(train, id = train_id)
test <- rename(test, id = test_id)

# Check missingness in training dataset
missing_values <- train %>% 
  summarize_all(funs(sum(is.na(.))/n())) %>% 
  gather(key="feature", value="missing_pct") %>%
  left_join(dict, by = c("feature" = "Column.Name")) %>%
  select(-Values) %>%
  arrange(missing_pct)

# Drop all columns which are completely empty or have no variance at all in train, and retain only these in test.
train <- removeConstantFeatures(train)
test <- test %>% select(one_of(names(train)))

# Assign labels to dataset wherever available for ease of interpreting columns. Use label(train) to get descriptions
train <- assignLabels(train, dict)


# Classify column variables based on types
# Assuming all columns in the codebook are categorical, since these have levels defined. Of course, we lose 
# ordinality when we do this. these can be revisited later on a case-by-case basis. 
# We add all columns of char type to this, and some other columns that look like those are factors.
idcol <- "id"
targetcol <- "is_female"
catcols <- union(union(names(train[, !grepl("Unknown", label(train), fixed = TRUE) & !names(train) %in% c(targetcol)]), 
                 names(train[, sapply(train, is.character)])),
                 c("AA4", "AA7", "AA14", "AA15", "DG8a", "DG8b", "DG8c", "DL4_96", "DL4_99", "DL11", "MT1", "IFI18", "FB13",
                   "DG9a", "DG9b", "DG9c", "G2P2_96", "G2P3_6", "G2P3_8", "G2P3_9","G2P3_11", "G2P3_13", "G2P3_96", "MT6C",
                   "MM23", "FB14", "FB15"
                   ,"DG10b","DG10c","DG11b","DG11c","G2P3_1","G2P3_2", "G2P3_3","G2P3_4","G2P3_5","G2P3_7","G2P3_10","G2P3_12"
                   ,"G2P3_14","G2P3_15","G2P3_16","MT3_1","MT3_2","MT3_3","MT12_1","MT12_2","MT12_3","MT12_4","MT12_5","MT12_6"
                   ,"MT12_7","MT12_8","MT12_9","MT12_10","MT12_11","MT12_12", "MT12_13", "MT12_14", "MT12_96","FF7_1","FF7_2"
                   ,"FF7_3","FF7_4","FF7_5","FF7_6","FF7_7","FF7_96","FF8_1","FF8_2","FF8_3","FF8_4","FF8_5","FF8_6","FF8_7","FF8_96"
                   ))
intcols <- names(train[, sapply(train, is.integer) & !( names(train) %in% c(idcol, catcols, targetcol))])
numcols <- c("DL8")

# A crude treatment of all NAs in the dataset as a special category. This has repercussions on numeric columns
#train[is.na(train)] <- -99

# Cast all categorical columns as factors in train & test
train[catcols] <- lapply(train[catcols], as.factor)
test[catcols] <- lapply(test[catcols], as.factor)

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



