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

# Check missingness in training dataset
missing_values <- train %>% 
  summarize_all(funs(sum(is.na(.))/n())) %>% 
  gather(key="feature", value="missing_pct") %>%
  left_join(dict, by = c("feature" = "Column.Name")) %>%
  select(-Values) %>%
  arrange(missing_pct)

# Drop all columns which are completely empty or have no variance at all.
train <- removeConstantFeatures(train)

# Assign labels to dataset wherever available for ease of interpreting columns. Use label(train) to get descriptions
train <- assignLabels(train, dict)


# Classify column variables based on types
# Assuming all columns in the codebook are categorical, since these have levels defined. Of course, we lose 
# ordinality when we do this. these can be revisited later on a case-by-case basis. 
# We add all columns of char type to this, and some other columns that look like those are factors.
idcol <- "train_id"
targetcol <- "is_female"
catcols <- union(union(names(train[, !grepl("Unknown", label(train), fixed = TRUE) & !names(train) %in% c(targetcol)]), 
                 names(train[, sapply(train, is.character)])),
                 c("AA4", "AA7", "AA14", "AA15", "DG8a", "DG8b", "DG8c", "DL4_96", "DL4_99", "DL11", "MT1", "IFI18", "FB13",
                   "DG9a", "DG9b", "DG9c", "G2P2_96", "G2P3_6", "G2P3_8", "G2P3_9","G2P3_11", "G2P3_13", "G2P3_96", "MT6C",
                   "MM23", "FB14", "FB15"))
intcols <- names(train[, sapply(train, is.integer) & !( names(train) %in% c(idcol, catcols, targetcol))])
numcols <- names(train[, !names(train) %in% c(catcols, intcols, idcol, targetcol) ])

# A crude treatment of all NAs in the dataset as a special category. This has repercussions on numeric columns
train[is.na(train)] <- -99

# Cast all categorical columns as factors
train[catcols] <- lapply(train[catcols], as.factor)

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



