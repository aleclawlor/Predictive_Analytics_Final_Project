library(readxl)

# load the dataset 
heart_dataset <- read_excel("C:/Users/Alexander/Spring_22/predictive_analytics/final_project/heart_failure_clinical_records_dataset.xlsx")


# randomly sample indices
train_percentage = .8

samplesize = train_percentage * nrow(heart_dataset)
set.seed(80)
index = sample(seq_len(nrow(heart_dataset)), size=samplesize)

# create training and test split
train = heart_dataset[index,]
test = heart_dataset[-index,]

# create a normalized dataset for neural networks
heart_dataset_normalized = as.data.frame(lapply(heart_dataset, function(x)(x-min(x)) / (max(x) - min(x))))

# now create normalized test and train
train_normalized = heart_dataset_normalized[index,]
test_normalized = heart_dataset_normalized[-index,]

# TODO: in the future we could make the train and test sets have equal numbers of each class