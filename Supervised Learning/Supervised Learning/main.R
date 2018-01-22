#################################
# Main Program
#################################

library(imputeTS);
library(dplyr);

source("automation.R");

train_file_action = "Training Data 2201 - Drop Column - 3 Class.csv";
test_file_action = "Test Data 2201 - Drop Column - 3 Class.csv";
train_file_word = "Training Data 1801 - Aggregated.csv";
test_file_word = "Test Data 1801 - Aggregated.csv";
target_column = "manual_assessment_avg";

train_data = baca(train_file_action);
colnames(train_data)[colnames(train_data) == target_column] = "respon";
train_data$source.word = NULL;
train_data$target.word = NULL;
train_data$X = NULL;
train_data$manual_assessment = NULL;
train_data$manual_assessment_2 = NULL;
train_data$username = NULL;

test_data = baca(test_file_action);
colnames(test_data)[colnames(test_data) == target_column] = "respon";
test_data$source.word = NULL;
test_data$target.word = NULL;
test_data$X = NULL;
test_data$manual_assessment = NULL;
test_data$manual_assessment_2 = NULL;
test_data$username = NULL;

nrow(train_data);
nrow(test_data);

train_data %>%
filter(is.na(respon) | is.na(origin_word_entropy)) %>%
group_by(origin_word_entropy, respon) %>%
summarise(jumlah_data = n());

#| is.na(manual_origin_source)

#Dataset1 = manual_assessment as target
train_dataset1 = train_data;
test_dataset1 = test_data;

#Data Preprocessing
train_dataset1$username = NULL;
train_dataset1$source.word = NULL;
train_dataset1$target.word = NULL;
train_dataset1_assessed = train_dataset1 %>%
filter(!is.na(respon) & !is.na(origin_word_entropy)) %>%
as.data.frame();
#check na value
colnames(train_dataset1_assessed)[colSums(is.na(train_dataset1_assessed)) > 0]
#normalization by replace NA with 0
train_dataset1_norm = na.replace(train_dataset1_assessed, 0);
colnames(train_dataset1_norm)[colSums(is.na(train_dataset1_norm)) > 0]

test_dataset1$username = NULL;
test_dataset1$source.word = NULL;
test_dataset1$target.word = NULL;
test_dataset1_assessed = test_dataset1 %>%
filter(!is.na(respon) & !is.na(origin_word_entropy)) %>%
as.data.frame();
#& !is.na(manual_origin_source)

#check na value
colnames(test_dataset1_assessed)[colSums(is.na(test_dataset1_assessed)) > 0]
#normalization by replace NA with 0
test_dataset1_norm = na.replace(test_dataset1_assessed, 0);
colnames(test_dataset1_norm)[colSums(is.na(test_dataset1_norm)) > 0];

nrow(train_dataset1_norm);
nrow(test_dataset1_norm);

hasil = fsRandomForest(modelRandomForest, train_dataset1_norm, test_dataset1_norm);

######### KALKULASI

set.seed(12345);

modelRandomForest = hitungRandomForest(train_dataset1_norm, test_dataset1_norm);
hitungNaiveBayes(train_dataset1_norm, test_dataset1_norm);
hitungDecisionTree(train_dataset1_norm, test_dataset1_norm);

#############

set.seed(12345);

modelRandomForest = hitungRandomForest(train_dataset1_normMDA, test_dataset1_normMDA);
hitungNaiveBayes(train_dataset1_normMDA, test_dataset1_normMDA);
hitungDecisionTree(train_dataset1_normMDA, test_dataset1_normMDA);

modelRandomForest = hitungRandomForest(train_dataset1_normMDG, test_dataset1_normMDG);
hitungNaiveBayes(train_dataset1_normMDG, test_dataset1_normMDG);
hitungDecisionTree(train_dataset1_normMDG, test_dataset1_normMDG);

hitungSoftmax(train_dataset1_norm, test_dataset1_norm);

modelRandomForest = hitungRandomForest(train_dataset1_norm, test_dataset1_norm);

modelNaiveBayes = trainNaiveBayes(train_dataset1_norm);
aucNB = aucNaiveBayes(modelNaiveBayes, test_dataset1_norm);

######### Backup Training Testing Data

train_data_back = train_data;
test_data_back = test_data;

train_data = train_dataset1_norm;
test_data = test_dataset1_norm;

modelRandomForest = trainRandomForest(train_data);
