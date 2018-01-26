#################################
# Main Program
#################################

library(imputeTS);
library(dplyr);

source("automation.R");

train_file_action = "C:/Datasets/Training Data 2301 - Drop Column.csv";
test_file_action = "C:/Datasets/Test Data 2301 - Drop Column.csv";
train_file_word = "Training Data 1801 - Aggregated.csv";
test_file_word = "Test Data 1801 - Aggregated.csv";
target_column = "manual_assessment_avg";

train_data = baca2(train_file_action);
colnames(train_data)[colnames(train_data) == target_column] = "respon";
train_data$source.word = NULL;
train_data$target.word = NULL;
train_data$X = NULL;
train_data$manual_assessment = NULL;
train_data$manual_assessment_2 = NULL;
train_data$username = NULL;

test_data = baca2(test_file_action);
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

modelRandomForest = trainRandomForest(train_dataset1_norm);

hasil = fsRandomForest(modelRandomForest, train_dataset1_norm, test_dataset1_norm);

] #######################

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
modelNaiveBayes = trainNaiveBayes(train_data);
modelCHAID = trainCHAID(train_data);
modelCART = trainCART(train_data);
modelC50 = trainC50(train_data);
modelSVM = trainCumSVM(train_data);

respon = test_dataset1_norm$respon;

source("automation.R");

predRF = predRandomForest(modelRandomForest, test_data);
predNB = predNaiveBayes(modelNaiveBayes, test_data);
predCHAID = predCHAID(modelCHAID, test_data);
predCART = predCART(modelCART, test_data);
predC50 = predC50(modelC50, test_data);
predSVM = predCumSVM(modelSVM, test_data);

print(confusionMatrix(predRF, respon));
print(confusionMatrix(predNB, respon));
print(confusionMatrix(predCHAID, respon));
print(confusionMatrix(predCART, respon));
print(confusionMatrix(predC50, respon));
print(confusionMatrix(predSVM, respon));

print(auc(respon, predRF));
print(auc(respon, predNB));
print(auc(respon, predCHAID));
print(auc(respon, predCART));
print(auc(respon, predC50));
print(auc(respon, predSVM));

####### Split by Percentile 

train_data_back = train_data;
test_data_back = test_data;

train_data = train_dataset1_norm;
test_data = test_dataset1_norm;

i = 0.5;

importances = data.frame(importance(modelRandomForest));
kolom_train_data = colnames(train_data)[1:ncol(train_data) - 1];
importances$kolom = kolom_train_data;
importances = rbind(importances, importances[1,]);

threshMDA = quantile(importances$MeanDecreaseAccuracy, i);
threshMDG = quantile(importances$MeanDecreaseGini, i);

importances$threshMDA = 0;
importances$threshMDG = 0;

importances$threshMDA[importances$MeanDecreaseAccuracy >= threshMDA] = 1;
importances$threshMDG[importances$MeanDecreaseGini >= threshMDG] = 1;

featuresMDA = subset(importances, threshMDA == 1);
featuresMDG = subset(importances, threshMDG == 1);

namaMDA = featuresMDA$kolom;
namaMDG = featuresMDG$kolom;

train_dataset1_normMDA = train_data %>%
        select(namaMDA) %>%
        as.data.frame();

train_dataset1_normMDA$respon = train_data$respon;

train_dataset1_normMDG = train_data %>%
        select(namaMDG) %>%
        as.data.frame();

train_dataset1_normMDG$respon = train_data$respon;

test_dataset1_normMDA = test_data %>%
        select(namaMDA) %>%
        as.data.frame();

test_dataset1_normMDA$respon = test_data$respon;

test_dataset1_normMDG = test_data %>%
        select(namaMDG) %>%
        as.data.frame();

set.seed(12345);

modelRandomForest_MDA = trainRandomForest(train_dataset1_normMDA);
modelNaiveBayes_MDA = trainNaiveBayes(train_dataset1_normMDA);
modelCHAID_MDA = trainCHAID(train_dataset1_normMDA);
modelCART_MDA = trainCART(train_dataset1_normMDA);
modelC50_MDA = trainC50(train_dataset1_normMDA);

modelSVM_MDA = trainCumSVM(train_dataset1_normMDA);
modelSVM_MDA = trainCumSVM_3class(train_dataset1_normMDA);

modelRandomForest_MDG = trainRandomForest(train_dataset1_normMDG);
modelNaiveBayes_MDG = trainNaiveBayes(train_dataset1_normMDG);
modelCHAID_MDG = trainCHAID(train_dataset1_normMDG);
modelCART_MDG = trainCART(train_dataset1_normMDG);
modelC50_MDG = trainC50(train_dataset1_normMDG);
modelSVM_MDG = trainCumSVM(train_dataset1_normMDG);

responMDA = test_dataset1_normMDA$respon;
responMDG = test_dataset1_normMDG$respon;

source("automation.R");

predRF_MDA = predRandomForest(modelRandomForest_MDA, test_dataset1_normMDA);
predNB_MDA = predNaiveBayes(modelNaiveBayes_MDA, test_dataset1_normMDA);
predCHAID_MDA = predCHAID(modelCHAID_MDA, test_dataset1_normMDA);
predCART_MDA = predCART(modelCART_MDA, test_dataset1_normMDA);
predC50_MDA = predC50(modelC50_MDA, test_dataset1_normMDA);

predSVM_MDA = predCumSVM(modelSVM_MDA, test_dataset1_normMDA);
predSVM_MDA = predCumSVM_3class(modelSVM_MDA, test_dataset1_normMDA);

predRF_MDG = predRandomForest(modelRandomForest_MDG, test_dataset1_normMDG);
predNB_MDG = predNaiveBayes(modelNaiveBayes_MDG, test_dataset1_normMDG);
predCHAID_MDG = predCHAID(modelCHAID_MDG, test_dataset1_normMDG);
predCART_MDG = predCART(modelCART_MDG, test_dataset1_normMDG);
predC50_MDG = predC50(modelC50_MDG, test_dataset1_normMDG);
predSVM_MDG = predCumSVM(modelSVM_MDG, test_dataset1_normMDG);

print(confusionMatrix(predRF_MDA, responMDA));
print(confusionMatrix(predNB_MDA, responMDA));
print(confusionMatrix(predCHAID_MDA, responMDA));
print(confusionMatrix(predCART_MDA, responMDA));
print(confusionMatrix(predC50_MDA, responMDA));
print(confusionMatrix(predSVM_MDA, responMDA));

print(auc(responMDA, predRF_MDA));
print(auc(responMDA, predNB_MDA));
print(auc(responMDA, predCHAID_MDA));
print(auc(responMDA, predCART_MDA));
print(auc(responMDA, predC50_MDA));
print(auc(responMDA, predSVM_MDA));

print(confusionMatrix(predRF_MDG, responMDG));
print(confusionMatrix(predNB_MDG, responMDG));
print(confusionMatrix(predCHAID_MDG, responMDG));
print(confusionMatrix(predCART_MDG, responMDG));
print(confusionMatrix(predC50_MDG, responMDG));
print(confusionMatrix(predSVM_MDG, responMDG));

print(auc(responMDG, predRF_MDG));
print(auc(responMDG, predNB_MDG));
print(auc(responMDG, predCHAID_MDG));
print(auc(responMDG, predCART_MDG));
print(auc(responMDG, predC50_MDG));
print(auc(responMDG, predSVM_MDG));

output = data.frame(test_dataset1_normMDA$respon, predRF_MDA, predC50_MDA, predSVM_MDA, predNB_MDA);
write.csv(output, "C:/Training Data 2301 - Drop Column - 3 Class.csv");

####### Hitung AUC

hasil = read.csv("Experiments/Training Data 2301 - Drop Column - 3 Class.csv", stringsAsFactors = FALSE);

hitungAUC_3class(hasil$test_data.respon, hasil$predRF);
