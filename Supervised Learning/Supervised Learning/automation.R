#################################
# Fungsi dan Prosedur
#################################

baca = function(nama_file) {

    output = read.csv(paste("Dataset/",nama_file,sep = ""), stringsAsFactors = FALSE);

    return(output);

}

partisi = function(input_data,response_name) {

    #70/30 partition
    #column response as y

    colnames(input_data)[colnames(input_data) == response_name] = "respon";
    partition_data = createDataPartition(input_data$respon, p = 0.7, list = FALSE, times = 1);

    dataTrain = input_data[partition_data,];
    dataTrain$tipe = "training";
    dataTest = input_data[-partition_data,];
    dataTest$tipe = "testing";

    output_data = rbind(dataTrain, dataTest);

    return(output_data);

}

hitungSVM2 = function(train_data,test_data, target_respon, nu_value, gamma_value) {
    
    library(e1071);

    model = svm(subset(train_data,select = -respon), train_data$respon, type = 'one-classification', nu = nu_value, gamma=gamma_value);

    pred_trains = predict(model, subset(train_data,select= -respon));
    pred_test = predict(model, subset(test_data, select = -respon));

    pred_data = test_data;
    pred_data$pred = pred_test;
    pred_data$flag = 0;
    pred_data$flag[pred_data$respon == target_respon & pred_data$pred == TRUE] = 1;
    pred_data$flag[pred_data$respon != target_respon & pred_data$pred == FALSE] = 1;

    print(model);

    pred_train = train_data;
    pred_train$pred = pred_trains;
    pred_train$flag = 1;

    print(paste("Training Accuracy : ", nrow(subset(pred_train, pred_train$pred == TRUE)) / nrow(pred_train)));
    print(paste("Testing Accuracy : ", nrow(subset(pred_data, flag == 1)) / nrow(pred_data)));

    pred_train$tipe = "TRAIN";
    pred_data$tipe = "TEST";

    train_test_data = rbind(pred_train, pred_data);

    return(train_test_data);

}

hitungSVM = function(input_data,target_respon,nu_value) {

    library(e1071);

    train_data = subset(input_data, tipe == "training");
    train_data$tipe = NULL;
    test_data = subset(input_data, tipe == "testing");
    test_data$tipe = NULL;

    df = subset(train_data, respon == target_respon);
    x = subset(df, select = -respon);
    y = df$respon;

    model = svm(x, y, type = 'one-classification',nu=nu_value);

    pred_trains = predict(model, x);
    pred_test = predict(model, subset(test_data, select = -respon));

    pred_data = test_data;
    pred_data$pred = pred_test;
    pred_data$flag = 0;
    pred_data$flag[pred_data$respon == target_respon & pred_data$pred == TRUE] = 1;
    pred_data$flag[pred_data$respon != target_respon & pred_data$pred == FALSE] = 1;

    print(model);

    pred_train = df;
    pred_train$pred = pred_trains;
    pred_train$flag = 1;

    print(paste("Training Accuracy : ", nrow(subset(pred_train, pred_train$pred == TRUE)) / nrow(pred_train)));
    print(paste("Testing Accuracy : ", nrow(subset(pred_data, flag == 1)) / nrow(pred_data)));

    pred_train$tipe = "TRAIN";
    pred_data$tipe = "TEST";

    train_test_data = rbind(pred_train, pred_data);

    return(train_test_data);

}

hitungSVM_multi = function(input_data, target_respon) {

    library(e1071);

    train_data = subset(input_data, tipe == "training");
    train_data$tipe = NULL;
    test_data = subset(input_data, tipe == "testing");
    test_data$tipe = NULL;

    df = subset(train_data);
    x = subset(df, select = -respon);
    y = as.factor(df$respon);

    model = svm(x, y, probability = TRUE);
    pred_train = predict(model, x, decision.values = TRUE, probability = TRUE);
    pred_test = predict(model, subset(test_data, select = -respon),decision.values = TRUE, probability = TRUE);

    #normalization and find specific prediction value 
    pred_train_value = as.data.frame(attr(pred_train, "probabilities"));
    pred_test_value = as.data.frame(attr(pred_test, "probabilities"));

    pred_train_value$pred = -1;
    for (i in 1:nrow(pred_train_value)) {

        max_prob = pred_train_value[i,1];
        max_pos = 1;

        for (j in 2:5) {
            current_prob = pred_train_value[i, j];
            current_pos = j;

            if (current_prob>max_prob) {
                max_prob = current_prob;
                max_pos = current_pos;
            }
        }

        pred_train_value$pred[i] = max_pos;
    }

    pred_test_value$pred = -1;
    for (i in 1:nrow(pred_test_value)) {

        max_prob = pred_test_value[i, 1];
        max_pos = 1;

        for (j in 2:5) {
            current_prob = pred_test_value[i, j];
            current_pos = j;

            if (current_prob > max_prob) {
                max_prob = current_prob;
                max_pos = current_pos;
            }
        }

        pred_test_value$pred[i] = max_pos;
    }

    pred_data = test_data;
    pred_data$pred = pred_test_value$pred;
    pred_data$flag = 0;
    pred_data$flag[pred_data$respon == pred_data$pred] = 1;

    print(model);

    pred_train = df;
    pred_train$pred = pred_train_value$pred;
    pred_train$flag = 0;
    pred_train$flag[pred_train$respon == pred_train$pred] = 1;

    print(paste("Training Accuracy : ", nrow(subset(pred_train, pred_train$pred == TRUE)) / nrow(pred_train)));
    print(paste("Testing Accuracy : ", nrow(subset(pred_data, flag == 1)) / nrow(pred_data)));

    pred_train$tipe = "TRAIN";
    pred_data$tipe = "TEST";

    train_test_data = rbind(pred_train, pred_data);

    return(train_test_data);

}

hitungSVM_multi2 = function(train_data,test_data, c_value, gamma_value) {

    library(e1071);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    model = svm(subset(train_data,select=-respon), train_data$respon, probability = TRUE,C=c_value,gamma=gamma_value);
    pred_train = predict(model, subset(train_data, select = -respon), decision.values = TRUE, probability = TRUE);
    pred_test = predict(model, subset(test_data, select = -respon), decision.values = TRUE, probability = TRUE);

    #normalization and find specific prediction value 
    pred_train_value = as.data.frame(attr(pred_train, "probabilities"));
    pred_test_value = as.data.frame(attr(pred_test, "probabilities"));

    pred_train_value$pred = -1;
    for (i in 1:nrow(pred_train_value)) {

        max_prob = pred_train_value[i, 1];
        max_pos = 1;

        for (j in 2:5) {
            current_prob = pred_train_value[i, j];
            current_pos = j;

            if (current_prob > max_prob) {
                max_prob = current_prob;
                max_pos = current_pos;
            }
        }

        pred_train_value$pred[i] = max_pos;
    }

    pred_test_value$pred = -1;
    for (i in 1:nrow(pred_test_value)) {

        max_prob = pred_test_value[i, 1];
        max_pos = 1;

        for (j in 2:5) {
            current_prob = pred_test_value[i, j];
            current_pos = j;

            if (current_prob > max_prob) {
                max_prob = current_prob;
                max_pos = current_pos;
            }
        }

        pred_test_value$pred[i] = max_pos;
    }

    pred_data = test_data;
    pred_data$pred = pred_test_value$pred;
    pred_data$flag = 0;
    pred_data$flag[pred_data$respon == pred_data$pred] = 1;

    print(model);

    pred_train = df;
    pred_train$pred = pred_train_value$pred;
    pred_train$flag = 0;
    pred_train$flag[pred_train$respon == pred_train$pred] = 1;

    print(paste("Training Accuracy : ", nrow(subset(pred_train, pred_train$pred == TRUE)) / nrow(pred_train)));
    print(paste("Testing Accuracy : ", nrow(subset(pred_data, flag == 1)) / nrow(pred_data)));

    pred_train$tipe = "TRAIN";
    pred_data$tipe = "TEST";

    train_test_data = rbind(pred_train, pred_data);

    return(train_test_data);

}

hitungDecisionTree = function(train_data,test_data) {

    library(C50);
    library(printr);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    print("CHAID");

    library(party);
    model = ctree(respon ~ ., data = train_data);
    pred = predict(model, test_data);
    print(confusionMatrix(pred, test_data$respon));

    print("CART");

    library(rpart);
    model = rpart(respon ~ ., data = train_data, method = "class");
    pred = predict(model, test_data, type = "class");
    print(confusionMatrix(pred, test_data$respon));

    print("C50");

    model = C5.0(respon ~ ., data = train_data);
    results = predict(object = model, newdata = test_data, type = "class");
    pred = predict(model, test_data);
    print(confusionMatrix(pred, test_data$respon));

}

hitungRandomForest = function(train_data, test_data) {

    library(caret);
    library(randomForest);
    library(e1071);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    model = randomForest(respon ~ ., data = train_data,importance=TRUE);
    pred = predict(model, newdata = test_data);
    print(table(pred, test_data$respon));
    print(confusionMatrix(pred, test_data$respon));

    return(model);

}

hitungNaiveBayes = function(train_data,test_data) {

    library(e1071);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    model = naiveBayes(respon ~ ., data = train_data)
    pred = predict(model, newdata = subset(test_data, select = -respon));

    print(table(pred, test_data$respon));
    print(confusionMatrix(pred, test_data$respon));

}

hitungNaiveBayes_normalization = function(train_data, test_data) {

    library(e1071)
    library(forecast)

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    for (i in 1:(ncol(train_data)-1)) {
            train_data[, i] = BoxCox(train_data[, i], BoxCox.lambda(train_data[, i]));
    }

    for (i in 1:(ncol(test_data)-1)) {
            test_data[, i] = BoxCox(test_data[, i], BoxCox.lambda(test_data[, i]));
    }

    model = naiveBayes(respon ~ ., data = train_data)
    pred = predict(model, newdata = subset(test_data, select = -respon));

    print(table(pred, test_data$respon));
    print(confusionMatrix(pred, test_data$respon));

}

hitungFeaturesImportance = function(train_data,test_data,modelRandomForest) {

    library(dplyr);

    set.seed(12345);

    importances = data.frame(importance(modelRandomForest));
    kolom_train_data = colnames(train_dataset1_norm)[1:ncol(train_dataset1_norm) - 1];
    importances$kolom = kolom_train_data;
    importances = rbind(importances, importances[1,]);

    importancesMDA = importances %>%
    select(kolom, MeanDecreaseAccuracy) %>%
    arrange(MeanDecreaseAccuracy) %>%
    as.data.frame();

    importancesMDG = importances %>%
    select(kolom, MeanDecreaseGini) %>%
    arrange(MeanDecreaseGini) %>%
    as.data.frame();

    for (i in 1:nrow(importances)) {



    }

}

#################################
# Main Program
#################################

library(imputeTS);
library(dplyr);

train_file = "Training Data.csv";
test_file = "Test Data.csv";
target_column = "manual_assessment";

train_data = baca(train_file);
colnames(train_data)[colnames(train_data) == target_column] = "respon";

test_data = baca(test_file);
colnames(test_data)[colnames(test_data) == target_column] = "respon";

train_data %>%
filter(is.na(respon) | is.na(manual_origin_source) | is.na(origin_word_entropy)) %>%
group_by(manual_origin_source,origin_word_entropy,respon) %>%
summarise(jumlah_data = n());

#Dataset1 = manual_assessment as target
train_dataset1 = train_data;
test_dataset1 = test_data;

#Data Preprocessing
train_dataset1$username = NULL;
train_dataset1$source.word = NULL;
train_dataset1$target.word = NULL;
train_dataset1_assessed = train_dataset1 %>%
filter(!is.na(respon) & !is.na(manual_origin_source) & !is.na(origin_word_entropy)) %>%
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
filter(!is.na(respon) & !is.na(manual_origin_source) & !is.na(origin_word_entropy)) %>%
as.data.frame();
#check na value
colnames(test_dataset1_assessed)[colSums(is.na(test_dataset1_assessed)) > 0]
#normalization by replace NA with 0
test_dataset1_norm = na.replace(test_dataset1_assessed, 0);
colnames(test_dataset1_norm)[colSums(is.na(test_dataset1_norm)) > 0]

######### KALKULASI

hitungFeaturesImportance(train_dataset1_norm,test_dataset1_norm);

modelSVM = hitungSVM(dataset1_part, 5, 0.1);
modelSVM2 = hitungSVM2(train_dataset1_norm, test_dataset1_norm, 5, 0.1, 0.001);

modelSVMMulti = hitungSVM_multi2(train_dataset1_norm, test_dataset1_norm, 0.1, 0.01);
modelSVMMulti = hitungSVM_multi2(train_dataset1_norm, test_dataset1_norm, 0.01, 0.01);
modelSVMMulti = hitungSVM_multi2(train_dataset1_norm, test_dataset1_norm, 1, 0.01);
modelSVMMulti = hitungSVM_multi2(train_dataset1_norm, test_dataset1_norm, 10, 0.01);

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



