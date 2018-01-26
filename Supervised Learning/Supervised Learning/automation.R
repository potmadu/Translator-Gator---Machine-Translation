#################################
# Fungsi dan Prosedur
#################################

baca = function(nama_file) {

    output = read.csv(paste("Dataset/",nama_file,sep = ""), stringsAsFactors = FALSE);

    return(output);

}

baca2 = function(nama_file) {

    output = read.csv(paste("C:/Datasets/", nama_file, sep = ""), stringsAsFactors = FALSE);

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

trainCumSVM = function(train_data) {

    print("Train CumSVM");

    train_data5 = subset(train_data, respon == 5, select = -respon);
    train_data54 = subset(train_data, respon >= 4, select = -respon);
    train_data543 = subset(train_data, respon >= 3, select = -respon);
    train_data5432 = subset(train_data, respon >= 2, select = -respon);

    #Best nu=0.001, gamma=0.01 accuracy=78.03
    #model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
    #model54 = svm(train_data54, train_data54$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);
    #model543 = svm(train_data543, train_data543$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
    #model5432 = svm(train_data5432, train_data5432$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);

    model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
    model54 = svm(train_data54, train_data54$respon, type = 'one-classification', nu = 0.1, gamma = 1);
    model543 = svm(train_data543, train_data543$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
    model5432 = svm(train_data5432, train_data5432$respon, type = 'one-classification', nu = 0.1, gamma = 1);

    models = list("model5" = model5, "model54" = model54, "model543" = model543, "model5432" = model5432);

    return(models);

}

trainCumSVM_3class = function(train_data) {

    library(e1071);

    print("Train CumSVM 3 Class");

    train_data3 = subset(train_data, respon == 3, select = -respon);
    train_data32 = subset(train_data, respon == 2, select = -respon);
    train_data321 = subset(train_data, respon == 1, select = -respon);

    #37.02%
    #model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
    #model32 = svm(train_data32, train_data32$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);

    #39.66%
    #model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 0.1);
    #model32 = svm(train_data32, train_data32$respon, type = 'one-classification', nu = 0.1);
    #model321 = svm(train_data321, train_data321$respon, type = 'one-classification', nu = 0.1);

    #37.01%
    #model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 0.001);
    #model32 = svm(train_data32, train_data32$respon, type = 'one-classification', nu = 0.001);
    #model321 = svm(train_data321, train_data321$respon, type = 'one-classification', nu = 0.001);

    #37.01%
    #model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 1);
    #model32 = svm(train_data32, train_data32$respon, type = 'one-classification', nu = 1);
    #model321 = svm(train_data321, train_data321$respon, type = 'one-classification', nu = 1);

    model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 0.1, gamma=0.001);
    model32 = svm(train_data32, train_data32$respon, type = 'one-classification', nu = 0.1, gamma = 0.001);
    model321 = svm(train_data321, train_data321$respon, type = 'one-classification', nu = 0.1, gamma = 0.001);
    
    models = list("model3" = model3, "model32" = model32,"model321"=model321);

    return(models);

}

predCumSVM_3class = function(models, test_data) {

    print("Pred CumSVM 3 class");

    model3 = models$model3;
    model32 = models$model32;
    model321 = models$model321;

    pred3 = predict(model3, subset(test_data, select = -respon));
    pred32 = predict(model32, subset(test_data, select = -respon));
    pred321 = predict(model321, subset(test_data, select = -respon));

    preds = data.frame(pred3, pred32,pred321);
    preds$pred = -1;

    for (i in 1:nrow(preds)) {
        if (preds$pred3[i]) {
            preds$pred[i] = 3;
        } else {
            if (preds$pred32[i]) {
                preds$pred[i] = 2;
            } else {
                preds$pred[i] = 1;
            }
        }
    }

    preds$target = test_data$respon;

    return(preds$pred);

}


akurasiCumSVM = function(models,test_data) {

    print("Akurasi CumSVM");

    model5 = models$model5;
    model54 = models$model54;
    model543 = models$model543;
    model5432 = models$model5432;

    pred5 = predict(model5, subset(test_data, select = -respon));
    pred4 = predict(model54, subset(test_data, select = -respon));
    pred3 = predict(model543, subset(test_data, select = -respon));
    pred2 = predict(model5432, subset(test_data, select = -respon));

    preds = data.frame(pred5, pred4, pred3, pred2);
    preds$pred = -1;

    for (i in 1:nrow(preds)) {
        if (preds$pred5[i]) {
            preds$pred[i] = 5;
        } else {
            if (preds$pred4[i]) {
                preds$pred[i] = 4;
            } else {
                if (preds$pred3[i]) {
                    preds$pred[i] = 3;
                } else {
                    if (preds$pred2[i]) {
                        preds$pred[i] = 2;
                    } else {
                        preds$pred[i] = 1;
                    }
                }
            }
        }
    }

    preds$target = test_data$respon;

    akurasi = data.frame(confusionMatrix(preds$pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

predCumSVM = function(models, test_data) {

    print("Pred CumSVM");

    model5 = models$model5;
    model54 = models$model54;
    model543 = models$model543;
    model5432 = models$model5432;

    pred5 = predict(model5, subset(test_data, select = -respon));
    pred4 = predict(model54, subset(test_data, select = -respon));
    pred3 = predict(model543, subset(test_data, select = -respon));
    pred2 = predict(model5432, subset(test_data, select = -respon));

    preds = data.frame(pred5, pred4, pred3, pred2);
    preds$pred = -1;

    for (i in 1:nrow(preds)) {
        if (preds$pred5[i]) {
            preds$pred[i] = 5;
        } else {
            if (preds$pred4[i]) {
                preds$pred[i] = 4;
            } else {
                if (preds$pred3[i]) {
                    preds$pred[i] = 3;
                } else {
                    if (preds$pred2[i]) {
                        preds$pred[i] = 2;
                    } else {
                        preds$pred[i] = 1;
                    }
                }
            }
        }
    }

    preds$target = test_data$respon;

    return(preds$pred);

}

aucCumSVM = function(models, test_data) {

    print("AUC CumSVM");

    model5 = models$model5;
    model54 = models$model54;
    model543 = models$model543;
    model5432 = models$model5432;

    pred5 = predict(model5, subset(test_data, select = -respon));
    pred4 = predict(model54, subset(test_data, select = -respon));
    pred3 = predict(model543, subset(test_data, select = -respon));
    pred2 = predict(model5432, subset(test_data, select = -respon));

    preds = data.frame(pred5, pred4, pred3, pred2);
    preds$pred = -1;

    for (i in 1:nrow(preds)) {
        if (preds$pred5[i]) {
            preds$pred[i] = 5;
        } else {
            if (preds$pred4[i]) {
                preds$pred[i] = 4;
            } else {
                if (preds$pred3[i]) {
                    preds$pred[i] = 3;
                } else {
                    if (preds$pred2[i]) {
                        preds$pred[i] = 2;
                    } else {
                        preds$pred[i] = 1;
                    }
                }
            }
        }
    }

    preds$target = test_data$respon;

    auc_value = auc(test_data$respon, preds$pred);

    return(auc_value);

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

trainDecisionTree = function(train_data,test_data) {

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

    library(C50);
    library(printr);

    model = C5.0(respon ~ ., data = train_data);
    results = predict(object = model, newdata = test_data, type = "class");
    pred = predict(model, test_data);
    print(confusionMatrix(pred, test_data$respon));

}

trainCHAID = function(train_data) {

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    print("Train CHAID");

    library(party);
    model = ctree(respon ~ ., data = train_data);

    return(model);

}

akurasiCHAID = function(model,test_data) {

    print("Akurasi CHAID");

    pred = predict(model, newdata = test_data);
    akurasi = data.frame(confusionMatrix(pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

predCHAID = function(model, test_data) {

    print("Pred CHAID");

    pred = predict(model, newdata = test_data);
    
    return(pred);

}

aucCHAID = function(model, test_data) {

    print("AUC CHAID");

    pred = predict(model, newdata = test_data);
    auc_value = auc(test_data$respon,pred);

    return(auc_value);

}

trainCART = function(train_data) {

    train_data$respon = as.factor(train_data$respon);

    print("Train CART");

    library(rpart);
    model = rpart(respon ~ ., data = train_data, method="class");

    return(model);

}

akurasiCART = function(model,test_data) {

    print("Akurasi CART");

    test_data$respon = as.factor(test_data$respon);

    pred = predict(model, test_data, type="class");
    akurasi = data.frame(confusionMatrix(pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

predCART = function(model, test_data) {

    print("Pred CART");

    test_data$respon = as.factor(test_data$respon);

    pred = predict(model, test_data, type = "class");

    return(pred);

}

aucCART = function(model, test_data) {

    print("AUC CART");

    test_data$respon = as.factor(test_data$respon);

    pred = predict(model, test_data, type = "class");
    auc_value = auc(test_data$respon,pred);

    return(auc_value);

}

trainC50 = function(train_data) {

    train_data$respon = as.factor(train_data$respon);

    print("Train C50");

    library(C50);
    library(printr);

    model = C5.0(respon ~ ., data = train_data);

    return(model);

}

akurasiC50 = function(model,test_data) {

    print("Akurasi C50");

    pred = predict(model, test_data);
    akurasi = data.frame(confusionMatrix(pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

predC50 = function(model, test_data) {

    print("Pred C50");

    pred = predict(model, test_data);

    return(pred);

}

aucC50 = function(model, test_data) {

    print("AUC C50");

    pred = predict(model, test_data);
    auc_value = auc(test_data$respon,pred);

    return(auc_value);

}

trainRandomForest = function(train_data) {

    print("Train Random Forest");

    library(caret);
    library(randomForest);
    library(e1071);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    model = randomForest(respon ~ ., data = train_data,importance=TRUE);

    return(model);

}

akurasiRandomForest = function(model,test_data) {

    print("Akurasi Random Forest");

    pred = predict(model, newdata = test_data);
    akurasi = data.frame(confusionMatrix(pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

predRandomForest = function(model, test_data) {

    print("Pred Random Forest");

    pred = predict(model, newdata = test_data);

    return(pred);

}

aucRandomForest = function(model, test_data) {

    print("AUC Random Forest");

    pred = predict(model, newdata = test_data);
    auc_value = auc(pred, test_data$respon);

    return(auc_value);

}

trainNaiveBayes = function(train_data) {

    print("Train Naive Bayes");

    library(e1071);

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    model = naiveBayes(respon ~ ., data = train_data)

    return(model);

}

predNaiveBayes = function(model,test_data) {

    print("Pred Naive Bayes");

    pred = predict(model, newdata = subset(test_data, select = -respon));

    return(pred);

}

akurasiNaiveBayes = function(model, test_data) {

    print("Akurasi Naive Bayes");

    pred = predict(model, newdata = subset(test_data, select = -respon));
    akurasi = data.frame(confusionMatrix(pred, test_data$respon)$overall)[1, 1];

    return(akurasi);

}

aucNaiveBayes = function(model, test_data) {

    print("AUC Naive Bayes");

    pred = predict(model, newdata = subset(test_data, select = -respon));
    auc_value = auc(test_data$respon,pred);

    return(auc_value);

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

    train_data$respon = as.factor(train_data$respon);
    test_data$respon = as.factor(test_data$respon);

    importances = data.frame(importance(modelRandomForest));
    kolom_train_data = colnames(train_data)[1:ncol(train_data) - 1];
    importances$kolom = kolom_train_data;
    importances = rbind(importances, importances[1,]);

    importancesMDA = importances %>%
    select(kolom, MeanDecreaseAccuracy) %>%
    arrange(-MeanDecreaseAccuracy) %>%
    as.data.frame();

    importancesMDG = importances %>%
    select(kolom, MeanDecreaseGini) %>%
    arrange(-MeanDecreaseGini) %>%
    as.data.frame();

    importancesMDA$results = -1;

    for (i in 1:nrow(importancesMDA)) {

        kolom = importancesMDA$kolom[1:(nrow(importancesMDA) - i)];

        train_dataset1_normMDA = train_data %>%
        select(kolom) %>%
        as.data.frame();

        train_dataset1_normMDA$respon = as.factor(train_data$respon);

        test_dataset1_normMDA = test_data %>%
        select(kolom) %>%
        as.data.frame();

        test_dataset1_normMDA$respon = as.factor(test_data$respon);

        model = randomForest(respon ~ ., data = train_data, importance = TRUE);
        pred = predict(model, newdata = test_data);
        hasil = confusionMatrix(pred, test_data$respon);
        akurasi = as.numeric(hasil$overall[1]);

        importancesMDA$results[i] = akurasi;

    }

    return(importancesMDA);

}

# Build your own `normalize()` function
normalize <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return(num / denom)
};


trainSoftmax = function(train_data,test_data) {

    library(keras);

    train_data$respon = as.numeric(train_data$respon)-1;
    test_data$respon = as.numeric(test_data$respon) - 1;

    train_data_norm = as.data.frame(lapply(train_data[1:(ncol(train_data)-1)], normalize));
    test_data_norm = as.data.frame(lapply(test_data[1:(ncol(test_data) - 1)], normalize));
    train_data_norm$respon = train_data$respon;
    test_data_norm$respon = test_data$respon;

    train_data_matrix = as.matrix(train_data);
    test_data_matrix = as.matrix(test_data);

    dimnames(train_data_matrix) = NULL;
    dimnames(test_data_matrix) = NULL;

    train_data_matrix_wTarget = train_data_matrix[, 1:(ncol(train_data_matrix) - 1)];
    train_data_matrix_target = train_data_matrix[, ncol(train_data_matrix)];

    test_data_matrix_wTarget = test_data_matrix[, 1:(ncol(test_data_matrix) - 1)];
    test_data_matrix_target = test_data_matrix[, ncol(test_data_matrix)];

    train_data_matrix_labels = to_categorical(train_data_matrix_target);
    test_data_matrix_labels = to_categorical(test_data_matrix_target);

    # Initialize a sequential model
    model = keras_model_sequential()

    # Add layers to the model
    # BEST RESULTS 1 hidden 180 units : 77.80%
    # 2 hidden: 180 relu units + 90 relu units : 78.49968%
    model %>%
    layer_dense(units = 180, activation = 'relu', input_shape = c(90)) %>%
    layer_dense(units = 5, activation = 'softmax');

    # Compile the model
    model %>% compile(
     loss = 'categorical_crossentropy',
     optimizer = 'adam',
     metrics = 'accuracy'
     )

    # Store the fitting history in `history` 
    history <- model %>% fit(
     train_data_matrix_wTarget,
     train_data_matrix_labels,
     epochs = 200,
     batch_size = 5,
     validation_split = 0.2
    )

    # Predict the classes for the test data
    classes = model %>% predict_classes(test_data_matrix_wTarget, batch_size = 128)

    # Evaluate on test data and labels
    score = model %>% evaluate(test_data_matrix_wTarget, test_data_matrix_labels, batch_size = 128)

    # Print the score
    print(score)

    return(output);

}

fsRandomForest = function(modelRandomForest,train_data,test_data) {

    importances = data.frame(importance(modelRandomForest));
    kolom_train_data = colnames(train_data)[1:ncol(train_data) - 1];
    importances$kolom = kolom_train_data;
    importances = rbind(importances, importances[1,]);

    samples = seq(0, 0.9, 0.1);

    output = data.frame(samples);
    output$randomforestMDA = -1;
    output$svmMDA = -1;   
    output$chaidMDA = -1;
    output$cartMDA = -1;
    output$c50MDA = -1;
    output$naivebayesMDA = -1;
    output$randomforestMDG = -1;
    output$svmMDG = -1;
    output$chaidMDG = -1;
    output$cartMDG = -1;
    output$c50MDG = -1;
    output$naivebayesMDG = -1;

    iter = 1;

    for (i in samples) {

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

        test_dataset1_normMDG$respon = test_data$respon;

        modelRF_MDA = trainRandomForest(train_dataset1_normMDA);
        modelCHAID_MDA = trainCHAID(train_dataset1_normMDA);
        modelCART_MDA = trainCART(train_dataset1_normMDA);
        modelC50_MDA = trainC50(train_dataset1_normMDA);
        modelNB_MDA = trainNaiveBayes(train_dataset1_normMDA);
        modelSVM_MDA = trainCumSVM(train_dataset1_normMDA);

        modelRF_MDG = trainRandomForest(train_dataset1_normMDG);
        modelCHAID_MDG = trainCHAID(train_dataset1_normMDG);
        modelCART_MDG = trainCART(train_dataset1_normMDG);
        modelC50_MDG = trainC50(train_dataset1_normMDG);
        modelNB_MDG = trainNaiveBayes(train_dataset1_normMDG);
        modelSVM_MDG = trainCumSVM(train_dataset1_normMDG);

        output$randomforestMDA[iter] = akurasiRandomForest(modelRF_MDA, test_dataset1_normMDA);
        output$chaidMDA[iter] = akurasiCHAID(modelCHAID_MDA, test_dataset1_normMDA);
        output$cartMDA[iter] = akurasiCART(modelCART_MDA, test_dataset1_normMDA);
        output$c50MDA[iter] = akurasiC50(modelC50_MDA, test_dataset1_normMDA);
        output$naivebayesMDA[iter] = akurasiNaiveBayes(modelNB_MDA, test_dataset1_normMDA);
        output$svmMDA[iter] = akurasiCumSVM(modelSVM_MDA, test_dataset1_normMDA);

        output$randomforestMDG[iter] = akurasiRandomForest(modelRF_MDG, test_dataset1_normMDG);
        output$chaidMDG[iter] = akurasiCHAID(modelCHAID_MDG, test_dataset1_normMDG);
        output$cartMDG[iter] = akurasiCART(modelCART_MDG, test_dataset1_normMDG);
        output$c50MDG[iter] = akurasiC50(modelC50_MDG, test_dataset1_normMDG);
        output$naivebayesMDG[iter] = akurasiNaiveBayes(modelNB_MDG, test_dataset1_normMDG);
        output$svmMDG[iter] = akurasiCumSVM(modelSVM_MDG, test_dataset1_normMDG);

        iter = iter + 1;

    }

    return(output);

}

fsRandomForest_AccuracyAndROC = function(modelRandomForest, train_data, test_data) {

    library(pROC);

    importances = data.frame(importance(modelRandomForest));
    kolom_train_data = colnames(train_data)[1:ncol(train_data) - 1];
    importances$kolom = kolom_train_data;
    importances = rbind(importances, importances[1,]);

    #samples = seq(0, 0.9, 0.1);
    samples = 0.9;

    output = data.frame(samples);

    output$randomforestMDA_12 = -1;
    output$randomforestMDA_23 = -1;
    output$randomforestMDA_34 = -1;
    output$randomforestMDA_45 = -1;

    output$randomforestMDG_12 = -1;
    output$randomforestMDG_23 = -1;
    output$randomforestMDG_34 = -1;
    output$randomforestMDG_45 = -1;

    output$chaidMDA_12 = -1;
    output$chaidMDA_23 = -1;
    output$chaidMDA_34 = -1;
    output$chaidMDA_45 = -1;

    output$chaidMDG_12 = -1;
    output$chaidMDG_23 = -1;
    output$chaidMDG_34 = -1;
    output$chaidMDG_45 = -1;

    output$cartMDA_12 = -1;
    output$cartMDA_23 = -1;
    output$cartMDA_34 = -1;
    output$cartMDA_45 = -1;

    output$cartMDG_12 = -1;
    output$cartMDG_23 = -1;
    output$cartMDG_34 = -1;
    output$cartMDG_45 = -1;

    output$c50MDA_12 = -1;
    output$c50MDA_23= -1;
    output$c50MDA_34 = -1;
    output$c50MDA_45 = -1;

    output$c50MDG_12 = -1;
    output$c50MDG_23 = -1;
    output$c50MDG_34 = -1;
    output$c50MDG_45 = -1;

    output$naivebayesMDA_12 = -1;
    output$naivebayesMDA_23 = -1;
    output$naivebayesMDA_34 = -1;
    output$naivebayesMDA_45 = -1;

    output$naivebayesMDG_12 = -1;
    output$naivebayesMDG_23 = -1;
    output$naivebayesMDG_34 = -1;
    output$naivebayesMDG_45 = -1;

    output$svmMDA_12 = -1;
    output$svmMDA_23 = -1;
    output$svmMDA_34 = -1;
    output$svmMDA_45 = -1;

    output$svmMDG_12 = -1;
    output$svmMDG_23 = -1;
    output$svmMDG_34 = -1;
    output$svmMDG_45 = -1;

    iter = 1;

    for (i in samples) {

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

        test_dataset1_normMDG$respon = test_data$respon;

        modelRF_MDA = trainRandomForest(train_dataset1_normMDA);
        modelCHAID_MDA = trainCHAID(train_dataset1_normMDA);
        modelCART_MDA = trainCART(train_dataset1_normMDA);
        modelC50_MDA = trainC50(train_dataset1_normMDA);
        modelNB_MDA = trainNaiveBayes(train_dataset1_normMDA);
        modelSVM_MDA = trainCumSVM(train_dataset1_normMDA);

        modelRF_MDG = trainRandomForest(train_dataset1_normMDG);
        modelCHAID_MDG = trainCHAID(train_dataset1_normMDG);
        modelCART_MDG = trainCART(train_dataset1_normMDG);
        modelC50_MDG = trainC50(train_dataset1_normMDG);
        modelNB_MDG = trainNaiveBayes(train_dataset1_normMDG);
        modelSVM_MDG = trainCumSVM(train_dataset1_normMDG);

        hasilMDA = data.frame(test_dataset1_normMDA$respon);
        colnames(hasilMDA) = c("respon");
        hasilMDG = data.frame(test_dataset1_normMDG$respon);
        colnames(hasilMDG) = c("respon");

        hasilMDA$predRF = predRandomForest(modelRF_MDA, test_dataset1_normMDA);
        hasilMDA$predCHAID = predCHAID(modelCHAID_MDA, test_dataset1_normMDA);
        hasilMDA$predCART = predCART(modelCART_MDA, test_dataset1_normMDA);
        hasilMDA$predC50 = predC50(modelC50_MDA, test_dataset1_normMDA);
        hasilMDA$predNB = predNaiveBayes(modelNB_MDA, test_dataset1_normMDA);
        hasilMDA$predSVM = predCumSVM(modelSVM_MDA, test_dataset1_normMDA);

        hasilMDG$predRF = predRandomForest(modelRF_MDG, test_dataset1_normMDG);
        hasilMDG$predCHAID = predCHAID(modelCHAID_MDG, test_dataset1_normMDG);
        hasilMDG$predCART = predCART(modelCART_MDG, test_dataset1_normMDG);
        hasilMDG$predC50 = predC50(modelC50_MDG, test_dataset1_normMDG);
        hasilMDG$predNB = predNaiveBayes(modelNB_MDG, test_dataset1_normMDG);
        hasilMDG$predSVM = predCumSVM(modelSVM_MDG, test_dataset1_normMDG);

        test_dataset1_MDA12 = subset(test_dataset1_normMDA,respon==1 | respon==2);
        test_dataset1_MDA23 = subset(test_dataset1_normMDA, respon == 2 | respon == 3);
        test_dataset1_MDA34 = subset(test_dataset1_normMDA, respon == 3 | respon == 4);
        test_dataset1_MDA45 = subset(test_dataset1_normMDA, respon == 4 | respon == 5);

        test_dataset1_MDG12 = subset(test_dataset1_normMDG, respon == 1 | respon == 2);
        test_dataset1_MDG23 = subset(test_dataset1_normMDG, respon == 2 | respon == 3);
        test_dataset1_MDG34 = subset(test_dataset1_normMDG, respon == 3 | respon == 4);
        test_dataset1_MDG45 = subset(test_dataset1_normMDG, respon == 4 | respon == 5);

        output$randomforestMDA[iter] = akurasiRandomForest(modelRF_MDA, test_dataset1_normMDA);
        output$chaidMDA[iter] = akurasiCHAID(modelCHAID_MDA, test_dataset1_normMDA);
        output$cartMDA[iter] = akurasiCART(modelCART_MDA, test_dataset1_normMDA);
        output$c50MDA[iter] = akurasiC50(modelC50_MDA, test_dataset1_normMDA);
        output$naivebayesMDA[iter] = akurasiNaiveBayes(modelNB_MDA, test_dataset1_normMDA);
        output$svmMDA[iter] = akurasiCumSVM(modelSVM_MDA, test_dataset1_normMDA);

        output$randomforestMDG[iter] = akurasiRandomForest(modelRF_MDG, test_dataset1_normMDG);
        output$chaidMDG[iter] = akurasiCHAID(modelCHAID_MDG, test_dataset1_normMDG);
        output$cartMDG[iter] = akurasiCART(modelCART_MDG, test_dataset1_normMDG);
        output$c50MDG[iter] = akurasiC50(modelC50_MDG, test_dataset1_normMDG);
        output$naivebayesMDG[iter] = akurasiNaiveBayes(modelNB_MDG, test_dataset1_normMDG);
        output$svmMDG[iter] = akurasiCumSVM(modelSVM_MDG, test_dataset1_normMDG);

        #AUC

        output$randomforestMDA_12[iter] = aucRandomForest(modelRF_MDA, test_dataset1_normMDA12);
        output$randomforestMDA_23[iter] = aucRandomForest(modelRF_MDA, test_dataset1_normMDA23);
        output$randomforestMDA_34[iter] = aucRandomForest(modelRF_MDA, test_dataset1_normMDA34);
        output$randomforestMDA_45[iter] = aucRandomForest(modelRF_MDA, test_dataset1_normMDA45);

        output$randomforestMDG_12[iter] = aucRandomForest(modelRF_MDG, test_dataset1_normMDG12);
        output$randomforestMDG_23[iter] = aucRandomForest(modelRF_MDG, test_dataset1_normMDG23);
        output$randomforestMDG_34[iter] = aucRandomForest(modelRF_MDG, test_dataset1_normMDG34);
        output$randomforestMDG_45[iter] = aucRandomForest(modelRF_MDG, test_dataset1_normMDG45);

        output$chaidMDA_12[iter] = aucCHAID(modelCHAID_MDA, test_dataset1_normMDA12);
        output$chaidMDA_23[iter] = aucCHAID(modelCHAID_MDA, test_dataset1_normMDA23);
        output$chaidMDA_34[iter] = aucCHAID(modelCHAID_MDA, test_dataset1_normMDA34);
        output$chaidMDA_45[iter] = aucCHAID(modelCHAID_MDA, test_dataset1_normMDA45);

        output$chaidMDG_12[iter] = aucCHAID(modelCHAID_MDG, test_dataset1_normMDG12);
        output$chaidMDG_23[iter] = aucCHAID(modelCHAID_MDG, test_dataset1_normMDG23);
        output$chaidMDG_34[iter] = aucCHAID(modelCHAID_MDG, test_dataset1_normMDG34);
        output$chaidMDG_45[iter] = aucCHAID(modelCHAID_MDG, test_dataset1_normMDG45);

        output$cartMDA_12[iter] = aucCART(modelCART_MDA, test_dataset1_normMDA12);
        output$cartMDA_23[iter] = aucCART(modelCART_MDA, test_dataset1_normMDA23);
        output$cartMDA_34[iter] = aucCART(modelCART_MDA, test_dataset1_normMDA34);
        output$cartMDA_45[iter] = aucCART(modelCART_MDA, test_dataset1_normMDA45);
        
        output$cartMDG_12[iter] = aucCART(modelCART_MDG, test_dataset1_normMDG12);
        output$cartMDG_23[iter] = aucCART(modelCART_MDG, test_dataset1_normMDG23);
        output$cartMDG_34[iter] = aucCART(modelCART_MDG, test_dataset1_normMDG34);
        output$cartMDG_45[iter] = aucCART(modelCART_MDG, test_dataset1_normMDG45);
        
        output$c50MDA_12[iter] = aucC50(modelC50_MDA, test_dataset1_normMDA12);
        output$c50MDA_23[iter] = aucC50(modelC50_MDA, test_dataset1_normMDA23);
        output$c50MDA_34[iter] = aucC50(modelC50_MDA, test_dataset1_normMDA34);
        output$c50MDA_45[iter] = aucC50(modelC50_MDA, test_dataset1_normMDA45);

        output$c50MDG_12[iter] = aucC50(modelC50_MDG, test_dataset1_normMDG12);
        output$c50MDG_23[iter] = aucC50(modelC50_MDG, test_dataset1_normMDG23);
        output$c50MDG_34[iter] = aucC50(modelC50_MDG, test_dataset1_normMDG34);
        output$c50MDG_45[iter] = aucC50(modelC50_MDG, test_dataset1_normMDG45);

        output$naivebayesMDA_12[iter] = aucNaiveBayes(modelNB_MDA, test_dataset1_normMDA12);
        output$naivebayesMDA_23[iter] = aucNaiveBayes(modelNB_MDA, test_dataset1_normMDA23);
        output$naivebayesMDA_34[iter] = aucNaiveBayes(modelNB_MDA, test_dataset1_normMDA34);
        output$naivebayesMDA_45[iter] = aucNaiveBayes(modelNB_MDA, test_dataset1_normMDA45);

        output$naivebayesMDG_12[iter] = aucNaiveBayes(modelNB_MDG, test_dataset1_normMDG12);
        output$naivebayesMDG_23[iter] = aucNaiveBayes(modelNB_MDG, test_dataset1_normMDG23);
        output$naivebayesMDG_34[iter] = aucNaiveBayes(modelNB_MDG, test_dataset1_normMDG34);
        output$naivebayesMDG_45[iter] = aucNaiveBayes(modelNB_MDG, test_dataset1_normMDG45);

        output$svmMDA_12[iter] = aucCumSVM(modelSVM_MDA, test_dataset1_normMDA12);
        output$svmMDA_23[iter] = aucCumSVM(modelSVM_MDA, test_dataset1_normMDA23);
        output$svmMDA_34[iter] = aucCumSVM(modelSVM_MDA, test_dataset1_normMDA34);
        output$svmMDA_45[iter] = aucCumSVM(modelSVM_MDA, test_dataset1_normMDA45);

        output$svmMDG_12[iter] = aucCumSVM(modelSVM_MDG, test_dataset1_normMDG12);
        output$svmMDG_23[iter] = aucCumSVM(modelSVM_MDG, test_dataset1_normMDG23);
        output$svmMDG_34[iter] = aucCumSVM(modelSVM_MDG, test_dataset1_normMDG34);
        output$svmMDG_45[iter] = aucCumSVM(modelSVM_MDG, test_dataset1_normMDG45);

        iter = iter + 1;

    }

    return(output);

}

hitungAUC_5class = function(aktual, prediksi) {

    library(pROC);

    df = data.frame(aktual, as.numeric(prediksi));
    colnames(df) = c("aktual", "prediksi");

    df12 = subset(df, aktual == 1 | aktual == 2);
    df23 = subset(df, aktual == 2 | aktual == 3);
    df34 = subset(df, aktual == 3 | aktual == 4);
    df45 = subset(df, aktual == 4 | aktual == 5);

    df12$prediksi[df12$prediksi != 1] = 2;
    df23$prediksi[df23$prediksi != 2] = 3;
    df34$prediksi[df34$prediksi != 3] = 4;
    df45$prediksi[df45$prediksi != 4] = 5;

    auc12 = auc(df12$aktual, df12$prediksi)[1];
    auc23 = auc(df23$aktual, df23$prediksi)[1];
    auc34 = auc(df34$aktual, df34$prediksi)[1];
    auc45 = auc(df45$aktual, df45$prediksi)[1];
    rataAUC = (auc12 + auc23 + auc34 + auc45) / 4;

    hasil_auc = list("auc12" = auc12, "auc23" = auc23, "auc34" = auc34, "auc45" = auc45, "rataAUC" = rataAUC);

    return(hasil_auc);

}

hitungAUC_3class = function(aktual, prediksi) {

    library(pROC);

    df = data.frame(aktual, as.numeric(prediksi));
    colnames(df) = c("aktual", "prediksi");

    df12 = subset(df, aktual == 1 | aktual == 2);
    df23 = subset(df, aktual == 2 | aktual == 3);

    df12$prediksi[df12$prediksi != 1] = 2;
    df23$prediksi[df23$prediksi != 2] = 3;

    auc12 = auc(df12$aktual, df12$prediksi)[1];
    auc23 = auc(df23$aktual, df23$prediksi)[1];
    rataAUC = (auc12 + auc23) / 2;

    hasil_auc = list("auc12" = auc12, "auc23" = auc23, "rataAUC" = rataAUC);

    return(hasil_auc);

}
