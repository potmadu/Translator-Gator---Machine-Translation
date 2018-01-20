library(Kendall);

Kendall(test_dataset1_norm$respon, test_dataset1_norm$respon);

test_dataset1_norm$pred = as.numeric(test_dataset1_norm$respon);
test_dataset1_norm$respon = as.numeric(test_dataset1_norm$respon);

data12 = subset(test_dataset1_norm, test_dataset1_norm$respon == 1 | test_dataset1_norm$respon == 2);
data23 = subset(test_dataset1_norm, test_dataset1_norm$respon == 2 | test_dataset1_norm$respon == 3);
data34 = subset(test_dataset1_norm, test_dataset1_norm$respon == 3 | test_dataset1_norm$respon == 4);
data45 = subset(test_dataset1_norm, test_dataset1_norm$respon == 4 | test_dataset1_norm$respon == 5);

data12$pred[data12$respon != 1] = 2;
data23$pred[data23$respon != 2] = 3;
data34$pred[data34$respon != 3] = 4;
data45$pred[data45$respon != 4] = 5;

data23 %>% group_by(respon,pred) %>% summarise(jumlah_data = n()) %>% as.data.frame();

wilcox.test(data12$pred, data12$respon);
wilcox.test(data23$pred, data23$respon);
wilcox.test(data34$pred, data34$respon);
wilcox.test(data45$pred, data45$respon);

#Using As Is Dataset

train_data_back = train_data
test_data_back = test_data
train_data = train_dataset1_norm
test_data = test_dataset1_norm

train_data$respon = as.factor(train_data$respon);
test_data$respon = as.factor(test_data$respon);

pred = predict(modelRandomForest, newdata = test_data);

print(table(pred, test_data$respon));
print(confusionMatrix(pred, test_data$respon));
#85.21% Accuracy

test_data$pred = as.numeric(pred);
test_data$respon = as.numeric(test_data$respon);

Kendall(pred, test_data$respon);

data12 = subset(test_data, test_data$respon == 1 | test_data$respon == 2);
data23 = subset(test_data, test_data$respon == 2 | test_data$respon == 3);
data34 = subset(test_data, test_data$respon == 3 | test_data$respon == 4);
data45 = subset(test_data, test_data$respon == 4 | test_data$respon == 5);

data12$pred[data12$pred != 1] = 2;
data23$pred[data23$pred != 2] = 3;
data34$pred[data34$pred != 3] = 4;
data45$pred[data45$pred != 4] = 5;

data45 %>% group_by(respon, pred) %>% summarise(jumlah_data = n()) %>% as.data.frame();

wilcox.test(data12$respon, data12$pred);
wilcox.test(data23$respon, data23$pred);
wilcox.test(data34$respon, data34$pred);
wilcox.test(data45$respon, data45$pred);

wilcox.test(data12$respon, data12$pred, paired = TRUE, correct=FALSE);
wilcox.test(data23$respon, data23$pred, paired = TRUE, correct = FALSE);
wilcox.test(data34$respon, data34$pred, paired = TRUE, correct = FALSE);
wilcox.test(data45$respon, data45$pred, paired = TRUE, correct = FALSE);

auc(data12$respon, data12$pred);
auc(data23$respon, data23$pred);
auc(data34$respon, data34$pred);
auc(data45$respon, data45$pred);

