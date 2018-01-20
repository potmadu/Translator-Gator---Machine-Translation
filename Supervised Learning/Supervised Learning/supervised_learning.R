realtime_dat = read.csv("Dataset/Feature-User-Word-Translation-Realtime.csv",stringsAsFactors=FALSE);
end_dat = read.csv("Dataset/Feature-User-Word-Translation-At-the-end-ofproject.csv", stringsAsFactors = FALSE);
manual_check = read.csv("Dataset/Manual quality check TG2.csv", stringsAsFactors = FALSE);

manual_check = manual_check[, 1:19];

####################################
# SVM
####################################

library(e1071);

data(iris);
df = iris;

df = subset(df, Species == 'setosa');

x = subset(df, select = -Species);
y = df$Species;
model = svm(x, y, type = 'one-classification',nu=0.10);

print(model);
summary(model);

pred_train = predict(model, x);
pred_test = predict(model, subset(iris, select = -Species));

pred_data = iris;
pred_data$pred = pred_test;
pred_data$flag = 0;
pred_data$flag[pred_data$Species == 'setosa' & pred_data$pred == TRUE] = 1;
nrow(subset(pred_data, flag == 1)) / nrow(pred_data);

pred_train = as.data.frame(pred_train);
nrow(subset(pred_train, pred_train == TRUE)) / nrow(pred_train);

data(iris);
attach(iris);
x = subset(iris, select = -Species);
y = Species;

model = svm(x, y, probability = TRUE);
pred_prob = predict(model, x, decision.values = TRUE, probability = TRUE);

####################################
# Cumulative One-Class SVM
####################################

library(e1071);

train_data_back = train_data;
test_data_back = test_data;

train_data = train_dataset1_normMDA;
test_data = test_dataset1_normMDA;

train_data5 = subset(train_data, respon == 5, select = -respon);
train_data54 = subset(train_data, respon >= 4, select = -respon);
train_data543 = subset(train_data, respon >= 3, select = -respon);
train_data5432 = subset(train_data, respon >= 2, select = -respon);

#Best nu=0.001, gamma=0.01 accuracy=78.03
#model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
#model54 = svm(train_data54, train_data54$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);
#model543 = svm(train_data543, train_data543$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
#model5432 = svm(train_data5432, train_data5432$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);

model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.1,gamma=0.01);
model54 = svm(train_data54, train_data54$respon, type = 'one-classification', nu = 0.1, gamma = 1);
model543 = svm(train_data543, train_data543$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
model5432 = svm(train_data5432, train_data5432$respon, type = 'one-classification', nu = 0.1, gamma = 1);

pred5 = predict(model5, subset(test_data,select=-respon));
pred4 = predict(model54, subset(test_data, select = -respon));
pred3 = predict(model543, subset(test_data, select = -respon));
pred2 = predict(model5432, subset(test_data, select = -respon));

head(pred5);
head(pred4);
head(pred3);
head(pred2);
head(test_data$respon);

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

confusionMatrix(preds$pred, test_data$respon);

test_data %>%
group_by(respon) %>%
summarise(jumlah_data = n(), persentase = n() / 4679) %>%
as.data.frame();

####################################
# Low Vote One-Class SVM
####################################

library(e1071);

train_data_back = train_data;
test_data_back = test_data;

train_data = train_dataset1_norm;
test_data = test_dataset1_norm;

train_data5 = subset(train_data, respon == 5, select = -respon);
train_data4 = subset(train_data, respon == 4, select = -respon);
train_data3 = subset(train_data, respon == 3, select = -respon);
train_data2 = subset(train_data, respon == 2, select = -respon);
train_data1 = subset(train_data, respon == 1, select = -respon);

train_data5$user_is_age_0 = NULL;
train_data4$user_is_age_0 = NULL;
train_data3$user_is_age_0 = NULL;
train_data2$user_is_age_0 = NULL;
train_data1$user_is_age_0 = NULL;

test_data$user_is_age_0 = NULL;

#Best nu=0.001, gamma=0.01 accuracy=78.03
#model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
#model54 = svm(train_data54, train_data54$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);
#model543 = svm(train_data543, train_data543$respon, type = 'one-classification', nu = 0.001, gamma = 0.01);
#model5432 = svm(train_data5432, train_data5432$respon, type = 'one-classification', nu = 0.001, gamma = 0.1);

model5 = svm(train_data5, train_data5$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
model4 = svm(train_data4, train_data4$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
model3 = svm(train_data3, train_data3$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
model2 = svm(train_data2, train_data2$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);
model1 = svm(train_data1, train_data1$respon, type = 'one-classification', nu = 0.1, gamma = 0.01);

pred5 = predict(model5, subset(test_data, select = -respon));
pred4 = predict(model4, subset(test_data, select = -respon));
pred3 = predict(model3, subset(test_data, select = -respon));
pred2 = predict(model2, subset(test_data, select = -respon));
pred1 = predict(model1, subset(test_data, select = -respon));

head(pred5);
head(pred4);
head(pred3);
head(pred2);
head(pred1);
head(test_data$respon);

preds = data.frame(pred5, pred4, pred3, pred2,pred1);
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

confusionMatrix(preds$pred, test_data$respon);

test_data %>%
group_by(respon) %>%
summarise(jumlah_data = n(), persentase = n() / 4679) %>%
as.data.frame();


####################################
# 10-folds Cross Validation
####################################

library(caret);

folds = createFolds(end_dat, k = 10, list = TRUE, returnTrain = FALSE);

####################################

svm_tune = tune(svm, x, y, type = 'one-classification', ranges = list(gamma = seq(0.05, 1, by = 0.05), nu = seq(0.1, 1, by = 0.1)));

svm_tune = tune(svm, x, z, type = 'one-classification', nu = seq(0.1, 1, by = 0.1), gamma = seq(0.05, 1, by = 0.05));

svm_tune = tune(svm, Species ~ ., data = iris, type='one-classification',
ranges = list(gamma = 2 ^ (-1:1), cost = 2 ^ (2:4))
)

svm_cross10 = tune.svm(x,y,subset(iris,select = -Species),nu = 0.1,gamma = 0.5,tunecontrol=)

####################################
# Naive Bayes
####################################

data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)

library(e1071)
model <- naiveBayes(class ~ ., data = breast_cancer)
class(model)
summary(model)
print(model)
preds = predict(model, newdata = breast_cancer);

library(e1071);

train_data = subset(input_data, tipe == "training");
train_data$tipe = NULL;
test_data = subset(input_data, tipe == "testing");
test_data$tipe = NULL;

train_data$respon = as.factor(train_data$respon);
test_data$respon = as.factor(test_data$respon);

model = naiveBayes(respon ~ ., data = train_data);
preds = predict(model, newdata=test_data);
confusionMatrix(preds, test_data$respon);

model = naiveBayes(respon ~ ., data = train_data, laplace = 1);
preds = predict(model, newdata = test_data);
conf_matrix = table(preds, test_data$respon);
conf_matrix;

model = naiveBayes(respon ~ ., data = train_data, laplace = 2);
preds = predict(model, newdata = test_data);
conf_matrix = table(preds, test_data$respon);
conf_matrix;

model = naiveBayes(respon ~ ., data = train_data, laplace = 3);
preds = predict(model, newdata = test_data);
conf_matrix = table(preds, test_data$respon);
conf_matrix;

####################################
# Random Forest
####################################

library(randomForest);
library(MASS);

attach(Boston);

dim(Boston);
set.seed(101);

train = sample(1:nrow(Boston), 300);

Boston.rf = randomForest(medv ~ ., data = Boston, subset = train);

Boston.rf;

oob.err = double(13);
test.err = double(13);

for (mtry in 1:13) {
    rf = randomForest(medv ~ ., data = Boston, subset = train, mtry = mtry, ntree = 400);
    oob.err[mtry] = rf$mse[400];
    pred = predict(rf, Boston[-train,]);
    test.err[mtry] = with(Boston[-train,], mean((medv - pred) ^ 2));
    cat(mtry," ")
}

library(randomForest)
library(mlbench)
library(caret)
# Load Dataset
data(Sonar)
dataset <- Sonar
x <- dataset[, 1:60]
y <- dataset[, 61]
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry = mtry)
rf_default <- train(Class ~ ., data = dataset, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control)
print(rf_default)

url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine <- read.table(url)

library(randomForest)

train_data$respon = as.factor(train_data$respon);
test_data$respon = as.factor(test_data$respon);

model <- randomForest(respon ~ ., data = train_data)
pred <- predict(model, newdata = test_data)
table(pred, test_data$respon)
confusionMatrix(pred, test_data$respon)

####################################
# Decision Tree
####################################

library(rpart);
fit = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis);
fit2 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,parms = list(prior=c(.65,.35),split="information"));
fit3 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, control = rpart.control(cp = 0.05));
par(mfrow = c(1, 2), xpd = NA);
plot(fit);
text(fit, use.n = TRUE);
plot(fit2);
text(fit2, use.n = TRUE);

#CHAID

library(party);
model = ctree(respon ~ ., data = train_data);
pred = table(predict(model), test_data$respon);
confusionMatrix(pred,test_data$respon);

#CART

model = rpart(respon ~ ., data = train_data, method = "class");
pred = predict(model, test_data, type = "class");
confusionMatrix(pred, test_data$respon)

#C4.5

library(RWeka);
library(caret);

set.seed(1958);
model = train(respon ~ ., data = train_data, method = "J48");
pred = predict(model, test_data);
confusionMatrix(pred, test_data$respon);

#C5.0

library(C50);
library(printr);

model = C5.0(respon ~ ., data = train_data);
results = predict(object = model, newdata = test_data, type = "class");
pred = predict(model, test_data);
confusionMatrix(pred, test_data$respon);

###################################

z.auto = rpart(Mileage ~ Weight, car.test.frame);
predict(z.auto);

fit = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis);
predict(fit, type = "prob");
predict(fit, type = "vector");
predict(fit, type = "class");
predict(fit, type = "matrix");

sub = c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25));
fit = rpart(Species ~ ., data = iris, subset = sub);
fit;
table(predict(fit, iris[-sub,], type = "class"), iris[-sub, "Species"]);

####################################
# Softmax Classifier
####################################

library(keras);

data(iris);

# Build your own `normalize()` function
normalize <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return(num / denom)
};

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize));

iris[, 5] <- as.numeric(iris[, 5]) - 1;

# Turn `iris` into a matrix
iris <- as.matrix(iris);

# Set iris `dimnames` to `NULL`
dimnames(iris) <- NULL;

# Normalize the `iris` data
iris2 <- normalize(iris[, 1:4])

# Return the summary of `iris`
summary(iris2);

# Determine sample size
ind <- sample(2, nrow(iris2), replace = TRUE, prob = c(0.80, 0.20))

# Split the `iris` data
iris.training <- iris2[ind == 1, 1:4]
iris.test <- iris2[ind == 2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind == 1, 5]
iris.testtarget <- iris[ind == 2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)

# Initialize a sequential model
model <- keras_model_sequential()

# Add layers to the model
model %>%
    layer_dense(units = 180, activation = 'relu', input_shape = c(90)) %>%
    layer_dense(units = 6, activation = 'softmax')

# Print a summary of a model
summary(model)

# Get model configuration
get_config(model)

# Get layer configuration
get_layer(model, index = 1)

# List the model's layers
model$layers

# List the input tensors
model$inputs

# List the output tensors
model$outputs

# Compile the model
model %>% compile(
     loss = 'categorical_crossentropy',
     optimizer = 'adam',
     metrics = 'accuracy'
 )

# Fit the model 
model %>% fit(
     iris.training,
     iris.trainLabels,
     epochs = 200,
     batch_size = 5,
     validation_split = 0.2
 )

# Store the fitting history in `history` 
history <- model %>% fit(
     iris.training,
     iris.trainLabels,
     epochs = 200,
     batch_size = 5,
     validation_split = 0.2
 )

# Plot the history
plot(history)

# Plot the model loss of the training data
plot(history$metrics$loss, main = "Model Loss", xlab = "epoch", ylab = "loss", col = "blue", type = "l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col = "green")

# Add legend
legend("topright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

# Plot the accuracy of the training data 
plot(history$metrics$acc, main = "Model Accuracy", xlab = "epoch", ylab = "accuracy", col = "blue", type = "l")

# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col = "green")

# Add Legend
legend("bottomright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

# Predict the classes for the test data
classes <- model %>% predict_classes(iris.test, batch_size = 128)

# Confusion matrix
table(iris.testtarget, classes)

# Evaluate on test data and labels
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)

# Print the score
print(score)

############ LAYER EVALUATIONS

# Initialize a sequential model
model <- keras_model_sequential()

# Add layers to the model
model %>%
    layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>%
    layer_dense(units = 5, activation = 'relu') %>%
    layer_dense(units = 3, activation = 'softmax')

# Compile the model
model %>% compile(
     loss = 'categorical_crossentropy',
     optimizer = 'adam',
     metrics = 'accuracy'
 )

# Save the training history in history
history <- model %>% fit(
  iris.training, iris.trainLabels,
  epochs = 200, batch_size = 5,
  validation_split = 0.2
 )

# Plot the model loss
plot(history$metrics$loss, main = "Model Loss", xlab = "epoch", ylab = "loss", col = "blue", type = "l")
lines(history$metrics$val_loss, col = "green")
legend("topright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

# Plot the model accuracy
plot(history$metrics$acc, main = "Model Accuracy", xlab = "epoch", ylab = "accuracy", col = "blue", type = "l")
lines(history$metrics$val_acc, col = "green")
legend("bottomright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

############ HIDDEN LAYER EVALUATIONS

# Initialize the sequential model
model <- keras_model_sequential()

# Add layers to the model
model %>%
    layer_dense(units = 28, activation = 'relu', input_shape = c(4)) %>%
    layer_dense(units = 3, activation = 'softmax')

# Compile the model
model %>% compile(
     loss = 'categorical_crossentropy',
     optimizer = 'adam',
     metrics = 'accuracy'
 )

# Save the training history in the history variable
history <- model %>% fit(
  iris.training, iris.trainLabels,
  epochs = 200, batch_size = 5,
  validation_split = 0.2
 )

# Plot the model loss
plot(history$metrics$loss, main = "Model Loss", xlab = "epoch", ylab = "loss", col = "blue", type = "l")
lines(history$metrics$val_loss, col = "green")
legend("topright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

# Plot the model accuracy
plot(history$metrics$acc, main = "Model Accuracy", xlab = "epoch", ylab = "accuracy", col = "blue", type = "l")
lines(history$metrics$val_acc, col = "green")
legend("bottomright", c("train", "test"), col = c("blue", "green"), lty = c(1, 1))

####################################
# Feature Selections
####################################

######### FIND CORRELATED FEATURES

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[, 1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

######### RANK FEATURES BY IMPORTANCE

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# train the model
model <- train(diabetes ~ ., data = PimaIndiansDiabetes, method = "lvq", preProcess = "scale", trControl = control)
# estimate variable importance
importance <- varImp(model, scale = FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

######### RECURSIVE FEATURE ELIMINATION

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[, 1:8], PimaIndiansDiabetes[, 9], sizes = c(1:8), rfeControl = control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type = c("g", "o"))

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
input_data = rbind(train_dataset1_norm, test_dataset1_norm)
# define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# run the RFE algorithm
results <- rfe(input_data[, 1:ncol(input_data)-1], input_data$respon, sizes = c(1:ncol(input_data)-1), rfeControl = control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type = c("g", "o"))

######### RANDOM FOREST FEATURE SELECTIONS
# RUN MODEL FIRST!!!

importances = data.frame(importance(modelRandomForest));
kolom_train_data = colnames(train_dataset1_norm)[1:ncol(train_dataset1_norm) - 1];
importances$kolom = kolom_train_data;
importances = rbind(importances, importances[1,]);

meanMDA = mean(importances$MeanDecreaseAccuracy);
meanMDG = mean(importances$MeanDecreaseGini);

q1MDA = quantile(importances$MeanDecreaseAccuracy)[2];
q1MDG = quantile(importances$MeanDecreaseGini)[2];

q3MDA = quantile(importances$MeanDecreaseAccuracy)[4];
q3MDG = quantile(importances$MeanDecreaseGini)[4];

q80MDA = quantile(importances$MeanDecreaseAccuracy,0.8);
q80MDG = quantile(importances$MeanDecreaseGini,0.8);

q85MDA = quantile(importances$MeanDecreaseAccuracy, 0.85);
q85MDG = quantile(importances$MeanDecreaseGini, 0.85);

q90MDA = quantile(importances$MeanDecreaseAccuracy, 0.9);
q90MDG = quantile(importances$MeanDecreaseGini, 0.9);

q95MDA = quantile(importances$MeanDecreaseAccuracy, 0.95);
q95MDG = quantile(importances$MeanDecreaseGini, 0.95);

q96MDA = quantile(importances$MeanDecreaseAccuracy, 0.96);
q96MDG = quantile(importances$MeanDecreaseGini, 0.96);

q97MDA = quantile(importances$MeanDecreaseAccuracy, 0.97);
q97MDG = quantile(importances$MeanDecreaseGini, 0.97);

q98MDA = quantile(importances$MeanDecreaseAccuracy, 0.98);
q98MDG = quantile(importances$MeanDecreaseGini, 0.98);

#Spearman, Kendall, Polycholic

importances$threshMDA = 0;
importances$threshMDG = 0;

importances$threshMDA[importances$MeanDecreaseAccuracy >= meanMDA] = 1;
importances$threshMDG[importances$MeanDecreaseGini >= meanMDA] = 1;

featuresMDA = subset(importances, threshMDA == 1);
featuresMDG = subset(importances, threshMDG == 1);

featuresMDA %>% arrange(-MeanDecreaseAccuracy) %>% select(kolom, MeanDecreaseAccuracy) %>% as.data.frame();

namaMDA = featuresMDA$kolom;
namaMDG = featuresMDG$kolom;

train_dataset1_normMDA = train_dataset1_norm %>%
select(namaMDA) %>%
as.data.frame();

train_dataset1_normMDA$respon = train_dataset1_norm$respon;

train_dataset1_normMDG = train_dataset1_norm %>%
select(namaMDG) %>%
as.data.frame();

train_dataset1_normMDG$respon = train_dataset1_norm$respon;

test_dataset1_normMDA = test_dataset1_norm %>%
select(namaMDA) %>%
as.data.frame();

test_dataset1_normMDA$respon = test_dataset1_norm$respon;

test_dataset1_normMDG = test_dataset1_norm %>%
select(namaMDG) %>%
as.data.frame();

test_dataset1_normMDG$respon = test_dataset1_norm$respon;

hitungRandomForest(train_dataset1_normMDA, test_dataset1_normMDA);
