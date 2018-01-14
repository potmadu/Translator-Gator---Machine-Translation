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

#CTREE

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

library(nnet);

seeds = read.csv("seeds.csv", header = T);

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

