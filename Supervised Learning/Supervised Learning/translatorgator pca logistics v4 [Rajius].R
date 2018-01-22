##Below is the script for PCA application in training and test data set of logistic modelling
##The use of PCA should be carried out only in training data as the following:
##1. Produce principal components via PCA method from the training test
##2. Use components in the model (in this case ordinal logistic)
##3. Predict the components in the test set
##4. Use the predicted components in the test set to the model


train = read.csv("/Users/rajius/Documents/Translator Gator project/Data/Training Data 1601 - Drop Column.csv"
                 , header = TRUE, sep = ",",)
test = read.csv("/Users/rajius/Documents/Translator Gator project/Data/Test Data 1601 - Drop Column.csv"
                , header = TRUE, sep = ",",)
train$manual_assessment = factor(train$manual_assessment)
test$manual_assessment = factor(test$manual_assessment)

#PCA for numerical variables
myvars <- names(train) %in% c("id","manual_origin_source","manual_assessment","target.word",
                              "username","user_language","source.word")
train.nums = train[!myvars]
pr<-prcomp(na.omit(train.nums), scale = TRUE)
plot(pr, type="l" )  #only for the first 10 components shown
std_dev <- pr$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

numvar = 0
predrate = 0

for (i in 2:40) {
numvar[i] = i
pred.train = predict(pr, train)[,1:i]
train.new = data.frame(train$manual_assessment,pred.train)
names(train.new)[names(train.new)=="train.manual_assessment"] <- "manual_assessment"

##Ordinal logistic regression
(lrm.fit.pca=lrm(manual_assessment ~ ., data = na.omit(train.new)))
pred.test = predict(pr, newdata = (test))
pred.test = as.data.frame(pred.test)
pred.test = pred.test[,1:i]

test.new = data.frame(test$manual_assessment,pred.test)
names(test.new)[names(test.new)=="test.manual_assessment"] <- "manual_assessment"
lrm.fit.pca.predict = predict(lrm.fit.pca, test.new, type="fitted.ind")
predictHat.pca = levels(test.new$manual_assessment)[max.col(lrm.fit.pca.predict)]
cTab = table(test.new$manual_assessment, predictHat.pca, 
             dnn=c("Original score", "Predicted score"))
addmargins(cTab)
manual_assessment = as.numeric(test$manual_assessment)
predictHat.pca = as.numeric(predictHat.pca)
#mean(test$manual_assessment==predictHat.pca, na.rm=TRUE)
predrate[i] = mean(manual_assessment==predictHat.pca, na.rm=TRUE)
}

plot(numvar, predrate, xlim=c(1,40), ylim=c(0.75,0.76), xlab="Number of components", 
  ylab="Accuracy")
#component 20 looks the best
predrate[20]
#0.757
