##Below is the script for manual feature selection based on the measure collinearity.
##The cutoff is 0.75

train = read.csv("/Users/rajius/Documents/Translator Gator project/Data/Training Data 1601 - Drop Column.csv"
                 , header = TRUE, sep = ",",)
test = read.csv("/Users/rajius/Documents/Translator Gator project/Data/Test Data 1601 - Drop Column.csv"
                , header = TRUE, sep = ",",)
train$total.vote = train$translation_total_vote_up + train$translation_total_vote_down
train$manual_assessment = factor(train$manual_assessment)
test$total.vote = test$translation_total_vote_up + test$translation_total_vote_down
test$manual_assessment = factor(test$manual_assessment)

train$age_15 = train$user_is_age_15 * 2
train$age_20 = train$user_is_age_20 * 3
train$age_25 = train$user_is_age_25 * 4
train$age_30 = train$user_is_age_30 * 5
train$age_35 = train$user_is_age_35 * 6
train$age_40 = train$user_is_age_40 * 7
train$age.group = (train$user_is_age_0 + train$age_15 + train$age_20
                   + train$age_25 + train$age_30+train$age_35
                   + train$age_40)
train <- train[c(-7:-14,-99:-104)]
myvars <- names(train) %in% c("id","manual_origin_source","manual_assessment","target.word",
                              "username","user_language","source.word")
train.penalty = train[!myvars]

test$age_15 = test$user_is_age_15 * 2
test$age_20 = test$user_is_age_20 * 3
test$age_25 = test$user_is_age_25 * 4
test$age_30 = test$user_is_age_30 * 5
test$age_35 = test$user_is_age_35 * 6
test$age_40 = test$user_is_age_40 * 7
test$age.group = (test$user_is_age_0 + test$age_15 + test$age_20
                  + test$age_25 + test$age_30 + test$age_35
                  + test$age_40)
test <- test[c(-7:-14,-99:-104)]
myvars <- names(test) %in% c("id","manual_origin_source","manual_assessment","target.word",
                             "username","user_language","source.word")
test.penalty = test[!myvars]


##Feature selection
correlations = cor(na.omit(train.penalty))
require(caret)
highCorr = findCorrelation(correlations, cutoff = .75)
length(highCorr)
#55
highCorr
filtered.train = train.penalty[, -highCorr]
filtered.test = test.penalty[, -highCorr]

train.new = data.frame(train$manual_assessment,filtered.train)
names(train.new)[names(train.new)=="train.manual_assessment"] <- "manual_assessment"
train.new = na.omit(train.new)
test.new = data.frame(test$manual_assessment,filtered.test)
names(test.new)[names(test.new)=="test.manual_assessment"] <- "manual_assessment"
test.new = na.omit(test.new)

