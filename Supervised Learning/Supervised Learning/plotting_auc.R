supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv", stringsAsFactors = FALSE);
supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv", stringsAsFactors = FALSE);

hasil = supervised_balanced_5;
aktual = hasil$respon_asli;
prediksi = hasil$baseline;

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

    rataAUC = mean(auc12, auc23, auc34, auc45);

    hasil_auc = list("auc12" = auc12, "auc23" = auc23, "auc34" = auc34, "auc45" = auc45, "rataAUC" = rataAUC);

    return(hasil_auc);

}

plotAUC_5class = function(aktual, prediksi) {

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

    rataAUC = mean(auc12, auc23, auc34, auc45);

    hasil_auc = list("auc12" = auc12, "auc23" = auc23, "auc34" = auc34, "auc45" = auc45, "rataAUC" = rataAUC);

    roc12 = roc(df12$aktual,
            df12$prediksi, percent = TRUE,
    # arguments for auc
            partial.auc = c(100, 90), partial.auc.correct = TRUE,
            partial.auc.focus = "sens",
    # arguments for ci
            ci = TRUE, boot.n = 100, ci.alpha = 0.9, stratified = FALSE,
    # arguments for plot
            plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE,
            show.thres = TRUE)

    roc23 = roc(df23$aktual, df23$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent)
    roc34 = roc(df34$aktual, df34$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent)
    roc45 = roc(df45$aktual, df45$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent)

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
    rataAUC = mean(auc12, auc23);

    hasil_auc = list("auc12" = auc12, "auc23" = auc23, "rataAUC" = rataAUC);

    return(hasil_auc);

}

#plot pertama parameternya grid=true

hasil = supervised_balanced_5;
aktual = hasil$test_data.respon;
prediksi = hasil$predSVM;

df = data.frame(hasil$test_data.respon, hasil$predC50);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

roc12 = roc(df12$aktual, df12$prediksi, plot = TRUE, grid = TRUE, percent = roc12$percent);
roc23 = roc(df23$aktual, df23$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);
roc34 = roc(df34$aktual, df34$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);
roc45 = roc(df45$aktual, df45$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);

#####################

devtools::install_github("hadley/ggplot2");
devtools::install_github("sachsmc/plotROC");

library(plotROC);
library(ggplot2);

set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1],
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)

longtest <- melt_roc(test, "D", c("M1", "M2"))
head(longtest)

ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc();

#########################

library(plotROC);
library(ggplot2);

hasil = supervised_balanced_5;
aktual = hasil$test_data.respon;
prediksi = hasil$predSVM;

df = data.frame(hasil$test_data.respon, hasil$predRF);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

output12 = df12;
output23 = df23;
output34 = df34;
output45 = df45;

df = data.frame(hasil$test_data.respon, hasil$predNB);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

output12$predNB = df12$prediksi;
output23$predNB = df23$prediksi;
output34$predNB = df34$prediksi;
output45$predNB = df45$prediksi;

colnames(df) = c("aktual", "predRF");

df = data.frame(hasil$test_data.respon, hasil$predC50);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

output12$predC50 = df12$prediksi;
output23$predC50 = df23$prediksi;
output34$predC50 = df34$prediksi;
output45$predC50 = df45$prediksi;

df = data.frame(hasil$test_data.respon, hasil$predSVM);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

output12$predSVM = df12$prediksi;
output23$predSVM = df23$prediksi;
output34$predSVM = df34$prediksi;
output45$predSVM = df45$prediksi;

basicplot = ggplot(df12, aes(d = aktual, m = prediksi)) + geom_roc();
basicplot;

basicplot2 = ggplot(df23, aes(d = aktual, m = predNB)) + geom_roc();
basicplot2;

longtest = melt_roc(output12, "aktual", c("prediksi", "predNB", "predC50", "predSVM"));
head(longtest);

ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc();

###################

library(pROC);
library(ggplot2);

hasil = supervised_balanced_5;
aktual = hasil$test_data.respon;
prediksi = hasil$predSVM;

df = data.frame(hasil$test_data.respon, hasil$predSVM);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

roc12 = roc(df12$aktual, df12$prediksi, plot = TRUE, grid = TRUE, percent = roc12$percent);
roc23 = roc(df23$aktual, df23$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);
roc34 = roc(df34$aktual, df34$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);
roc45 = roc(df45$aktual, df45$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent);

###################

supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - 3 Class - Results.csv", stringsAsFactors = FALSE);
regression_balanced_3 = read.csv("C:/Experiments/summary_result_2301_test_balance_3.csv", stringsAsFactors = FALSE);
supervised_imbalanced_3 = read.csv("C:/Experiments/Training Data 2301 - Drop Column - 3 Class.csv", stringsAsFactors = FALSE);
regression_imbalanced_3 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_3.csv", stringsAsFactors = FALSE);

supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv", stringsAsFactors = FALSE);
regression_balanced_5 = read.csv("C:/Experiments/summary_result_2301_test_balance_5.csv", stringsAsFactors = FALSE);
supervised_imbalanced_5 = read.csv("C:/Experiments/Training Data 2301 - Drop Column.csv", stringsAsFactors = FALSE);
regression_imbalanced_5 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_5.csv", stringsAsFactors = FALSE);

hasil3 = regression_imbalanced_3;
hasil5 = regression_imbalanced_5;
aktual3 = hasil3$respon_asli;
aktual5 = hasil5$respon_asli;
prediksi3 = hasil3$baseline;
prediksi5 = hasil5$baseline;

df3 = data.frame(aktual3, prediksi3);
colnames(df3) = c("aktual", "prediksi");
df5 = data.frame(aktual5, prediksi5);
colnames(df5) = c("aktual", "prediksi");

df12_3 = subset(df3, aktual == 1 | aktual == 2);
df23_3 = subset(df3, aktual == 2 | aktual == 3);
df12_3$prediksi[df12_3$prediksi != 1] = 2;
df23_3$prediksi[df23_3$prediksi != 2] = 3;

df12_5 = subset(df5, aktual == 1 | aktual == 2);
df23_5 = subset(df5, aktual == 2 | aktual == 3);
df34_5 = subset(df5, aktual == 3 | aktual == 4);
df45_5 = subset(df5, aktual == 4 | aktual == 5);
df12_5$prediksi[df12_5$prediksi != 1] = 2;
df23_5$prediksi[df23_5$prediksi != 2] = 3;
df34_5$prediksi[df34_5$prediksi != 3] = 4;
df45_5$prediksi[df45_5$prediksi != 4] = 5;

plot.roc(df12_3$aktual, df12_3$prediksi, col = "#550000");
plot.roc(df23_3$aktual, df23_3$prediksi, add = TRUE, col = "#801515");
plot.roc(df12_5$aktual, df12_5$prediksi, add = TRUE, col = "#919106");
plot.roc(df23_5$aktual, df23_5$prediksi, add = TRUE, col = "#9D9D1E");
plot.roc(df34_5$aktual, df34_5$prediksi, add = TRUE, col = "#B7B757");
plot.roc(df45_5$aktual, df45_5$prediksi, add = TRUE, col = "#C3C37A");
legend("bottomright", title = c("   ROC Curve   "),
legend = c("3 Categories - 1,2", "3 Categories - 2,3", "5 Categories - 1,2", "5 Categories - 2,3", "5 Categories - 3,4", "5 Categories - 4,5"),
col = c("#550000", "#801515", "#919106", "#9D9D1E", "#B7B757", "#C3C37A"),
lwd = 2);

