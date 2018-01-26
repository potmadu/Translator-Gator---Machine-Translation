library(pROC);
data(aSAH);

#Contoh
roc1 = roc(aSAH$outcome,
            aSAH$s100b, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            show.thres=TRUE,smooth=TRUE, col="red")

roc2 <- roc(aSAH$outcome, aSAH$wfns,plot=TRUE, add=TRUE, percent=roc1$percent, smooth=TRUE, col="blue")

##### CODE

hasil3 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_3.csv",stringsAsFactors=FALSE);
hasil5 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_5.csv",stringsAsFactors=FALSE);
hasil3_balanced = read.csv("C:/Experiments/summary_result_2301_test_balance_3.csv",stringsAsFactors=FALSE);
hasil5_bl = read.csv("C:/Experiments/summary_result_2301_test_balance_5.csv",stringsAsFactors=FALSE);

hasil_balanced_3class = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - 3 Class - Results.csv",stringsAsFactors=FALSE);

sv5_im = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv",stringsAsFactors=FALSE);

supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv",stringsAsFactors=FALSE);
supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv",stringsAsFactors=FALSE);

> str(hasil)
'data.frame':   5343 obs. of  12 variables:
 $ X                    : int  1 2 3 4 5 6 7 8 9 10 ...
 $ ID                   : int  1 2 3 4 6 8 10 11 12 13 ...
 $ respon_asli          : int  2 5 4 3 5 4 5 5 4 3 ...
 $ baseline             : int  5 5 5 5 5 5 5 5 5 5 ...
 $ LDA.full             : int  5 5 5 1 4 5 5 5 5 5 ...
 $ LDA.feature.selection: int  5 5 5 5 5 5 5 5 5 5 ...
 $ LDA.penalized        : int  5 5 5 3 4 5 5 5 5 5 ...
 $ LDA.PCA.logistics    : int  5 5 5 5 5 5 5 5 5 5 ...
 $ Log.penalized        : int  5 5 5 5 5 5 5 5 5 5 ...
 $ LDA.RF               : int  5 5 5 5 4 5 5 5 5 3 ...
 $ Log.RF               : int  5 5 5 5 5 5 5 5 5 5 ...
 $ LDA.PCA              : int  5 5 5 1 5 5 5 5 5 5 ...

hitungAUC_5class(hasil5_bl$respon_asli,hasil5_bl$baseline);

hitungAUC_5class(sv5_im$test_data.respon,sv5_im$predRF);

hasil = supervised_balanced_5;
aktual = hasil$respon_asli;
prediksi = hasil$baseline;

hasil = supervised_balanced_5;
aktual = hasil$test_data.respon;
prediksi = hasil$predSVM;

hitungAUC_5class = function(aktual, prediksi) {

    library(pROC);

    df = data.frame(aktual, as.numeric(prediksi));
    colnames(df) = c("aktual","prediksi");

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

    hasil_auc = list("auc12"=auc12,"auc23"=auc23,"auc34"=auc34,"auc45"=auc45,"rataAUC"=rataAUC);

    return(hasil_auc);

}

plotAUC_5class = function(aktual, prediksi) {

    library(pROC);

    df = data.frame(aktual, as.numeric(prediksi));
    colnames(df) = c("aktual","prediksi");

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

    hasil_auc = list("auc12"=auc12,"auc23"=auc23,"auc34"=auc34,"auc45"=auc45,"rataAUC"=rataAUC);

	roc12 = roc(df12$aktual,
            df12$prediksi, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            show.thres=TRUE)

	roc23 = roc(df23$aktual, df23$prediksi,plot=TRUE, add=TRUE, percent=roc12$percent)
	roc34 = roc(df34$aktual, df34$prediksi,plot=TRUE, add=TRUE, percent=roc12$percent)
	roc45 = roc(df45$aktual, df45$prediksi,plot=TRUE, add=TRUE, percent=roc12$percent)

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

roc1 <- roc(df45$aktual,
            df45$prediksi, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE, smooth=TRUE)

roc12 = roc(df12$aktual, df12$prediksi, plot=TRUE, grid=TRUE, percent=roc12$percent)
roc23 = roc(df23$aktual, df23$prediksi, plot=TRUE, add=TRUE, percent=roc12$percent)
roc34 = roc(df34$aktual, df34$prediksi, plot=TRUE, add=TRUE, percent=roc12$percent)
roc45 = roc(df45$aktual, df45$prediksi, plot=TRUE, add=TRUE, percent=roc12$percent)

##############

set.seed(12345);

supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv",stringsAsFactors=FALSE);

x1 = factor(supervised_balanced_5$test_data.respon);
x2 = factor(supervised_balanced_5$predRF);

n = length(x1);
Brep = 2000;
xy = data.frame(cbind(x1,x2));

polychor(x1,x2, ML = TRUE, std.err = TRUE, maxcor = .996);

###############
# Multiplot 

library(ggplot2)

# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
    geom_density() +
    ggtitle("Final weight, by diet")

# Fourth plot
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(Diet ~ .) +
    ggtitle("Final weight, by diet") +
    theme(legend.position="none")        # No legend (redundant in this graph)    

##########################

supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv", stringsAsFactors = FALSE);
supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - 3 Class - Results.csv", stringsAsFactors = FALSE);

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

supervised_balanced_5 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - Results.csv", stringsAsFactors = FALSE);
supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - 3 Class - Results.csv", stringsAsFactors = FALSE);

regression_balanced_5 = read.csv("C:/Experiments/summary_result_2301_test_balance_5.csv",stringsAsFactors=FALSE);
regression_balanced_3 = read.csv("C:/Experiments/summary_result_2301_test_balance_3.csv",stringsAsFactors=FALSE);

supervised_imbalanced_5 = read.csv("C:/Experiments/Training Data 2301 - Drop Column.csv",stringsAsFactors=FALSE);
regression_imbalanced_5 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_5.csv",stringsAsFactors=FALSE);

hasil = regression_imbalanced_5;
aktual = hasil$respon_asli;
prediksi = hasil$LDA.RF;

df = data.frame(aktual,prediksi);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);
df34 = subset(df, aktual == 3 | aktual == 4);
df45 = subset(df, aktual == 4 | aktual == 5);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;
df34$prediksi[df34$prediksi != 3] = 4;
df45$prediksi[df45$prediksi != 4] = 5;

plot.roc(df12$aktual, df12$prediksi,col="red");
plot.roc(df23$aktual, df23$prediksi,add=TRUE,col="blue");
plot.roc(df34$aktual, df34$prediksi,add=TRUE,col="orange");
plot.roc(df45$aktual, df45$prediksi,add=TRUE,col="green");
legend("bottomright",title=c("   ROC Curve   "),legend=c("Category 1,2","Category 2,3","Category 3,4","Category 4,5"),col=c("red","blue","orange","green"),lwd=2);

############

supervised_balanced_3 = read.csv("C:/Experiments/Bootstrap Training Data 2301 - Drop Column - 3 Class - Results.csv", stringsAsFactors = FALSE);
regression_balanced_3 = read.csv("C:/Experiments/summary_result_2301_test_balance_3.csv",stringsAsFactors=FALSE);

supervised_imbalanced_3 = read.csv("C:/Experiments/Training Data 2301 - Drop Column - 3 Class.csv", stringsAsFactors = FALSE);
regression_imbalanced_3 = read.csv("C:/Experiments/summary_result_2301_test_imbalance_3.csv",stringsAsFactors=FALSE);

hasil = supervised_imbalanced_3;
aktual = hasil$test_data.respon;
prediksi = hasil$predNB;

df = data.frame(aktual,prediksi);
colnames(df) = c("aktual", "prediksi");

df12 = subset(df, aktual == 1 | aktual == 2);
df23 = subset(df, aktual == 2 | aktual == 3);

df12$prediksi[df12$prediksi != 1] = 2;
df23$prediksi[df23$prediksi != 2] = 3;

plot.roc(df12$aktual, df12$prediksi,col="red");
plot.roc(df23$aktual, df23$prediksi,add=TRUE,col="blue");
legend("bottomright",title=c("   ROC Curve   "),legend=c("Category 1,2","Category 2,3"),col=c("red","blue"),lwd=2);


############

roc12 = roc(df12$aktual, df12$prediksi, plot = TRUE, grid = TRUE, percent = roc12$percent, auc.polygon=TRUE, col="red", print.auc=TRUE);
roc23 = roc(df23$aktual, df23$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent, auc.polygon=TRUE, col="blue", print.auc=TRUE);
roc34 = roc(df34$aktual, df34$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent, auc.polygon=TRUE, col="green", print.auc=TRUE);
roc45 = roc(df45$aktual, df45$prediksi, plot = TRUE, add = TRUE, percent = roc12$percent, auc.polygon=TRUE, col="yellow", print.auc=TRUE);

plot.roc(df12$aktual, df12$prediksi);

