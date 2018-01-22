getFI_RandomForest = function(train_data) {

    set.seed(12345);

    library(caret);
    library(randomForest);
    library(e1071);

    train_data$respon = as.factor(train_data$respon);

    model = randomForest(respon ~ ., data = train_data, importance = TRUE);

    importances = data.frame(importance(model));
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

    output = list("MDA" = importancesMDA, "MDG" = importancesMDG);

    return(output);

}
