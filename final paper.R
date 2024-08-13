data<-read.csv("C:/Users/Xu Siyu/Desktop/Liver_disease_data.csv")
View(data)


head(data)
summary(data)
str(data)
sum(is.na(data))


library(tidyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(pROC)
data <- as.data.frame(data)

###### Pearson
target_variable <- data[[ncol(data)]]
features <- data[, -ncol(data)]

cor_values <- sapply(features, function(x) cor(x, target_variable, use = "complete.obs", method = "pearson"))

top_features <- sort(abs(cor_values), decreasing = TRUE)

print(top_features)
print(names(top_features))

# visulisation
top_feature_names <- names(top_features)
selected_data <- data[, c(top_feature_names, names(data)[ncol(data)])]
cor_matrix <- cor(selected_data, use = "complete.obs")

corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)



##### boxplot
data_long <- pivot_longer(data, cols = c(Age,Gender,BMI,AlcoholConsumption,Smoking
                                         ,GeneticRisk,PhysicalActivity,Diabetes,Hypertension
                                         ,LiverFunctionTest,Diagnosis), names_to = "Variable", values_to = "Value")
p1 <- ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.colour = "red", colour = "red",outlier.shape = 1) +
  facet_wrap(~ Variable, scales = "free") + # 使用facets来为每个变量创建单独的图表
  theme_minimal() + # 使用简约主题
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), strip.background = element_blank()) + # 移除不必要的标签和背景
  labs(title = "Boxplots of Variables", y = "Values")
print(p1)


# qq plot
p2 <- ggplot(data_long, aes(sample = Value)) +
  stat_qq(colour = "green") +
  stat_qq_line(colour = "red") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Q-Q Plots for Various Variables", x = "Theoretical Quantiles", y = "Sample Quantiles")
print(p2)



sum(data$Diagnosis==0)
sum(data$Diagnosis==1)

# Diagnosis distribution
ggplot(data, aes(x = factor(Diagnosis))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Liver Disease Diagnosis",
       x = "Diagnosis (0: No Disease, 1: Disease)",
       y = "Count")


# age distribution
ggplot(data, aes(x = Age, fill = factor(Diagnosis))) +
  geom_histogram(bins = 20, position = "dodge") +
  labs(title = "Age Distribution by Diagnosis",
       x = "Age",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("No Disease", "Disease"))


# BMI and GeneticRisk
ggplot(data, aes(x = Smoking , y = GeneticRisk , color = factor(Diagnosis))) +
  geom_point() +
  labs(title = "Smoking  vs GeneticRisk  by Diagnosis",
       x = "Smoking ",
       y = "GeneticRisk ") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("No Disease", "Disease"))

ggplot(data, aes(x = GeneticRisk, fill = factor(Diagnosis))) +
  geom_histogram(bins = 20, position = "dodge") +
  labs(title = "GeneticRisk Distribution by Diagnosis",
       x = "GeneticRisk",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("No Disease", "Disease"))
##########################################################

### 
model1 <- glm(Diagnosis ~ Age+BMI, 
              family = binomial(link = "logit"), data = data)
summary(model1)

roc_response1 <- roc(data$Diagnosis, fitted(model1))
plot(roc_response1)
auc(roc_response1)
auc1 <- auc(roc_response1)

### lifestle
model2 <- glm(Diagnosis ~ AlcoholConsumption + Smoking + PhysicalActivity, 
             family = binomial(link = "logit"), data = data)
summary(model2)

roc_response2 <- roc(data$Diagnosis, fitted(model2))
plot(roc_response2)
auc(roc_response2)
auc2 <- auc(roc_response2)

### 
model3 <- glm(Diagnosis ~ GeneticRisk+Diabetes+Hypertension,
             family = binomial(link = "logit"), data = data)
summary(model3)

roc_response3 <- roc(data$Diagnosis, fitted(model3))
plot(roc_response3)
auc(roc_response3)
auc3 <- auc(roc_response3)


plot(roc_response1, col = "blue", lwd = 2, lty = 1, main = "ROC Curves for Different Models", 
     xlab = "1 - Specificity", ylab = "Sensitivity")

lines(roc_response2, col = "green", lwd = 2, lty = 2)
lines(roc_response3, col = "red", lwd = 2, lty = 3)

grid()

# 
legend("bottomright", 
       legend = c(paste("Demographic Info (AUC =", round(auc1, 2), ")"),
                  paste("Lifestyle (AUC =", round(auc2, 2), ")"),
                  paste("Health Indicators (AUC =", round(auc3, 2), ")")),
       col = c("blue", "green", "red"), 
       lty = c(1, 2, 3), lwd = 2, cex = 0.8, box.lty = 0)



#########################################################




#########################################################


data<-read.csv("C:/Users/Xu Siyu/Desktop/Liver_disease_data.csv")

################# missing value
remove_ratio <- 0.1

total_elements <- prod(dim(data))
num_remove <- floor(remove_ratio * total_elements)
na_matrix <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))

set.seed(123) 
na_indices <- sample(total_elements, num_remove)

na_matrix[na_indices] <- TRUE
data[na_matrix] <- NA



library(VIM)

aggr(data, col = c('navyblue', 'yellow'), numbers = TRUE, sortVars = TRUE, 
     labels = names(data), cex.axis = 0.7, gap = 3, 
     ylab = c("Missing data", "Pattern"))

#######################data imputation
library(ggplot2)
library(dplyr)
library(cowplot)

#### simple
value_imputed <- data.frame(
   original = data$Age,
   imputed_zero = replace(data$Age, is.na(data$Age), 0),
   imputed_mean = replace(data$Age, is.na(data$Age), mean(data$Age, na.rm = TRUE)),
   imputed_median = replace(data$Age, is.na(data$Age), median(data$Age, na.rm = TRUE))
)

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


#### multiple
library(mice)

#linear regression
imputed_data_np <- mice(data, m = 5, method = 'norm.predict', maxit = 50, seed = 500)
imputed_data_np <- complete(imputed_data_np, 1)


#pmm
imputed_data_pmm <- mice(data, m = 5, method = 'pmm', maxit = 50, seed = 500)
imputed_data_pmm <- complete(imputed_data_pmm, 1)


#rf
imputed_data_rf <- mice(data, m = 5, method = 'rf', maxit = 50, seed = 500)
imputed_data_rf <- complete(imputed_data_rf, 1)


#gassian
imputed_data_norm <- mice(data, m = 5, method = 'norm', maxit = 50, seed = 500)
imputed_data_norm <- complete(imputed_data_norm, 1)


# histogram(age)
ggplot() +
  geom_histogram(data = imputed_data_np, aes(x = Age), fill = 'red', alpha = 0.5, binwidth = 1) +
  geom_histogram(data = imputed_data_pmm, aes(x = Age), fill = 'blue', alpha = 0.5, binwidth = 1) +
  geom_histogram(data = imputed_data_rf, aes(x = Age), fill = 'green', alpha = 0.5, binwidth = 1) +
  geom_histogram(data = imputed_data_norm, aes(x = Age), fill = 'purple', alpha = 0.5, binwidth = 1) +
  labs(title = "Comparison of Age Distribution After Imputation", x = "Age", y = "Frequency") +
  scale_fill_manual(name = "Imputation Method", values = c("NP" = "red", "PMM" = "blue","RF" = "green","Norm" = "purple"))



#### KNN
imputed_data_KNN <- kNN(data, k = 5, imp_var = FALSE)
View(imputed_data_KNN)


ggplot() +
  geom_histogram(data = data, aes(x = Age), fill = 'red', alpha = 0.5, binwidth = 1) +
  geom_histogram(data = imputed_data_KNN, aes(x = Age), fill = 'blue', alpha = 0.5, binwidth = 1) +
  labs(title = "Comparison of Age Distribution Before and After Imputation", x = "Age", y = "Frequency") +
  scale_fill_manual(name = "Dataset", values = c("Before" = "red", "After" = "blue"))


data<-imputed_data_KNN

###### prepropossing
library(caret)

data$Gender <- as.integer(data$Gender)
data$Smoking <- as.integer(data$Smoking)
data$GeneticRisk <- as.integer(data$GeneticRisk)
data$Diabetes <- as.integer(data$Diabetes)
data$Hypertension <- as.integer(data$Hypertension)
data$Diagnosis <- as.integer(data$Diagnosis)



data$Diagnosis=factor(data$Diagnosis)
data$Gender=factor(data$Gender)
data$Smoking=factor(data$Smoking)
data$GeneticRisk=factor(data$GeneticRisk)
data$Diabetes=factor(data$Diabetes)
data$Hypertension=factor(data$Hypertension)
#mydata<-data
#levels(mydata$Diagnosis) <- c("Class0", "Class1")
#levels(mydata$Diagnosis)



#### train-test split
set.seed(123)
index <- createDataPartition(data$Diagnosis, p = 0.7, list = TRUE)
train_data <- data[index[[1]], ]
test_data <- data[-index[[1]], ]


View(data)
View(train_data)
View(test_data)




######################################################### KNN

library(kknn)
library(pROC)

start_time <- Sys.time()

model_knn <- kknn(Diagnosis ~ ., train_data, test_data, k = 7, distance = 2)
pre_knn <- fitted(model_knn)

end_time <- Sys.time()

confusion_matrix <- table(Actual = test_data$Diagnosis, Predicted = pre_knn)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * (precision * recall) / (precision + recall)

class_counts <- rowSums(confusion_matrix)
weighted_precision <- sum(precision * class_counts) / sum(class_counts)
weighted_recall <- sum(recall * class_counts) / sum(class_counts)
weighted_f1 <- sum(f1 * class_counts) / sum(class_counts)

roc_obj <- roc(as.numeric(test_data$Diagnosis), as.numeric(pre_knn))
auc <- auc(roc_obj)

mse <- mean((as.numeric(pre_knn) - as.numeric(test_data$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(weighted_precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(weighted_recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(weighted_f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


# 
roc_obj <- roc(as.numeric(test_data$Diagnosis), as.numeric(pre_knn))

roc_df <- data.frame(
  TPR = roc_obj$sensitivities, 
  FPR = 1 - roc_obj$specificities,
  Threshold = roc_obj$thresholds
)

auc_value <- auc(roc_obj)

ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  ggtitle("ROC Curve of KNN") +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  theme_minimal() +
  annotate("text", x = 0.5, y = 0.25, label = paste("AUC =", round(auc_value, 2)), size = 5, color = "black")


# confusion matrix
conf_matrix <- as.data.frame(as.table(confusion_matrix))
ggplot(conf_matrix, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of KNN") +
  theme_minimal()




############## cross validation
library(caret)
set.seed(123)

train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

start_time <- Sys.time()

model_knn1 <- train(Diagnosis ~ .,
                    data = mydata, method = "knn",
                    trControl = train_control,
                    preProcess = "scale",
                    tuneLength = 10,
                    metric = "Accuracy") 

end_time <- Sys.time()

print(model_knn1)
print(model_knn1$results)

predictions <- predict(model_knn1, mydata)
pred_probs <- predict(model_knn1, mydata, type = "prob")

conf_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(mydata$Diagnosis, pred_probs[, 2])
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


######################################################### 




######################################################### GLM

library(pROC)

start_time <- Sys.time()

model_glm <- glm(Diagnosis ~ ., data = train_data, family = binomial())

end_time <- Sys.time()
plot(model_glm)

par(mfrow = c(2, 2))
plot(model_glm)
par(mfrow = c(1, 1))

probabilities <- predict(model_glm, test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$Diagnosis)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1 <- 2 * (precision * recall) / (precision + recall)

class_counts <- rowSums(confusion_matrix)
weighted_precision <- sum(precision * class_counts) / sum(class_counts)
weighted_recall <- sum(recall * class_counts) / sum(class_counts)
weighted_f1 <- sum(f1 * class_counts) / sum(class_counts)

roc_obj <- roc(test_data$Diagnosis, probabilities)
auc <- auc(roc_obj)

mse <- mean((as.numeric(predicted_classes) - as.numeric(test_data$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(weighted_precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(weighted_recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(weighted_f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


# confusion matrix
conf_matrix <- as.data.frame(as.table(confusion_matrix))
ggplot(conf_matrix, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of GLM") +
  theme_minimal()

# 
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# ROC curve
ggplot(data = roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  ggtitle(paste("ROC Curve (AUC =", round(auc(roc_obj), 2), ")")) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  theme_minimal()

##############  cross validation

library(caret)
library(pROC)
library(dplyr)

train_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE, 
  summaryFunction = twoClassSummary  
)
set.seed(123)

start_time <- Sys.time()

model_glm1 <- train(
  Diagnosis ~ .,
  data = mydata,
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "Accuracy",
  preProcess = "scale"
)

end_time <- Sys.time()

print(model_glm1)
print(model_glm1$results)

predictions <- predict(model_glm1, mydata)
pred_probs <- predict(model_glm1, mydata, type = "prob")

conf_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(mydata$Diagnosis, pred_probs[, 2])
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


######################################################### 




######################################################### rf

library(randomForest)
library(pROC)
library(caret)

best_mtry <- tuneRF(train_data[, -ncol(train_data)], train_data$Diagnosis, stepFactor = 1.5, improve = 0.01)
library(ggplot2)

mtry_results <- data.frame(
  mtry = c(2, 3, 4),
  OOB_error = c(12.93, 12.26, 13.43)
)

ggplot(mtry_results, aes(x = mtry, y = OOB_error)) +
  geom_line() +
  geom_point() +
  labs(title = "OOB Error Rate of mtry",
       x = "mtry",
       y = "OOB Error Rate (%)") +
  theme_minimal()

start_time <- Sys.time()

model_rf <- randomForest(Diagnosis ~ ., data = train_data, ntree = 500, mtry = 10)

end_time <- Sys.time()

predictions <- predict(model_rf, newdata = test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']

precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

probabilities <- predict(model_rf, newdata = test_data, type = "prob")[,2]
roc_obj <- roc(as.numeric(test_data$Diagnosis), probabilities)
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(test_data$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))

# 
plot(model_rf, main = "Error Rate vs Number of Trees")



#confusion matrix
conf_matrix <- as.data.frame(as.table(conf_matrix))
ggplot(conf_matrix, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of RF") +
  theme_minimal()

# 
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

#ROC curve
ggplot(data = roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc(roc_obj), 2), ")"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

##############  cross validation

library(caret)
library(pROC)

set.seed(123)

train_control <- trainControl(
  method = "cv",      
  number = 10,      
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary   
)

start_time <- Sys.time()

model_rf1 <- train(
  Diagnosis ~ .,
  data = mydata,
  method = "rf",
  trControl = train_control,
  metric = "Accuracy", 
  tuneGrid = expand.grid(.mtry = c(2, 3, 4, 5)) 
)

end_time <- Sys.time()

print(model_rf1)
print(model_rf1$results)

predictions <- predict(model_rf1, mydata)
pred_probs <- predict(model_rf1, mydata, type = "prob")

conf_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(mydata$Diagnosis, pred_probs[, 2])
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


######################################################### 






######################################################### GBM

library(gbm)
library(caret)
library(pROC)

set.seed(123)

start_time <- Sys.time()

train_control <- trainControl(method = "cv", number = 5)
model_gbm <- train(
  Diagnosis ~ .,
  data = train_data,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  metric = "Accuracy",
  tuneGrid = expand.grid(.n.trees = (1:5) * 50, .interaction.depth = 1:5, .shrinkage = 0.1, .n.minobsinnode = 10)
)


end_time <- Sys.time()

predictions <- predict(model_gbm, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']

precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

probabilities <- predict(model_gbm, test_data, type = "prob")[,2]
roc_obj <- roc(as.numeric(test_data$Diagnosis), probabilities)
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(test_data$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))



conf_matrix <- as.data.frame(as.table(confusion_matrix))


# 混淆矩阵热图
ggplot(conf_matrix, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of GB") +
  theme_minimal()

# 
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# ROC curve
ggplot(data = roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc, 2), ")"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()


##############  cross validation
library(caret)
library(pROC)
library(gbm)

set.seed(123)
train_control <- trainControl(
  method = "repeatedcv",  
  number = 10,            
  repeats = 5,            
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

start_time <- Sys.time()

model_gbm1 <- train(
  Diagnosis ~ .,
  data = mydata,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  metric = "ROC",
  tuneGrid = expand.grid(
    .interaction.depth = c(1, 3, 5),
    .n.trees = c(50, 100, 150),
    .shrinkage = c(0.01, 0.1),
    .n.minobsinnode = 10
  )
)

end_time <- Sys.time()

print(model_gbm1)
print(model_gbm1$results)
plot(model_gbm1)

predictions <- predict(model_gbm1, newdata = mydata)
pred_probs <- predict(model_gbm1, newdata = mydata, type = "prob")

conf_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(mydata$Diagnosis, pred_probs[, 2])
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))




######################################################### 






######################################################### SVM

library(e1071)
library(caret)
library(pROC)
library(MASS)
start_time <- Sys.time()

model_svm <- svm(Diagnosis ~ ., data = train_data, type = "C-classification", kernel = "radial")

end_time <- Sys.time()

train_data_numeric <- train_data
train_data_numeric[] <- lapply(train_data_numeric, function(x) if(is.factor(x)) as.numeric(x) else x)

# PCA dimension reduction
pca <- prcomp(train_data_numeric[, -which(names(train_data_numeric) == "Diagnosis")], scale. = TRUE)
train_data_pca <- data.frame(pca$x[, 1:2], Diagnosis = train_data$Diagnosis)


test_data_numeric <- test_data
test_data_numeric[] <- lapply(test_data_numeric, function(x) if(is.factor(x)) as.numeric(x) else x)

# 
pca1 <- prcomp(test_data_numeric[, -which(names(test_data_numeric) == "Diagnosis")], scale. = TRUE)
test_data_pca <- data.frame(pca1$x[, 1:2], Diagnosis = test_data$Diagnosis)


train_data<-train_data_pca
test_data<-test_data_pca

#
model_svm_pca <- svm(Diagnosis ~ ., data = train_data_pca, kernel = "linear", cost = 10)
model_svm<-model_svm_pca
# 
x1_range <- seq(min(train_data_pca$PC1), max(train_data_pca$PC1), length.out = 100)
x2_range <- seq(min(train_data_pca$PC2), max(train_data_pca$PC2), length.out = 100)
grid <- expand.grid(PC1 = x1_range, PC2 = x2_range)
grid$pred <- predict(model_svm, grid)

# 
ggplot(train_data_pca, aes(x = PC1, y = PC2, color = Diagnosis)) +
  geom_point(size = 2) +
  geom_contour(data = grid, aes(z = as.numeric(pred)), breaks = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "SVM Decision Boundary in PCA-reduced Space")
##############################
start_time <- Sys.time()

model_svm <- svm(Diagnosis ~ ., data = train_data, type = "C-classification", kernel = "radial")

end_time <- Sys.time()

predictions <- predict(model_svm, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']

precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

probabilities <- predict(model_svm, test_data, decision.values = TRUE)
roc_obj <- roc(as.numeric(test_data$Diagnosis), attr(probabilities, "decision.values"))
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(test_data$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))


# 
tuned_svm_model <- tune(svm, Diagnosis ~ ., data = train_data, 
                        ranges = list(cost = 10^(-1:2), gamma = 10^(-2:1)))

# 
print(tuned_svm_model$best.parameters)


# confusion matrix
conf_matrix <- as.data.frame(as.table(conf_matrix))
ggplot(conf_matrix, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of KNN") +
  theme_minimal()

# 
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# # ROC curve
ggplot(data = roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc_value, 2), ")"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()


##############  cross validation
library(caret)
library(pROC)

set.seed(123)

# 
train_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,  
  summaryFunction = twoClassSummary 
)

# 
tune_grid <- expand.grid(
  C = 10^seq(-2, 1, by = 1),  
  sigma = 10^seq(-2, -1, by = 1)  
)

start_time <- Sys.time()

model_svm1 <- train(
  Diagnosis ~ .,
  data = mydata,
  method = "svmRadial",
  trControl = train_control,
  tuneLength = 10,
  preProcess = "scale",
  metric = "ROC",
  tuneGrid = tune_grid
)

end_time <- Sys.time()

print(model_svm1)

predictions <- predict(model_svm1, newdata = mydata)
pred_probs <- predict(model_svm1, newdata = mydata, type = "prob")

conf_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(response = mydata$Diagnosis, predictor = pred_probs[, 2])
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))
  
  

######################################################### xgboost
set.seed(123)

library(xgboost)
library(caret)
library(pROC)
View(train_data)
View(test_data)

train_data <- data.frame(lapply(train_data, function(x) if(is.factor(x)) as.numeric(as.factor(x)) else x))
test_data <- data.frame(lapply(test_data, function(x) if(is.factor(x)) as.numeric(as.factor(x)) else x))

train_data$Diagnosis <- as.numeric(train_data$Diagnosis) -1
test_data$Diagnosis <- as.numeric(test_data$Diagnosis) -1

dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-which(names(train_data) == "Diagnosis")]), label = train_data$Diagnosis)
dtest <- xgb.DMatrix(data = as.matrix(test_data[,-which(names(test_data) == "Diagnosis")]), label = test_data$Diagnosis)

watchlist <- list(train = dtrain, eval = dtest)

start_time <- Sys.time()

params <- list(
  objective = "binary:logistic",  
  eta = 0.01, 
  max_depth = 6  
)

model_xgb <- xgb.train(params = params, data = dtrain, nrounds = 100,  watchlist = watchlist)
end_time <- Sys.time()

preds <- predict(model_xgb, dtest)
predictions <- ifelse(preds > 0.5, 1, 0)


conf_matrix <- confusionMatrix(factor(predictions), factor(test_data$Diagnosis))

accuracy <- conf_matrix$overall['Accuracy']

precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

roc_obj <- roc(test_data$Diagnosis, preds)
auc <- auc(roc_obj)

mse <- mean((as.numeric(predictions) - test_data$Diagnosis)^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))



if (!is.null(model_xgb$evaluation_log)) {
  evaluation_log <- model_xgb$evaluation_log
  print(evaluation_log)
} else {
  cat("No evaluation log generated.\n")
}

evaluation_log <- model_xgb$evaluation_log

# 
plot(evaluation_log$iter, evaluation_log$train_logloss, type = "l", col = "blue", ylim = range(evaluation_log[,2:3]), xlab = "Iteration", ylab = "Log Loss", main = "Training and Validation Log Loss")
lines(evaluation_log$iter, evaluation_log$eval_logloss, col = "red")
legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)


conf_matrix_df <- as.data.frame(as.table(conf_matrix$table))
ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  ggtitle("Confusion Matrix of XGBoost") +
  theme_minimal()

# ROC curve
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

ggplot(data = roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc, 2), ")"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

###################  cross validation

set.seed(123)
tune_grid <- expand.grid(
  nrounds = 100,         
  max_depth = 6,         
  eta = 0.01,            
  gamma = 0,              
  colsample_bytree = 0.8, 
  min_child_weight = 1,  
  subsample = 0.8    
)



model_xgb1 <- train(
  Diagnosis ~ ., 
  data = mydata,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  preProcess = c("center", "scale")  
)


print(model_xgb1)

predictions6 <- predict(model_xgb1, newdata = mydata)

confusion_matrix <- confusionMatrix(predictions6, mydata$Diagnosis)
print(confusion_matrix)



library(caret)
library(pROC)
library(xgboost)

set.seed(123)

# 
tune_grid <- expand.grid(
  nrounds = 100,         
  max_depth = 6,         
  eta = 0.01,            
  gamma = 0,             
  colsample_bytree = 0.8, 
  min_child_weight = 1, 
  subsample = 0.8       
)

start_time <- Sys.time()

model_xgb1 <- train(
  Diagnosis ~ ., 
  data = mydata,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  preProcess = c("center", "scale")  
)

end_time <- Sys.time()

print(model_xgb1)

# 
predictions <- predict(model_xgb1, newdata = mydata)
pred_probs <- predict(model_xgb1, newdata = mydata, type = "prob")

# confusion matrix
confusion_matrix <- confusionMatrix(predictions, mydata$Diagnosis)

#
accuracy <- confusion_matrix$overall['Accuracy']
precision <- confusion_matrix$byClass['Pos Pred Value']
recall <- confusion_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

# AUC
roc_obj <- roc(mydata$Diagnosis, pred_probs[, 2])
auc <- auc(roc_obj)

# MSE
mse <- mean((as.numeric(predictions) - as.numeric(mydata$Diagnosis))^2)

train_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

# 
print(paste("准确率: ", round(accuracy * 100, 2), "%", sep = ""))
print(paste("精确度: ", round(precision * 100, 2), "%", sep = ""))
print(paste("召回率: ", round(recall * 100, 2), "%", sep = ""))
print(paste("F1得分: ", round(f1 * 100, 2), "%", sep = ""))
print(paste("MSE: ", round(mse, 4), sep = ""))
print(paste("AUC: ", round(auc, 4), sep = ""))
print(paste("训练时间: ", round(train_time, 3), "秒", sep = ""))



######################################################### 







######################################################### 堆叠（Stacking）

set.seed(123)
#
pred_rf <- predict(model_rf, test_data)
pred_xgb <- predict(model_xgb, dtest)
pred_gbm<-predict(model_gbm,test_data)

###
test_data$pred_rf <- pred_rf
test_data$pred_xgb <- pred_xgb
test_data$pred_gbm <- pred_gbm


meta_model_rf <- randomForest(Diagnosis ~ pred_rf + pred_xgb + pred_gbm, data = test_data, ntree = 500, mtry = 3)
meta_model_gbm <- train(
  Diagnosis ~ pred_rf + pred_xgb + pred_gbm,
  data = test_data,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  metric = "Accuracy",
  tuneGrid = expand.grid(.n.trees = (1:5) * 50, .interaction.depth = 1:5, .shrinkage = 0.1, .n.minobsinnode = 10)
)

str(stacked_features)
#
stacked_features <- data.frame(pred_rf, pred_xgb, pred_gbm)
stacked_features$pred_rf <- ifelse(stacked_features$pred_rf == "Yes", 1, 0)


stacked_features <- model.matrix(~. - 1, data = stacked_features)
stacked_target <- as.numeric(stacked_target)

stacked_target <- as.numeric(as.factor(test_data$Diagnosis)) - 1
dtrain_xgb <- xgb.DMatrix(data = as.matrix(stacked_features), label = stacked_target)
meta_model_xgb <- xgb.train(params = params, data = dtrain_xgb, nrounds = 100)




# RF
predictions_rf <- predict(meta_model_rf, newdata = test_data)
accuracy_rf <- mean(predictions_rf == test_data$Diagnosis)
print(paste("Accuracy of Stacked Model of rf:", round(accuracy_rf * 100, 2), "%"))

# GB
predictions_gbm <- predict(meta_model_gbm, newdata = test_data)
accuracy_gbm <- mean(predictions_gbm == test_data$Diagnosis)
print(paste("Accuracy of Stacked Model of gbm:", round(accuracy_gbm * 100, 2), "%"))

# XGBoost
predictions_xgb <- predict(meta_model_xgb, as.matrix(stacked_features))
predictions_xgb_labels <- ifelse(predictions_xgb > 0.5, 1, 0)  # 阈值 0.5 进行分类
accuracy_xgb <- mean(predictions_xgb_labels == stacked_target)
print(paste("Accuracy of Stacked Model of xgb: ", round(accuracy_xgb * 100, 2), "%", sep = ""))




##########################################  cross validation






set.seed(123)
#
pred_rf1 <- predict(model_rf1, mydata)
pred_xgb1 <- predict(model_xgb1, mydata)
pred_gbm1<-predict(model_gbm1,mydata)

###
mydata$pred_rf1 <- pred_rf1
mydata$pred_xgb1 <- pred_xgb1
mydata$pred_gbm1 <- pred_gbm1


meta_model_rf1 <- randomForest(Diagnosis ~ pred_rf1 + pred_xgb1 + pred_gbm1, data = mydata, ntree = 500, mtry = 3)
meta_model_gbm1 <- train(
  Diagnosis ~ pred_rf1 + pred_xgb1 + pred_gbm1,
  data = mydata,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  metric = "Accuracy",
  tuneGrid = expand.grid(.n.trees = (1:5) * 50, .interaction.depth = 1:5, .shrinkage = 0.1, .n.minobsinnode = 10)
)
meta_model_xgb1 <- train(
  Diagnosis ~ pred_rf1 + pred_xgb1 + pred_gbm1, 
  data = mydata,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  preProcess = c("center", "scale")  
)



# RF
predictions_rf1 <- predict(meta_model_rf1, newdata = mydata)
accuracy_rf1 <- mean(predictions_rf1 == mydata$Diagnosis)
print(paste("Accuracy of Stacked Model of rf:", round(accuracy_rf1 * 100, 2), "%"))

# GB
predictions_gbm1 <- predict(meta_model_gbm1, newdata = mydata)
accuracy_gbm1 <- mean(predictions_gbm1 == mydata$Diagnosis)
print(paste("Accuracy of Stacked Model of gbm:", round(accuracy_gbm1 * 100, 2), "%"))

# XGBoost
predictions_xgb1 <- predict(meta_model_xgb1, newdata = mydata)
accuracy_xgb1 <- mean(predictions_xgb1 == mydata$Diagnosis)
print(paste("Accuracy of Stacked Model of gbm:", round(accuracy_xgb1 * 100, 2), "%"))


