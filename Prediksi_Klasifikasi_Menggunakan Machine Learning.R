# INSTALL AND CALL PACKAGES ==============================================================================================================================================================================================================================================================================
install.packages("rpart")
install.packages("readxl")
install.packages("rpart.plot")
install.packages("caret")
install.packages("e1071")
install.packages("ROSE")
install.packages("smotefamily")
install.packages("randomForest")
install.packages("randomForestExplainer")
install.packages("writexl")

library(rpart)
library(readxl)
library(writexl)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(Metrics)
library(ROSE)
library(dplyr)
library(smotefamily)
library(randomForest)
library(randomForestExplainer)

# MODEL PREDIKSI ==============================================================================================================================================================================================================================================================================
##READ AND CLEAN DATA ====================================================================================================================================================================================================================================================================================
data_training_sheet1 <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 1)
data_testing_sheet2 <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 2)

# Dapatkan nama-nama kolom dari data_testing
nama_kolom_testing <- colnames(data_testing_sheet2)
# Pilih kolom-kolom yang sesuai dari data_training
data_training_sheet1 <- data_training_sheet1[, nama_kolom_testing]

# Menggabungkan data training dan data testing
combined_data <- rbind(data_training_sheet1, data_testing_sheet2)

##EKSPLORASI VISUALISASI DATA ====================================================================================================================================================================================================================================================================================
# Statistik deskriptif 
summary(combined_data[c("Amount", "Time", "Class")])
head(combined_data)

# Filter data berdasarkan kelas 0 (non-penipuan) dan kelas 1 (penipuan)
non_fraud_data <- combined_data[combined_data$Class == 0, ]
fraud_data <- combined_data[combined_data$Class == 1, ]

par(mfrow = c(1, 2))  # Membagi layout menjadi 1 baris dan 2 kolom

# Histogram untuk kelas 0 (non-penipuan)
hist(non_fraud_data$Amount, main = "Histogram of Amount (Class 0)", xlab = "Amount", col = "blue")

# Histogram untuk kelas 1 (penipuan)
hist(fraud_data$Amount, main = "Histogram of Amount (Class 1)", xlab = "Amount", col = "red")

par(mfrow = c(1, 1))


##CEK KORELASI PEARSON ====================================================================================================================================================================================================================================================================================
for (i in 1:28) {
  var_name <- paste("V", i, sep="")
  
  # Korelasi antara variabel independen (misal: V1) dan variabel dependen (Amount)
  correlation <- cor(combined_data[[var_name]], combined_data$Amount)
  cat(paste("Korelasi antara", var_name, "dan Amount:", correlation, "\n"))
  
  # Visualisasi data: Scatterplot antara V1 hingga V28 dan Amount
  scatterplot <- ggplot(combined_data, aes(x = combined_data[[var_name]], y = combined_data$Amount)) +
    geom_point(color = "blue") +
    labs(title = paste("Scatterplot of Amount vs.", var_name))
  print(scatterplot)
}


# Data frame with variables V1 to V28
variables <- combined_data[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", 
                               "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20",
                               "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28")]

# Loop through each variable and calculate Pearson correlation
correlation_results <- list()

for (var_name in names(variables)) {
  cor_result <- cor.test(combined_data$Amount, variables[[var_name]], method = "pearson")
  correlation_results[[var_name]] <- cor_result
}

# Display the results
for (var_name in names(variables)) {
  cat(paste("Variable:", var_name, "\n"))
  print(correlation_results[[var_name]])
  cat("\n")
}

##REGRESI LINIER BERGANDA=================================================================================================================================================================================================================================================================================================================================================================================================
#Seluruh Data
# Membagi data training menjadi variabel independen (X) dan variabel dependen (Y)
X_train <- data_training_sheet1[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28")]
Y_train <- data_training_sheet1$Amount

# Membuat model regresi linier berganda
model_reg1 <- lm(Y_train ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28, data = X_train)

# Menampilkan rincian model
print(summary(model_reg1))

# Memprediksi data testing
X_test <- data_testing_sheet2[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28")]
Y_test <- data_testing_sheet2$Amount
predictions <- predict(model_reg1, newdata = X_test)

# Menghitung RMSE
rmse <- sqrt(mean((predictions - Y_test)^2))

# Menghitung adjusted R-squared
rsquared <- summary(model_reg1)$adj.r.squared

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$Regresi_Linier_Seluruh_Variabel <- predictions

# Menampilkan hasil RMSE dan R-squared yang disesuaikan
cat("RMSE:", rmse, "\n")
cat("R-squared (Adjusted):", rsquared, "\n")


#Regresi liner berganda Variabel Signifikan=======================================================================================================================================================================================================================================================================================================================================
# Membagi data training menjadi variabel independen (X) dan variabel dependen (Y)
X_train <- data_training_sheet1[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V10", "V11", "V15", "V16", "V20", "V23", "V25", "V26", "V27")]
Y_train <- data_training_sheet1$Amount

# Membuat model regresi linier berganda
model_reg2 <- lm(Y_train ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V16 +V20 + V23 + V25 +V26+V27, data = X_train)

# Menampilkan rincian model
print(summary(model_reg2))

# Memprediksi data testing
X_test <- data_testing_sheet2[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28")]
Y_test <- data_testing_sheet2$Amount
predictions <- predict(model_reg2, newdata = X_test)

# Menghitung RMSE
rmse <- sqrt(mean((predictions - Y_test)^2))

# Menghitung adjusted R-squared
rsquared <- summary(model_reg2)$adj.r.squared

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$Regresi_Linier_Variabel_Signifikan <- predictions

# Menampilkan hasil RMSE dan R-squared yang disesuaikan
cat("RMSE:", rmse, "\n")
cat("R-squared (Adjusted):", rsquared, "\n")



##Support Vector Regression==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
# Baca data training dari lembar kerja pertama (sheet 1)
data_training_sheet1 <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 1)

# Baca data testing dari lembar kerja kedua (sheet 2)
data_testing_sheet2 <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 2)


# Buat model SVR dengan kernel linier=================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
# Menciptakan vektor nilai epsilon, C yang akan diuji

# Membagi data training menjadi data prediktor (X) dan target (Y)
X <- data_training_sheet1[, -ncol(data_training_sheet1)] # Menghilangkan kolom Amount
Y <- data_training_sheet1$Amount

# Set parameter grid
param_grid <- expand.grid(
  C = c(0.01, 0.1, 1, 10),
  epsilon = c(0.01, 0.1, 1)
)

# Inisialisasi vektor untuk menyimpan hasil evaluasi model
results <- data.frame()

# Melakukan Grid Search untuk model SVR dengan kernel linier
for (i in 1:nrow(param_grid)) {
  formula <- as.formula(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28)
  model <- svm(formula, data = data_training_sheet1, kernel = "linear", cost = param_grid$C[i], epsilon = param_grid$epsilon[i])
  results[i, "C"] <- param_grid$C[i]
  results[i, "epsilon"] <- param_grid$epsilon[i]
  results[i, "RMSE"] <- sqrt(mean((predict(model, X) - Y)^2))
}

# Menampilkan parameter terbaik berdasarkan RMSE terendah
best_params <- param_grid[which.min(results$RMSE), ]
print(best_params)

# Jumlah iterasi
jumlah_iterasi <- nrow(param_grid)
print(jumlah_iterasi)


# Membagi data testing menjadi data prediktor (X_test) dan target (Y_test)
X_test <- data_testing_sheet2[, -ncol(data_testing_sheet2)] # Menghilangkan kolom Amount
Y_test <- data_testing_sheet2$Amount

# Membuat model SVR dengan parameter terbaik
model_linear <- svm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28, data = data_training_sheet1, kernel = "linear", cost = 0.01, epsilon = 0.1)

# Melakukan prediksi pada data testing
predictions <- predict(model_linear, X_test)

# Menghitung RMSE
rmse <- sqrt(mean((predictions - Y_test)^2))

# Menghitung R-squared (Koefisien Determinasi)
SST <- sum((Y_test - mean(Y_test))^2)
SSR <- sum((predictions - Y_test)^2)
rsquared <- 1 - (SSR / SST)

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$SVR_Linier <- predictions

# Menampilkan hasil RMSE dan R-squared
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsquared, "\n")



#PARAMETER KERNEL RBF====================================================================================================================================================================================================================================================================================================================================================================================================================================================
# Set parameter grid untuk kernel RBF
param_grid_rbf <- expand.grid(
  C = c(0.01, 0.1, 1, 10),
  epsilon = c(0.01, 0.1, 1),
  gamma = c(0.01, 0.1, 1)
)

# Inisialisasi vektor untuk menyimpan hasil evaluasi model RBF
results_rbf <- data.frame()

# Melakukan Grid Search untuk model SVR dengan kernel RBF
for (i in 1:nrow(param_grid_rbf)) {
  formula <- as.formula(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28)
  model_rbf <- svm(formula, data = data_training_sheet1, kernel = "radial", cost = param_grid_rbf$C[i], epsilon = param_grid_rbf$epsilon[i], gamma = param_grid_rbf$gamma[i])
  results_rbf[i, "C"] <- param_grid_rbf$C[i]
  results_rbf[i, "epsilon"] <- param_grid_rbf$epsilon[i]
  results_rbf[i, "gamma"] <- param_grid_rbf$gamma[i]
  results_rbf[i, "RMSE"] <- sqrt(mean((predict(model_rbf, X) - Y)^2))
}

# Menampilkan parameter terbaik berdasarkan RMSE terendah untuk kernel RBF
best_params_rbf <- param_grid_rbf[which.min(results_rbf$RMSE), ]
print(best_params_rbf)

# Jumlah iterasi
jumlah_iterasi2 <- nrow(param_grid_rbf)
print(jumlah_iterasi2)

# Membuat model SVR dengan parameter terbaik
best_model_rbf <- svm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28, data = data_training_sheet1, kernel = "radial", cost = 10, epsilon = 0.01, gamma = 0.01)

# Melakukan prediksi pada data testing
predictions_rbf <- predict(best_model_rbf, X_test)

# Menghitung RMSE
rmse_rbf <- sqrt(mean((predictions_rbf - Y_test)^2))

# Menghitung R-squared (Koefisien Determinasi)
SST_rbf <- sum((Y_test - mean(Y_test))^2)
SSR_rbf <- sum((predictions_rbf - Y_test)^2)
rsquared_rbf <- 1 - (SSR_rbf / SST_rbf)

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$SVR_RBF <- predictions_rbf

# Menampilkan hasil RMSE dan R-squared
cat("RMSE:", rmse_rbf, "\n")
cat("R-squared:", rsquared_rbf, "\n")


# Buat model SVR dengan kernel polynomial==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
# Set parameter grid untuk kernel polynomial
param_grid_poly <- expand.grid(
  C = c(0.01, 0.1, 1, 10),
  epsilon = c(0.01, 0.1, 1),
  degree = c(2, 3, 4) 
)

# Inisialisasi vektor untuk menyimpan hasil evaluasi model polynomial
results_poly <- data.frame()

# Melakukan Grid Search untuk model SVR dengan kernel polynomial
for (i in 1:nrow(param_grid_poly)) {
  formula <- as.formula(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28)
  model_poly <- svm(formula, data = data_training_sheet1, kernel = "polynomial", cost = param_grid_poly$C[i], epsilon = param_grid_poly$epsilon[i], degree = param_grid_poly$degree[i])
  results_poly[i, "C"] <- param_grid_poly$C[i]
  results_poly[i, "epsilon"] <- param_grid_poly$epsilon[i]
  results_poly[i, "degree"] <- param_grid_poly$degree[i]
  results_poly[i, "RMSE"] <- sqrt(mean((predict(model_poly, X) - Y)^2))
}

# Menampilkan parameter terbaik berdasarkan RMSE terendah untuk kernel polynomial
best_params_poly <- param_grid_poly[which.min(results_poly$RMSE), ]
print(best_params_poly)

# Jumlah iterasi
jumlah_iterasi3 <- nrow(param_grid_poly)
print(jumlah_iterasi3)

# Membuat model SVR dengan kernel polynomial dan parameter terbaik
best_model_poly <- svm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28, data = data_training_sheet1, kernel = "polynomial", cost = 10, epsilon = 0.01, degree = 3)

# Melakukan prediksi pada data testing
predictions_poly <- predict(best_model_poly, X_test)

# Menghitung RMSE untuk model polynomial
rmse_poly <- sqrt(mean((predictions_poly - Y_test)^2))

# Menghitung R-squared (Koefisien Determinasi) untuk model polynomial
SST_poly <- sum((Y_test - mean(Y_test))^2)
SSR_poly <- sum((predictions_poly - Y_test)^2)
rsquared_poly <- 1 - (SSR_poly / SST_poly)

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$SVR_Polynomial <- predictions_poly

# Menampilkan hasil RMSE dan R-squared untuk model polynomial
cat("RMSE (Polynomial):", rmse_poly, "\n")
cat("R-squared (Polynomial):", rsquared_poly, "\n")

# Cetak ringkasan model
print(model_linear)
print(best_model_rbf)
print(best_model_poly)



##Regresi Polynomial====================================================================================================================================================================================================================================================================================
# Baca data
data_training <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 1)
data_testing <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 2)

# Derajat polinomial yang akan diuji
polynomial_degrees <- c(1,2, 3, 4, 5, 8, 10)

# Inisialisasi vektor untuk menyimpan hasil RMSE dan R-squared
rmse_results <- vector("numeric", length(polynomial_degrees))
rsquare_results <- vector("numeric", length(polynomial_degrees))

# Loop melalui derajat polinomial yang diuji
for (i in 1:length(polynomial_degrees)) {
  polynomial_degree <- polynomial_degrees[i]
  
  # Membuat formula untuk regresi polinomial
  formula <- as.formula(paste("Amount ~", paste(paste0("poly(V", 1:28, ",", polynomial_degree, ")"), collapse = " + ")))
  
  # Melakukan regresi polinomial
  polynomial_model <- lm(formula, data = data_training)
  
  # Melakukan prediksi pada data testing
  predictions <- predict(polynomial_model, newdata = data_testing)
  
  # Menghitung RMSE pada data testing
  rmse <- rmse(predictions, data_testing$Amount)
  rmse_results[i] <- rmse
  
  # Menghitung R-squared (R^2) pada data testing
  rsquare <- R2(predictions, data_testing$Amount)
  rsquare_results[i] <- rsquare
}

# Mengatur opsi tampilan angka
options(scipen = 999)

# Menampilkan hasil RMSE dan R-squared untuk setiap derajat polinomial
results_df <- data.frame(Polynomial_Degree = polynomial_degrees, RMSE = rmse_results, R_squared = rsquare_results)
print(results_df)

##
# Membuat model regresi polinomial
polynomial_degree <- 2  # Derajat polinomial yang optimum

# Membuat formula untuk regresi polinomial
formula <- as.formula(paste("Amount ~", paste(paste0("poly(V", 1:28, ",", polynomial_degree, ")"), collapse = " + ")))

# Melakukan regresi polinomial
polynomial_model <- lm(formula, data = data_training)

# Melihat ringkasan model
summary(polynomial_model)

# Melakukan prediksi pada data testing
predictions <- predict(polynomial_model, newdata = data_testing)

# Hasil prediksi
head(predictions)

# Melihat estimasi parameter model
coefficients <- coef(polynomial_model)
print(coefficients)

# Menghitung RMSE pada data testing
rmse <- rmse(predictions, data_testing$Amount)
cat("RMSE pada Data Testing:", rmse, "\n")

# Menghitung R-squared (R^2) pada data testing
rsquare <- R2(predictions, data_testing$Amount)
cat("R-squared (R^2) pada Data Testing:", rsquare, "\n")

# Menyimpan hasil prediksi ke data_testing_sheet2
data_testing_sheet2$Regresi_Polynomial <- predictions


#==========================================================================================================================================================================================================================
# MODEL KLASIFIKASI==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
## READ AND CLEAN DATA ========================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
train_val <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 1)
train_val2 <- train_val[,-1]
train_val3 <- train_val2[,-1]
train_val <- train_val3[,-29]
View(train_val)

testing <- read_excel("D:/Data STATBIS ITS/Sem 7/Machine learning/Data ETS.xlsx", sheet = 2)
testing2 <- testing[,-1]
testing3 <- testing2[,-1]
testing <- testing3[,-29]
View(testing)

table(train_val$Class)

# HANDLING IMBALANCE DATA WITH ROSE ==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
set.seed(100)
rose = ROSE(Class ~ ., data = train_val)$data
table(rose$Class)

View(rose)


# CLASSIFICATION WITH ROSE IMBALANCE RESULT ====================================================================================================================================================================================================================================================================================================================================================================================================================================================

## REGRESI LOGISTIK==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
model_rl <- glm(Class ~ ., data = rose, family = "binomial")

summary(model_rl)

predictions_rl <- predict(model_rl, testing, type = "response")
predictions_rl <- ifelse(predictions_rl >= 0.5, 1, 0)

conf_mtrx_rl <- confusionMatrix(as.factor(testing$Class),
                                as.factor(predictions_rl),
                                positive = "1")
print(conf_mtrx_rl)

# Menyimpan hasil klasifikasi ke data_testing_sheet2
data_testing_sheet2$Regresi_Logistik <- predictions_rl

## DECISION TREE =============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
model_dt <- rpart(Class ~ ., data = rose, method = "class")

summary(model_dt)
prp(model_dt)

predictions_dt <- predict(model_dt, testing, type = "class")

conf_mtrx_dt <- confusionMatrix(as.factor(testing$Class),
                                as.factor(predictions_dt),
                                positive = "1")
print(conf_mtrx_dt)

# Menyimpan hasil klasifikasi ke data_testing_sheet2
data_testing_sheet2$Regresi_Desicion_Tree <- predictions_dt

## SUPPORT VECTOR MACHINE ====================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
set.seed(100)
best_c <- tune(svm, Class ~ ., data = rose, kernel = "linear", 
               ranges=list(cost=c(0.001,0.005,0.01,0.1,1)))
best_c

print(best_c$best.model)

model_svm <- svm(Class ~ ., data = rose, kernel = "polynomial", 
                 cost = 0.005)

predictions_svm <- predict(model_svm, testing)
predictions_svm <- ifelse(predictions_svm >= 0.5, 1, 0)

conf_mtrx_svm <- confusionMatrix(as.factor(testing$Class),
                                 as.factor(predictions_svm),
                                 positive = "1")

print(conf_mtrx_svm)

# Menyimpan hasil klasifikasi ke data_testing_sheet2
data_testing_sheet2$SVM_Kernel <- predictions_svm 

## RANDOM FOREST ============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
set.seed(100)  # Atur seed untuk mereproduksi hasil

# Mengubah tipe data kolom "class" menjadi faktor
train_val$Class <- as.factor(train_val$Class)

# Definisikan kombinasi parameter
ntrees <- c(100, 250, 500, 1000)
mtry_values <- c(4, 9, 2)

# Membuat rangkaian parameter
parameter_combinations <- expand.grid(ntree = ntrees, mtry = mtry_values)

# Inisialisasi vektor untuk menyimpan hasil OOB
oob_results <- numeric(length(parameter_combinations$ntree))

# Loop melalui kombinasi parameter
for (i in 1:nrow(parameter_combinations)) {
  ntree <- parameter_combinations$ntree[i]
  mtry <- parameter_combinations$mtry[i]
  
  # Membuat model Random Forest
  rf_model <- randomForest(train_val$Class ~ ., data = train_val, ntree = ntree, mtry = mtry)
  
  # Menghitung OOB error
  oob_error <- rf_model$err.rate[nrow(rf_model$err.rate), 1]
  
  # Menyimpan hasil OOB ke dalam vektor
  oob_results[i] <- oob_error
}

# Menampilkan hasil OOB untuk masing-masing kombinasi parameter
result_df <- data.frame(parameter_combinations, OOB_Error = oob_results)
print(result_df)

summary(rf_model)


# Melatih model Random Forest dengan parameter terbaik
best_model_rf <- randomForest(Class ~ ., data = rose, ntree = 500, mtry = 4, localImp = TRUE)

# Mengganti 'data_testing' dengan nama dataframe yang sesuai dengan data testing Anda
# Lakukan prediksi pada data testing
predictions_rf <- predict(best_model_rf, testing)
predictions_rf <- ifelse(predictions_rf >= 0.5, 1, 0)

conf_mtrx_rf <- confusionMatrix(as.factor(testing$Class),
                                as.factor(predictions_rf),
                                positive = "1")
print(conf_mtrx_rf)

# Menyimpan hasil klasifikasi ke data_testing_sheet2
data_testing_sheet2$Random_forest <- predictions_rf

#MENYIMPAN CSV HASIL PREDIKSI DAN KLASIFIKASI=====================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

write_xlsx(data_testing_sheet2, "D:/Data STATBIS ITS/Sem 7/Machine learning/DataTesting_with_Predictions.xlsx")

#=====================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

