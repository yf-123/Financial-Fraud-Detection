#library for correlations
#install.packages("corrplot")
library(corrplot)
#library for plotting the samples
#install.packages("caret")
library(caret)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2)
#library for data manipulation
#install.packages("dplyr")
library(dplyr)
#library for drawing wordcloud
#install.packages("wordcloud")
library(wordcloud)
#library for transform data between wide and long formats
#install.packages("reshape2")
library(reshape2)
#install.packages("scales")
library(scales)
#install.packages("factoextra")
library(factoextra)
#install.packages("readr")
library(readr)
#install.packages("DMwR2")
library(DMwR2)
#install.packages("nnet")
library(nnet)
#install.packages("smotefamily")
library(smotefamily)
#install.packages("randomForest")
library(randomForest)
#install.packages("keras")
library(keras)
library(tensorflow)
#install.packages("reticulate")
library(reticulate)
#install.packages("rpart")
library(rpart)
library(ROCR)


Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# Load the dataset
a1_data <- read.csv("D:\\HKUrelated\\HKUcourses\\FinancialFraudAnalytics\\workspaceForVSCodeR\\A1_data.csv")
#a1_data <- read.csv("\\.A1_data.csv")

# Exploratory Data Analysis
# 1. Distinguish Attributes
#View data structure
print("Structure of dataset:")
str(a1_data)
print("Summary of dataset:")
print(summary(a1_data))

# 2. Univariate Analysis
# Calculate the mean of each column
mean_values <- a1_data %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
print("Mean values of each column:")
print(mean_values)

# Calculate the standard deviation
sd_values <- a1_data %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
print("SD of each column:")
print(sd_values)

# Delete columns with a standard deviation of 0
num_col <- a1_data %>%
  select(where(is.numeric))
print(paste("Number of numerical columns:", ncol(num_col)))
sd_vector <- as.numeric(sd_values)
names(sd_vector) <- names(sd_values)
non_zero_sd_columns <- names(sd_vector)[!is.na(sd_vector) & sd_vector != 0]
num_col <- num_col %>%
  select(all_of(non_zero_sd_columns))
print(paste("Number of numerical columns after removing zero SD columns:",
            ncol(num_col)))

other_col <- a1_data %>%
  select(where(~ !is.numeric(.)))
print(paste("Number of non-numerical variables:", ncol(other_col)))
a1_data <- bind_cols(other_col, num_col)
print(paste("Number of columns after removing zero SD columns:",
            ncol(a1_data)))

# Using the NA to fill missing values in non-numerical variables
a1_data <- a1_data %>%
  mutate(across(where(~ !is.numeric(.)), ~ ifelse(. == "", NA, .)))

# Check the number of missing values in each column
missing_values <- colSums(is.na(a1_data))
# Print the percentage of missing values for each column
total_values <- nrow(a1_data)
missing_percentage <- (missing_values / total_values) * 100
print("missing_percentage:")
print(missing_percentage)


#isFraud pie chart
fraud_counts <- table(a1_data$isFraud)
print(fraud_counts)
labels <- c("Not Fraud", "Is Fraud")
names(fraud_counts) <- labels
percentages <- round(fraud_counts / sum(fraud_counts) * 100, 1)
labels_with_p <- paste(labels, percentages, "%", sep=" ")
pie(fraud_counts, labels = labels_with_p, main = "Distribution of isFraud", col = c("lightblue", "lightcoral"))

#TransactionAmt histogram  
TAmt_mean <- mean(a1_data$TransactionAmt)
TAmt_median <- median(a1_data$TransactionAmt)
ggplot(a1_data, aes(x = TransactionAmt)) +
  geom_histogram(binwidth = 10, aes(y = ..count../sum(..count..)*100), fill = "lightblue", color = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = TAmt_mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = TAmt_median), color = "green", linetype = "dashed", linewidth = 0.5) +
  geom_text(aes(x = TAmt_mean, y = -1, label = paste("Mean:", round(TAmt_mean, 1))), color = "red", vjust = -1) +
  geom_text(aes(x = TAmt_median, y = 14, label = paste("Median:", round(TAmt_median, 1))), color = "green", vjust = -1) +
  ggtitle("Distribution of Transaction Amount") +
  labs(x = "Transaction Amount", y = "Percentage(%)") +
  theme_minimal()

# ProductCD pie chart
ProductCD_counts <- table(a1_data$ProductCD)
print(ProductCD_counts)
labels <- c("key_AD", "key_LY", "key_TP", "key_WF")
names(ProductCD_counts) <- labels
percentages <- round(ProductCD_counts / sum(ProductCD_counts) * 100, 1)
labels_with_p <- paste(labels, percentages, "%", sep=" ")
pie(ProductCD_counts, labels = labels_with_p, main = "Distribution of ProductCD labels", col = c("lightblue", "lightcoral", "lightgreen", "gray"))

# Card1 Boxplot
card1_mean <- mean(a1_data$card1)
card1_median <- median(a1_data$card1)
ggplot(a1_data, aes(y = card1)) +
  geom_boxplot() +
  ggtitle("Distribution of card1") +
  geom_hline(aes(yintercept = card1_mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = card1_median), color = "green", linetype = "dashed", linewidth = 0.5) +
  geom_text(aes(x = 0.2, y = card1_mean, label = paste("Mean:", round(card1_mean, 1))), color = "red", vjust = -0.5) +
  geom_text(aes(x = 0.3, y = card1_median, label = paste("Median:", round(card1_median, 1))), color = "green", vjust = -0.5) +
  labs(x = "Frequency", y = "card1")

# Card2 Boxplot
card2_mean <- mean(a1_data$card2)
card2_median <- median(a1_data$card2)
ggplot(a1_data, aes(y = card2)) +
  geom_boxplot() +
  ggtitle("Distribution of card2") +
  geom_hline(aes(yintercept = card2_mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = card2_median), color = "green", linetype = "dashed", linewidth = 0.5) +
  geom_text(aes(x = 0.2, y = card2_mean, label = paste("Mean:", round(card2_mean, 1))), color = "red", vjust = -0.5) +
  geom_text(aes(x = 0.3, y = card2_median, label = paste("Median:", round(card2_median, 1))), color = "green", vjust = -0.5) +
  labs(x = "Frequency", y = "card2")

# Card3 Boxplot
card3_mean <- mean(a1_data$card3)
card3_median <- median(a1_data$card3)
ggplot(a1_data, aes(y = card3)) +
  geom_boxplot() +
  ggtitle("Distribution of card3") +
  geom_hline(aes(yintercept = card3_mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = card3_median), color = "green", linetype = "dashed", linewidth = 0.5) +
  geom_text(aes(x = 0.2, y = card3_mean, label = paste("Mean:", round(card3_mean, 1))), color = "red", vjust = -0.5) +
  geom_text(aes(x = 0.3, y = card3_median, label = paste("Median:", round(card3_median, 1))), color = "green", vjust = -0.5) +
  labs(x = "Frequency", y = "card3")

# Card4 pie chart
card4_counts <- table(a1_data$card4)
print(card4_counts)
labels <- c("american express", "discover", "mastercard", "visa")
names(card4_counts) <- labels
percentages <- round(card4_counts / sum(card4_counts) * 100, 1)
labels_with_p <- paste(labels, percentages, "%", sep=" ")
pie(card4_counts, labels = labels_with_p, main = "Distribution of card4 labels", col = c("lightblue", "lightcoral", "lightgreen", "gray"))

# Card6 bar chart
card6_counts <- table(a1_data$card6)
print(card6_counts)
card6_df <- as.data.frame(card6_counts)
colnames(card6_df) <- c("card6", "count")
card6_df$percentage <- round(card6_df$count / sum(card6_df$count) * 100, 1)
ggplot(card6_df, aes(x = card6, y = count, fill = card6)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = count), vjust = -0.5) +
  geom_text(aes(label = paste(percentage, "%")), vjust = 1.5, color = "black") +
  labs(title = "Distribution of card6 labels", x = "Card Type", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "pink", "gray"))

# id_31 bar chart
id31_counts <- a1_data %>%
  count(id_31) %>%
  arrange(desc(n))
top15 <- id31_counts %>%
  top_n(15, n)
id31_data <- a1_data %>%
  mutate(id_31 = ifelse(id_31 %in% top15$id_31, as.character(id_31), "others"))
id31_counts <- id31_data %>%
  count(id_31) %>%
  arrange(desc(n))
id31_counts <- id31_counts %>%
  mutate(proportion = n / sum(n))
ggplot(id31_counts, aes(x = reorder(id_31, -n), y = n, fill = id_31)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5) +  # 在柱子上方显示数量
  geom_text(aes(label = percent_format(accuracy = 0.1)(proportion)), vjust = 1.5, color = "black") +
  labs(title = "Distribution of id_31", x = "id_31", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightblue", "pink", "gray", "orange", "purple",
                               "green", "yellow", "red", "blue", "brown", "cyan",
                               "magenta", "darkgreen", "darkblue", "darkred", "darkgray"))


#DeviceType pie chart
DeType_counts <- table(a1_data$DeviceType)
print(DeType_counts)
labels <- c("desktop", "mobile")
names(DeType_counts) <- labels
percentages <- round(DeType_counts / sum(DeType_counts) * 100, 1)
labels_with_p <- paste(labels, percentages, "%", sep=" ")
pie(DeType_counts, labels = labels_with_p, main = "Distribution of DeviceType labels", col = c("lightblue","pink"))

#DeviceInfo bar chart
DeInfo_counts <- a1_data %>%
  count(DeviceInfo) %>%
  arrange(desc(n))
DeInfo_top15 <- DeInfo_counts %>%
  top_n(15, n)
DeInfo_data <- a1_data %>%
  mutate(DeviceInfo = ifelse(DeviceInfo %in% DeInfo_top15$DeviceInfo, as.character(DeviceInfo), "others"))
DeInfo_counts <- DeInfo_data %>%
  count(DeviceInfo) %>%
  arrange(desc(n))
ggplot(DeInfo_counts, aes(x = reorder(DeviceInfo, -n), y = n, fill = DeviceInfo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Distribution of DeviceInfo", x = "DeviceInfo", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightblue", "pink", "gray", "orange", "purple",
                               "green", "yellow", "red", "blue", "brown", "cyan",
                               "magenta", "darkgreen", "darkblue", "darkred", "darkgray"))
#DeviceInfo wordcloud  min.freq = 100
device_counts <- a1_data %>%
  count(DeviceInfo) %>%
  arrange(desc(n))
wordcloud(words = device_counts$DeviceInfo, freq = device_counts$n, min.freq = 100, colors = brewer.pal(8, "Dark2"))


# 3. Bi-/Multi-variate Analysis
# Scatter Plot of  TransactionDT & TransactionDT(Hour)
ggplot(a1_data, aes(x = TransactionDT, y = TransactionDT..Hour.)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of TransactionDT and TransactionDT(Hour)", x = "TransactionDT", y = "TransactionDT(Hour)") +
  theme_minimal()

# Boxplot of TransactionAmt & isFraud
ggplot(a1_data, aes(x = factor(isFraud), y = TransactionAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of TransactionAmt by isFraud", x = "isFraud", y = "TransactionAmt") +
  theme_minimal()

# bar chart of isFraud and card4 & card6
card4_var <- "card4"
card6_var <- "card6"
a1_data_long <- a1_data %>%
  pivot_longer(cols = c(card4_var, card6_var), names_to = "variable", values_to = "value")
ggplot(a1_data_long, aes(x = factor(isFraud), fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "isFraud", y = "Proportion", fill = "card4 & card6") +
  theme_minimal()

# bar chart of isFraud and id_30 & DeviceType
id_30_var <- "id_30"
DeType_var <- "DeviceType"
a1_data_long <- a1_data %>%
  pivot_longer(cols = c(id_30_var, DeType_var), names_to = "variable", values_to = "value")
ggplot(a1_data_long, aes(x = factor(isFraud), fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "isFraud", y = "Proportion", fill = "id_30 & DeviceType") +
  theme_minimal()

# Correlation Analysis of columns D1 to D15 except D11
D1_D15_subset <- a1_data[, !names(a1_data) %in% "D11"]
cor_matrix <- cor(D1_D15_subset[, paste0("D", c(1:10, 12:15))], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# Correlation Analysis of columns V310 to V314 
D1_D15_cor_matrix <- cor(a1_data[, paste0("V", 310:314)], use = "pairwise.complete.obs")
corrplot(D1_D15_cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# Correlation Analysis of columns C1 to C14 except C5 & C9
C1_C14_subset <- a1_data[, !names(a1_data) %in% c("C5", "C9")]
cor_matrix <- cor(C1_C14_subset[, paste0("C", c(1:4, 6:8, 10:14))], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)


# Correlation Analysis of columns id_1 to id_32 except some categorical columns
id1_32_subset <- a1_data[, !names(a1_data) %in% c("id_12", "id_15", "id_16",
                                                  "id_23", "id_27", "id_28", "id_29", "id_30", "id_31")]
required_columns <- paste0("id_", sprintf("%02d", c(1:9, 10:11, 13:14, 17:22, 24:26, 32)))
cor_matrix <- cor(id1_32_subset[, required_columns], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# Correlation Analysis of columns id_12 to id_29  some categorical columns
data_id_12 <- a1_data
id_12_columns <- paste0("id_", c(12, 15, 16, 27:29))
data_id_12[id_12_columns] <- lapply(data_id_12[id_12_columns], function(x) as.numeric(factor(x)))
cor_matrix <- cor(data_id_12[, id_12_columns], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         mar = c(0, 0, 1, 0))

# Correlation Analysis of columns id_35 to id_38 categorical columns
data_id_35 <- a1_data
id_35_columns <- paste0("id_", c(35:38))
data_id_35[id_35_columns] <- lapply(data_id_35[id_35_columns], function(x) as.numeric(factor(x)))
cor_matrix <- cor(data_id_35[, id_35_columns], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         mar = c(0, 0, 1, 0))

# Correlation Analysis of all numerical features
all_subset <- a1_data[, !names(a1_data) %in% c("TransactionID", "TransactionDT..Hour.")]
cor_matrix <- cor(all_subset %>% select(where(is.numeric)), use = "pairwise.complete.obs")
print(cor_matrix)

# 4. Detect erroneous & missing values
#Delete columns with pearson correlation greater than 0.5 
#obtained from multivariate analysis
columns_to_remove <- c("TransactionDT..Hour.",
                       "D2", "D3", "D4", "D6", "D7", "D12", "D13", "D15",
                       "V311", "V312", "V314",
                       "C2", "C4", "C6", "C7", "C8", "C10", "C11", "C12", "C13", "C14",
                       "id_03", "id_22", "id_16", "id_28","id_29", 
                       "id_17")
a1_data <- a1_data %>% select(-all_of(columns_to_remove))

# Delete columns with missing values exceeding 30%
threshold_col <- 0.3 * total_values
a1_data <- a1_data[, colSums(is.na(a1_data)) <= threshold_col]
print(paste("Number of columns after deleting:", ncol(a1_data)))

# Delete rows with missing values exceeding 30%
threshold_row <- 0.3 * ncol(a1_data)
a1_data <- a1_data[rowSums(is.na(a1_data)) <= threshold_row, ]
print(paste("Number of rows after deleting:", nrow(a1_data)))

# Delete duplicate rows
a1_data <- a1_data %>%
  distinct()

# Using the mean to fill missing values in numerical variables
mean_vector <- as.numeric(mean_values)
names(mean_vector) <- names(mean_values)
a1_data <- a1_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.),
                                            mean_vector[cur_column()], .)))
# Using the mode to fill missing values in categorical variables
fill_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
a1_data <- a1_data %>%
  mutate(across(where(~ !is.numeric(.)), ~ ifelse(is.na(.),
                                                  fill_mode(.), .)))
#Check the result of filling
filling_values_check <- colSums(is.na(a1_data))
filling_values_check <- filling_values_check[filling_values_check > 0]
for (col in names(filling_values_check)) {
  print(paste("Column", col, "has", filling_values_check[col], "missing values."))
}

#Check the summary of dataset again
print("Check the summary of dataset again:")
print(summary(a1_data))
#5. Detect outliers
data_toclean <- a1_data
# Calculate z-score and remove outliers, while saving the deleted rows
remove_outliers <- function(df, threshold = 3) {
  # Calculate z-score of each colummns
  z_scores <- df %>%
    mutate(across(where(is.numeric), ~ (.-mean(.))/sd(.), .names = "z_{col}"))
  # mark the outliers
  outliers <- z_scores %>%
    mutate(outlier = rowSums(across(starts_with("z_"), ~ abs(.) > threshold)) > 0)
  # save the deleted rows for future use
  row_deleted <- df[outliers$outlier, ]
  # delete the rows with outliers
  df_cleaned <- df[!outliers$outlier, ]
  return(list(cleaned_data = df_cleaned, deleted_rows = row_deleted))
}
result <- remove_outliers(data_toclean)
data_cleaned <- result$cleaned_data
row_deleted <- result$deleted_rows

print("Check the summary of dataset again:")
print(summary(data_cleaned))

#delete the colums with all the 0 value
data_cleaned <- select(data_cleaned, -C3)

# backup the deleted rows
write.csv(data_cleaned, "data_cleaned.csv", row.names = FALSE)

#Feature Engineering
# Create a new feature: hours of transaction time
data_addHour <- a1_data %>%
  mutate(TransactionHour = (TransactionDT %% (24 * 3600)) %/% 3600)
# Calculate the number of frauds per hour
fraud_by_hour <- data_addHour %>%
  group_by(TransactionHour) %>%
  summarise(FraudCount = sum(isFraud))
# Calculate the percentage of fraud per hour to the total fraud count
total_fraud <- sum(fraud_by_hour$FraudCount)
fraud_by_hour <- fraud_by_hour %>%
  mutate(FraudPercent = FraudCount / total_fraud * 100)

data_addHour <- select(data_addHour, -TransactionDT, -TransactionID)


##############Perform PCA analysis
categorical_cols <- data_addHour %>%
  select(where(~ is.character(.))) %>%
  colnames()
for (col in categorical_cols) {
  data_addHour[[col]] <- as.numeric(factor(data_addHour[[col]]))
}

target <- data_addHour$isFraud
features <- data_addHour %>%
  select(-isFraud)
features_scaled <- scale(features)
pca_result <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

summary(pca_result)

explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)


plot_data <- data.frame(
  Feature = colnames(features),
  ExplainedVarianceRatio = explained_variance
)

plot_data_sorted <- plot_data %>%
  arrange(desc(ExplainedVarianceRatio))


plot_data_sorted <- plot_data_sorted %>%
  mutate(RowNumber = row_number())

ggplot(plot_data_sorted, aes(x = reorder(Feature, -ExplainedVarianceRatio), y = ExplainedVarianceRatio)) +
  geom_line(group = 1, color = "steelblue2") +
  geom_point(color = "steelblue") +
  geom_text(aes(label = sprintf("%.3f", ExplainedVarianceRatio), 
                vjust = ifelse(RowNumber %% 2 == 0, -0.5, 1.5)), 
            hjust = 0.5) +
  theme_minimal() +
  labs(title = "Explained Variance by Original Features",
       x = "Features",
       y = "Explained Variance Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pca_data <- as.data.frame(pca_result$x)
pca_data$isFraud <- target

# head(pca_data)
fraud_pca_mean <- pca_data %>%
  filter(isFraud == 1) %>%
  summarise(across(starts_with("PC"), mean))

pca_components <- as.data.frame(pca_result$rotation)
important_features <- apply(pca_components, 2, function(x) names(x)[which.max(abs(x))])

fraud_pca_mean_long <- fraud_pca_mean %>%
  pivot_longer(cols = everything(), names_to = "PrincipalComponent", values_to = "MeanValue")

fraud_pca_mean_long$PrincipalComponent <- important_features[fraud_pca_mean_long$PrincipalComponent]
fraud_pca_mean_long$MeanValue <- abs(fraud_pca_mean_long$MeanValue)

fraud_pca_mean_long$PrincipalComponent <- make.unique(fraud_pca_mean_long$PrincipalComponent)

fraud_pca_mean_long <- fraud_pca_mean_long %>%
  arrange(desc(MeanValue))

ggplot(fraud_pca_mean_long, aes(y = reorder(PrincipalComponent, MeanValue), x = MeanValue)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  geom_text(aes(label = sprintf("%.3f", MeanValue)), hjust = -0.1) +
  theme_minimal() +
  labs(title = "Feature Importance of isFraud=1",
       x = "Absolute Mean Value",
       y = "Features") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))




#####Splitting the dataset####
set.seed(1111)
dataset_splited = data.frame(data_addHour)

train = sample(nrow(dataset_splited), 0.7*nrow(dataset_splited), replace = FALSE)
TrainSet = dataset_splited[train,]
TestSet = dataset_splited[-train,]


data_factor = TrainSet %>% mutate_if(is.character,as.factor) 
data_factor = as.data.frame(data_factor)
data_numeric <- data_factor %>%
  mutate(across(where(is.factor) & !all_of("isFraud"), as.numeric))
data_numeric$isFraud <- as.factor(data_numeric$isFraud)
round(prop.table(table(data_numeric$isFraud)), 2)
table(data_numeric$isFraud)

data_factor_test = TestSet %>% mutate_if(is.character,as.factor) 
data_factor_test = as.data.frame(data_factor_test)
data_numeric_test <- data_factor_test %>%
  mutate(across(where(is.factor) & !all_of("isFraud"), as.numeric))
data_numeric_test <- data_numeric_test %>%
  mutate(across(where(is.logical), as.numeric))
data_numeric_test$isFraud <- as.factor(data_numeric_test$isFraud)
round(prop.table(table(data_numeric_test$isFraud)), 2)
table(data_numeric_test$isFraud)


#####SMOTE#####

majority_count <- sum(data_numeric$isFraud == 0)
minority_count <- sum(data_numeric$isFraud == 1)

dup_size_needed <- (majority_count - minority_count) / minority_count
train_data_after_smote <- SMOTE(data_numeric[,-which(names(data_numeric) == "isFraud")], 
                          data_numeric$isFraud, 
                 K = 5, 
                 dup_size = dup_size_needed)

smote_data <- train_data_after_smote$data
smote_class <- smote_data$class

train_data_after_smote <- data.frame(smote_data, isFraud = smote_class)

train_data_after_smote$isFraud <- as.factor(train_data_after_smote$isFraud)

table(train_data_after_smote$isFraud)


train_data_after_smote <- train_data_after_smote %>% select(-class)

#####################
####Random Forest####

set.seed(2222)
start_time_rf <- Sys.time()
model_rf = randomForest(isFraud ~ ., data=train_data_after_smote,mtry = 33, ntree=500, importance = TRUE) 
end_time_rf <- Sys.time()
time_taken_rf <- end_time_rf - start_time_rf
print(paste("Elapsed time of Random Forest:", time_taken_rf))

model_rf

prediction_rf = predict(model_rf,TestSet)

prediction_rf <- factor(prediction_rf, levels = c("0", "1"))
TestSet$isFraud <- factor(TestSet$isFraud, levels = c("0", "1"))
prediction_rf <- factor(prediction_rf)
TestSet$isFraud <- factor(TestSet$isFraud)
sum(is.na(prediction_rf))
sum(is.na(TestSet$isFraud))
levels(prediction_rf)
levels(TestSet$isFraud)

confusionMatrix(prediction_rf,TestSet$isFraud)

print("Random Forest done!!!")

#####################################
####Neural Network###
#nn_1##############################

start_time_nn1 <- Sys.time()
model_nn <- nnet(isFraud ~ ., data = train_data_after_smote, maxit = 100, size = 5, decay = 0.1)
end_time_nn1 <- Sys.time()
time_taken_nn1 <- end_time_nn1 - start_time_nn1
print(paste("Elapsed time of Neural Network 1:", time_taken_nn1))
train_predictions <- predict(model_nn, train_data_after_smote, type = "class")
train_accuracy <- mean(train_predictions == train_data_after_smote$isFraud)

test_predictions <- predict(model_nn, data_numeric_test, type = "class")
test_accuracy <- mean(test_predictions == data_numeric_test$isFraud)

#cat("TrainSet acc:", train_accuracy, "\n")
cat("TestSet acc:", test_accuracy, "\n")

train_predictions <- factor(train_predictions, levels = levels(train_data_after_smote$isFraud))
test_predictions <- factor(test_predictions, levels = levels(data_numeric_test$isFraud))


print(confusionMatrix(test_predictions, data_numeric_test$isFraud))

#nn_2##############################
start_time_nn2 <- Sys.time()
model_nn2 <- nnet(isFraud ~ ., data = train_data_after_smote, maxit = 400, size = 5, decay = 0.1)
end_time_nn2 <- Sys.time()
time_taken_nn2 <- end_time_nn2 - start_time_nn2
print(paste("Elapsed time of Neural Network 2:", time_taken_nn2))

train_predictions_nn2 <- predict(model_nn2, train_data_after_smote, type = "class")
train_accuracy_nn2 <- mean(train_predictions_nn2 == train_data_after_smote$isFraud)
test_predictions_nn2 <- predict(model_nn2, data_numeric_test, type = "class")
test_accuracy_nn2 <- mean(test_predictions_nn2 == data_numeric_test$isFraud)

cat("TrainSet acc:", train_accuracy_nn2, "\n")
cat("TestSet acc:", test_accuracy_nn2, "\n")

train_predictions_nn2 <- factor(train_predictions_nn2, levels = levels(train_data_after_smote$isFraud))
test_predictions_nn2 <- factor(test_predictions_nn2, levels = levels(data_numeric_test$isFraud))


#print(confusionMatrix(train_predictions_nn2, train_data_after_smote$isFraud))

print(confusionMatrix(test_predictions_nn2, data_numeric_test$isFraud))

#nn_3##############################
epochs <- 4
train_accuracy_nn3 <- numeric(epochs)
test_accuracy_nn3 <- numeric(epochs)


start_time_nn3 <- Sys.time()
for (epoch in 1:epochs) {
  maxit_value_3 <- epoch * 100  
  model_nn3 <- nnet(isFraud ~ ., data = train_data_after_smote, maxit = maxit_value_3, size = 6, decay = 0.1, trace = FALSE)
  
  train_predictions_nn3 <- predict(model_nn3, train_data_after_smote, type = "class")
  train_accuracy_nn3[epoch] <- mean(train_predictions_nn3 == train_data_after_smote$isFraud)
  
  test_predictions_nn3 <- predict(model_nn3, data_numeric_test, type = "class")
  test_accuracy_nn3[epoch] <- mean(test_predictions_nn3 == data_numeric_test$isFraud)
  cat("Epoch:", epoch, "- Train Accuracy:", train_accuracy_nn3[epoch], "- Test Accuracy:", test_accuracy_nn3[epoch], "\n")
}
end_time_nn3 <- Sys.time()
time_taken_nn3 <- end_time_nn3 - start_time_nn3
print(paste("Elapsed time of Neural Network 3:", time_taken_nn3))

accuracy_data_nn3 <- data.frame(
  Epoch = 1:epochs,
  TrainAccuracy_nn3 = train_accuracy_nn3,
  TestAccuracy_nn3 = test_accuracy_nn3
)

ggplot(accuracy_data_nn3, aes(x = Epoch)) +
  geom_line(aes(y = TrainAccuracy_nn3, color = "Train Accuracy")) +
  geom_line(aes(y = TestAccuracy_nn3, color = "Test Accuracy")) +
  labs(title = "Accuracy over Epochs",
       x = "Epoch",
       y = "Accuracy",
       color = "NN3") +
  theme_minimal()

final_train_accuracy_nn3 <- train_accuracy_nn3[epochs]
final_test_accuracy_nn3 <- test_accuracy_nn3[epochs]

#cat("Final Train Accuracy:", final_train_accuracy_nn3, "\n")
cat("Final Test Accuracy:", final_test_accuracy_nn3, "\n")

#final_train_predictions_nn3 <- predict(model_nn3, train_data_after_smote, type = "class")
final_test_predictions_nn3 <- predict(model_nn3, data_numeric_test, type = "class")

final_train_predictions_nn3 <- factor(final_train_predictions_nn3, levels = levels(train_data_after_smote$isFraud))
final_test_predictions_nn3 <- factor(final_test_predictions_nn3, levels = levels(data_numeric_test$isFraud))
cat("Final Test Confusion Matrix:\n")
test_confusion_matrix_nn3 <- confusionMatrix(final_test_predictions_nn3, data_numeric_test$isFraud)
print(test_confusion_matrix_nn3)


#nn_4##############################
epochs <- 10
train_accuracy_nn4 <- numeric(epochs)
test_accuracy_nn4 <- numeric(epochs)

start_time_nn4 <- Sys.time()
for(epoch in 1:epochs) {
  maxit_value <- epoch * 100
  
  model_nn4 <- nnet(isFraud ~ ., data = train_data_after_smote, maxit = maxit_value, size = 6, decay = 0.1)
  
  train_predictions_nn4 <- predict(model_nn4, train_data_after_smote, type = "class")
  train_accuracy_nn4[epoch] <- mean(train_predictions_nn4 == train_data_after_smote$isFraud)
  
  test_predictions_nn4 <- predict(model_nn4, data_numeric_test, type = "class")
  test_accuracy_nn4[epoch] <- mean(test_predictions_nn4 == data_numeric_test$isFraud)
  
  cat("Epoch:", epoch, "- Train Accuracy:", train_accuracy_nn4[epoch], "- Test Accuracy:", test_accuracy_nn4[epoch], "\n")
}
end_time_nn4 <- Sys.time()
time_taken_nn4 <- end_time_nn4 - start_time_nn4
print(paste("Elapsed time of Neural Network 4:", time_taken_nn4))


accuracy_data_nn4 <- data.frame(
  Epoch = 1:epochs,
  TrainAccuracy_nn4 = train_accuracy_nn4,
  TestAccuracy_nn4 = test_accuracy_nn4
)


ggplot(accuracy_data_nn4, aes(x = Epoch)) +
  geom_line(aes(y = TrainAccuracy_nn4, color = "Train Accuracy")) +
  geom_line(aes(y = TestAccuracy_nn4, color = "Test Accuracy")) +
  labs(title = "Accuracy over Epochs",
       x = "Epoch",
       y = "Accuracy",
       color = "NN4") +
  theme_minimal()


final_test_predictions_nn4 <- factor(predict(model_nn4, data_numeric_test, type = "class"), levels = c(0, 1))
actual_values <- factor(data_numeric_test$isFraud, levels = c(0, 1))

confusion_matrix_nn4 <- confusionMatrix(final_test_predictions_nn4, actual_values)
print(confusion_matrix_nn4)

###########################
####Logistic Regression####
set.seed(4444)
start_time_lr <- Sys.time()
model_logistic <- glm(isFraud ~ ., data = train_data_after_smote, family = binomial)
end_time_lr <- Sys.time()
time_taken_lr <- end_time_lr - start_time_lr
print(paste("Elapsed time of Logistic Regression:", time_taken_lr))

prediction_train_prob = predict(model_logistic, train_data_after_smote, type = "response")
prediction_train_class = ifelse(prediction_train_prob > 0.5, 1, 0)
accuracy_lr_train = mean(prediction_train_class == train_data_after_smote$isFraud)

prediction_prob = predict(model_logistic, data_numeric_test, type = "response")
prediction_class = ifelse(prediction_prob > 0.5, 1, 0)
accuracy_lr_test = mean(prediction_class == data_numeric_test$isFraud)

print(paste("TrainSet acc:", accuracy_lr_train))
print(paste("TestSet acc:", accuracy_lr_test))

confusion_matrix_lr = table(data_numeric_test$isFraud, prediction_class)
print(confusion_matrix_lr)
confusionMatrix(confusion_matrix_lr)

accuracy_data <- data.frame(
  Dataset = c("Train", "Test"),
  Accuracy = c(accuracy_lr_train, accuracy_lr_test)
)

ggplot(accuracy_data, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", width = 0.2) +
  ylim(0, 1) +
  labs(title = "Accuracy of Logistic Regression", x = "Data Set", y = "Accuracy") +
  theme_minimal()


###########################
####Decision Tree####
set.seed(5555)
start_time_tree <- Sys.time()
model_tree <- rpart(isFraud ~ ., data = train_data_after_smote, method = "class")
end_time_tree <- Sys.time()
time_taken_tree <- end_time_tree - start_time_tree
print(paste("Elapsed time of Decision Tree:", time_taken_tree))

prediction_train_prob <- predict(model_tree, train_data_after_smote, type = "prob")[,2]
prediction_train_class <- ifelse(prediction_train_prob > 0.5, 1, 0)
accuracy_tree_train <- mean(prediction_train_class == train_data_after_smote$isFraud)

prediction_prob <- predict(model_tree, data_numeric_test, type = "prob")[,2]
prediction_class <- ifelse(prediction_prob > 0.5, 1, 0)
accuracy_tree_test <- mean(prediction_class == data_numeric_test$isFraud)

print(paste("TrainSet acc:", accuracy_tree_train))
print(paste("TestSet acc:", accuracy_tree_test))

confusion_matrix_tree <- table(data_numeric_test$isFraud, prediction_class)
print(confusion_matrix_tree)
confusionMatrix(confusion_matrix_tree)

nn_prob <- predict(model_nn3, data_numeric_test, type = "raw")
rf_prob <- predict(model_rf, data_numeric_test, type = "prob")[,2]
lr_prob <- predict(model_logistic, data_numeric_test, type = "response")
dt_prob <- predict(model_tree, data_numeric_test, type = "prob")[,2]


pred_nn <- prediction(nn_prob, data_numeric_test$isFraud)
pred_rf <- prediction(rf_prob, data_numeric_test$isFraud)
pred_lr <- prediction(lr_prob, data_numeric_test$isFraud)
pred_dt <- prediction(dt_prob, data_numeric_test$isFraud)

perf_nn <- performance(pred_nn, "tpr", "fpr")
perf_rf <- performance(pred_rf, "tpr", "fpr")
perf_lr <- performance(pred_lr, "tpr", "fpr")
perf_dt <- performance(pred_dt, "tpr", "fpr")


auc_nn <- performance(pred_nn, "auc")@y.values[[1]]
auc_rf <- performance(pred_rf, "auc")@y.values[[1]]
auc_lr <- performance(pred_lr, "auc")@y.values[[1]]
auc_dt <- performance(pred_dt, "auc")@y.values[[1]]


plot(perf_nn, col = "blue", main = "ROC Curves for Four Models", lwd = 2)
plot(perf_rf, col = "red", add = TRUE, lwd = 2)
plot(perf_lr, col = "green", add = TRUE, lwd = 2)
plot(perf_dt, col = "purple", add = TRUE, lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = c(paste("NN (AUC =", round(auc_nn, 2), ")"),
                                 paste("RF (AUC =", round(auc_rf, 2), ")"),
                                 paste("LR (AUC =", round(auc_lr, 2), ")"),
                                 paste("DT (AUC =", round(auc_dt, 2), ")")),
       col = c("blue", "red", "green", "purple"), lwd = 2)

# Final model and evaluation result

model_best = randomForest(isFraud ~ ., data=train_data_after_smote,mtry = 33, ntree=500, importance = TRUE) 
prediction_best = predict(model_best,TestSet)

prediction_best <- factor(prediction_best, levels = c("0", "1"))
TestSet$isFraud <- factor(TestSet$isFraud, levels = c("0", "1"))
prediction_best <- factor(prediction_best)
TestSet$isFraud <- factor(TestSet$isFraud)
sum(is.na(prediction_best))
sum(is.na(TestSet$isFraud))
levels(prediction_best)
levels(TestSet$isFraud)

confusionMatrix(prediction_best,TestSet$isFraud)


print("done!!!")
