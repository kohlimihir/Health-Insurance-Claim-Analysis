library(tidyverse)
library(caret)
library(scales)

data<-read.csv("insurance_data.csv")

head(data)

sapply(data,function(x)sum(x == "" | is.na(x)))

data<-data%>%filter(region!="")%>%mutate(age = case_when(is.na(age) ~ median(.$age, na.rm=TRUE),TRUE ~ age))

print("The number of duplicates is :")
print(nrow(unique(data)) - nrow(data))

data$age_bin <-cut(data$age, breaks = c(0, 18, 35, 55, Inf), 
            labels = c("Children", "Young Adults", "Adults", "Seniors"), 
            include.lowest = TRUE, 
            right = FALSE)

head(data)

data$bmi_category <- cut(data$bmi, 
                       breaks = c(0, 18.5, 24.9, 30, Inf), 
                       labels = c("Underweight", "Normal weight", "Overweight", "Obese"), 
                       include.lowest = TRUE, 
                       right = TRUE)

head(data)

print("The statistical summary of the dataset:")
print(summary(data[,3:ncol(data)]))

cat("\nNumber of rows: ",nrow(data),"\n")
cat("\nNumber of columns: ",ncol(data),"\n")

cat("\nColumn names:")
print(colnames(data))

cat("\nThe mean age of the dataset is",round(mean(data$age, na.rm = TRUE),0))

data%>% 
  ggplot(aes(x = gender, y = claim, fill = gender)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by Gender", x = "Gender", 
       y = "Claim Amount", fill = "Gender") +
  theme_classic()


gender_claim_summary_data <- data %>%
  group_by(gender) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

gender_claim_summary_data %>%
  ggplot(aes(x=gender, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=gender)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Total by Gender", x = "Gender", 
       y = "Claim Total", fill = "Gender") +
  theme_classic()

print("We can observe the male patients have higher claim amounts than female patients.")

data %>% 
  ggplot(aes(x=age)) +
  geom_bar(fill = "purple", alpha = 0.75) +
  labs(title = "Distribution of Age in Claims Data",
       x = "Age", y = NULL) +
  theme_classic()

age_claim_summary_data <- data %>%
  group_by(age_bin) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

age_claim_summary_data %>%
  ggplot(aes(x=age_bin, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=age_bin)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Total by Age Group", x ="Age Group",
       y = "Claim Total", fill = "Age Group") +
  theme_classic()

data %>% 
  ggplot(aes(x=age_bin,y=claim)) + 
  geom_boxplot(aes(fill = age_bin)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by Age Group", x ="Age Group",
       y = "Claim Amount", fill = "Age Group") +
  theme_classic()

print("We can observe higher claims of Young Adults and Adults than Seniors, but the distribution of the claim amounts seems to 
       be similar between the age groups.")

bmi_claim_summary_data <- data %>%
  group_by(bmi_category) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

bmi_claim_summary_data %>%
  ggplot(aes(x=bmi_category, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=bmi_category)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Amount by BMI Category", x ="BMI Category",
       y = "Claim Total", fill = "BMI Category") +
  theme_classic()

data %>% 
  ggplot(aes(x = claim, y = bmi, color = gender)) + 
  geom_point(alpha = 0.5, size = 3) + 
  scale_x_continuous(labels = comma) +
  labs(title = "BMI vs. Claim Amount", x = "Claim",  y = "BMI")+
  theme_classic()

data %>% 
  ggplot(aes(x=bmi_category,y=claim)) + 
  geom_boxplot(aes(fill = bmi_category)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by BMI Category", x ="BMI Category",
       y = "Claim Amount", fill = "BMI Category") +
  theme_classic()

print("We can observe higher claim amounts in Obese patients.")

smoker_claim_summary_data <- data %>%
  group_by(smoker) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

smoker_claim_summary_data %>%
  ggplot(aes(x=smoker, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=smoker)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Total by Smoker Status", x ="Smoker Status",
       y = "Claim Total", fill = "Smoker Status") +
  theme_classic()

data %>% 
  ggplot(aes(x=smoker,y=claim)) + 
  geom_boxplot(aes(fill = smoker)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by Smoker Status", x ="Smoker Status",
       y = "Claim Amount", fill = "Smoker Status") +
  theme_classic()

print("We can observe higher claim amounts in Smoker patients.")

diabetic_claim_summary_data <- data %>%
  group_by(diabetic) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

diabetic_claim_summary_data %>%
  ggplot(aes(x=diabetic, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=diabetic)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Total by Diabetic Status", x ="Diabetic Status",
       y = "Claim Total", fill = "Diabetic Status") +
  theme_classic()

data %>% 
  ggplot(aes(x=diabetic,y=claim)) + 
  geom_boxplot(aes(fill = diabetic)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by Diabetic Status", x ="Diabetic Status",
       y = "Claim Amount", fill = "Diabetic Status") +
  theme_classic()

print("We can observe that there is not significant difference in claim amounts of Diabetic and Non-Diabetic patients.")

region_claim_summary_data <- data %>%
  group_by(region) %>%
  summarise(claim_total = sum(claim, na.rm = TRUE))

region_claim_summary_data %>%
  ggplot(aes(x=region, y=claim_total)) +
  geom_bar(stat="identity", aes(fill=region)) + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=comma(claim_total)), vjust=-0.5) +
  labs(title = "Claim Total by Region", x ="Region",
       y = "Claim Total", fill = "Region") +
  theme_classic() 

data %>% 
  ggplot(aes(x=region,y=claim)) + 
  geom_boxplot(aes(fill = region)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Claim Distribution by Region", x ="Region",
       y = "Claim Amount", fill = "Region") +
  theme_classic()

print("We can observe higher claim amounts in patients from the norththeast region.")

gender_t_result <- t.test(data$claim[data$gender == 'female'], data$claim[data$gender == 'male'])
print(gender_t_result)

cat("Given the result of the T-Test and a p-value of",gender_t_result$p.value,", 
      we can conclude that there is a difference in claim amounts for each gender.")

smoker_t_result <- t.test(data$claim[data$smoker == 'Yes'], data$claim[data$smoker == 'No'])
print(smoker_t_result)
cat("Given the result of the T-Test and a p-value of",smoker_t_result$p.value,", 
     we can conclude that there is a difference in claim amounts for smokers.")

diabetic_t_result <- t.test(data$claim[data$diabetic == 'Yes'], data$claim[data$diabetic == 'No'])
print(diabetic_t_result)
cat("Given the result of the T-Test and a p-value of",diabetic_t_result$p.value,", 
     we can conclude that there is no difference in claim amounts for diabetic patients.")

bmi_anova_result <- aov(claim ~ bmi_category, data = data)
summary(bmi_anova_result)
bmi_tukey_posthoc <- TukeyHSD(bmi_anova_result, "bmi_category")
print(bmi_tukey_posthoc)
print("We can observe significant differences for the Obese BMI catgory")

region_anova_result <- aov(claim ~ region, data = data)
summary(region_anova_result)

region_anova_result <- aov(claim ~ region, data = data)
tukey_posthoc <- TukeyHSD(region_anova_result, "region")
print(tukey_posthoc)

print("We can observe significant differences for the northeast region.")

data.1<-data %>% 
  mutate(gender_male = ifelse(gender == "male", 1, 0),
         smoker_yes = ifelse(smoker == "Yes", 1, 0),
         diabetic_yes = ifelse(diabetic == "Yes", 1, 0))%>%
         unite("region", region) %>%
         spread(region, region, fill = 0)

head(data.1)

set.seed(123)

trainIndex <- createDataPartition(data.1$claim, p = 0.7, list = FALSE)
train_set <- data.1[trainIndex, ]
test_set <- data.1[-trainIndex, ]

mean_claim_train <- mean(train_set$claim)
baseline_predictions <- rep(mean_claim_train, length(test_set$claim))

residuals_baseline <- test_set$claim - baseline_predictions

mse_baseline <- mean(residuals_baseline^2)
rmse_baseline <- sqrt(mse_baseline)

print(paste("Baseline Mean Squared Error (MSE):", mse_baseline))
print(paste("Baseline Root Mean Squared Error (RMSE):", rmse_baseline))

model <- lm(claim ~ age + gender_male + bmi + smoker_yes + diabetic_yes + northeast + northwest + southeast, data = train_set)

model_summary <- summary(model)

print(paste("R-squared:", model_summary$r.squared))
print(paste("Adjusted R-squared:", model_summary$adj.r.squared))

test_pred <- predict(model, newdata = test_set)

mse <- mean((test_set$claim - test_pred)^2)
rmse <- sqrt(mse)

print(paste("Mean Squared Error (MSE) on Test Set:", mse))
print(paste("Root Mean Squared Error (RMSE) on Test Set:", rmse))
