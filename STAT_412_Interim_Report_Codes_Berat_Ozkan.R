library(dplyr)
library(tidyr)
library(naniar)
library(ggplot2)
library(GGally)
library(bestNormalize)
library(mice)
library(car)
library(LambertW)
library(lmtest)
library(glmnet)


data = read.csv("C:/Users/Lenovo/Desktop/STAT_412_Report/student_habits_performance_missing.csv")


#Data Cleaning and Tidying
str(data)

#Converting qualitative variables to factor.
data = data %>%
  mutate(
gender = as.factor(gender),
part_time_job = as.factor(part_time_job),
diet_quality = factor(diet_quality, levels = c("Poor", "Fair", "Good"), ordered = TRUE),
parental_education_level = factor(parental_education_level, 
                                      levels = c("None", "High School", "Bachelor", "Master"), 
                                      ordered = TRUE),
internet_quality = factor(internet_quality, levels = c("Poor", "Average", "Good"), ordered = TRUE) ,
extracurricular_participation = as.factor(extracurricular_participation)
)

#Remove "student_id" since useless for analysis
data = data[, !names(data) %in% "student_id"]

str(data)

#Check duplicated rows
sum(duplicated(data)) #There is no duplicated data




#EDA

#Descriptive statistic
summary(data)


#EDA
#Q1) How do study hours per day relate to exam scores?

ggplot(data, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_jitter() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relation Between Study Hours vs Exam Score", x = "Study Hours per Day", y = "Exam Score") + 
  theme_minimal()



#Q2) Do students with a part-time job perform differently than those without?
ggplot(data, aes(x = part_time_job, y = exam_score, fill = part_time_job)) +
  geom_boxplot() +
  labs(title = "Exam Scores by Part-Time Job Status", x = "Part-Time Job", y = "Exam Score") +
  theme_minimal()

#Q3) ??nternet kalitesine g??re online zaman (Netflix veya sosyal medya) ile s??nav ba??ar??s?? aras??nda nas??l bir ili??ki var?

ggplot(data, aes(x = social_media_hours, y = exam_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +
  facet_wrap(~ internet_quality) +
  labs(
    title = "Effect of Social Media Usage on Exam Score by Internet Quality",
    x = "Social Media Hours per Day",
    y = "Exam Score"
  ) +
  theme_minimal()




#Q4) what is distribution of exam score?
ggplot(data, aes(x = exam_score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7, aes(y = ..density..)) +
  geom_density(color = "red", size = 1) + 
  labs(title = "Distribution of Exam Scores", x = "Exam Score", y = "Density") +
  theme_minimal()





#Q5) Ebeveynlerin e??itim seviyesine g??re s??nav notu nas??l de??i??iyor.
ggplot(data, aes(x = parental_education_level, y = exam_score)) +
  geom_violin(fill = "purple", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.2) +
  labs(
    title = "Distribution of Exam Scores by Parental Education Level",
    x = "Parental Education Level",
    y = "Exam Score"
  ) +
  theme_minimal()



#Q6) What are the relationships between numeric variables in my data?
numeric_vars = data %>%
  select(where(is.numeric)) 
  
# Scatter plot matrisi olu??tur
ggpairs(numeric_vars,
        title = "Matrix Scatter Plot of Numerical Variables")





#Missingnes mechanism
vis_miss(data)   
#There is no any pattern so my missingness mechanism is MCAR

#Imputaion of data
imputed_data = mice(data, seed = 123)

# View summary of imputations
summary(imputed_data)

#Imputed data
data_completed = complete(imputed_data, 1)
summary(data_completed)


#Data Manipulation and Feature Engineering


#Converting the age variable into a factor by dividing it into 3 equal groups
age_breaks = quantile(data_completed$age, probs = seq(0, 1, length.out = 4), na.rm = TRUE)

data_completed$age_group = cut(data_completed$age,
                                breaks = age_breaks,
                                include.lowest = TRUE,
                                labels = paste0(round(age_breaks[-4]), "-", round(age_breaks[-1])))
summary(data_completed$age_group)


#Calculating school engagement score by extracuricular participation and attendance percentage

data_completed$binary_scores = ifelse(data_completed$extracurricular_participation == "Yes", 1, 0)

# Scole engagement score is scaled between 1-10
data_completed$school_engagement_score = ((data_completed$attendance_percentage / 100) * 0.7 + data_completed$binary_scores * 0.3) * 10

summary(data_completed$school_engagement_score)


#Removing outliers
numeric_vars = sapply(data_completed, is.numeric)

non_outlier_rows = rep(TRUE, nrow(data_completed))

#Founding outlier observations according to variables
for (var in names(data_completed)[numeric_vars]) {
  Q1 = quantile(data_completed[[var]], 0.25)
  Q3 = quantile(data_completed[[var]], 0.75)
  IQR_value = Q3 - Q1
     
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value
       
  outliers = data_completed[[var]] < lower_bound | data_completed[[var]] > upper_bound
  outlier_count = sum(outliers)
         
  cat(paste0(var, ": ", outlier_count, " outliers\n"))
   }



#Removing outlier observations from data
for (var in names(data_completed)[numeric_vars]) {
  Q1 = quantile(data_completed[[var]], 0.25)
  Q3 = quantile(data_completed[[var]], 0.75)
  IQR_value = Q3 - Q1
  
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value
  
  non_outlier_rows = non_outlier_rows & 
    (data_completed[[var]] >= lower_bound & data_completed[[var]] <= upper_bound)
}

data_completed_and_cleaned = data_completed[non_outlier_rows, ]

summary(data_completed_and_cleaned)


#Conformitory Data Analaysis

#??s exam_score distrubited normal?

qqnorm(data_completed_and_cleaned$exam_score,main = "Q-Q Plot of Exam Score",pch = 19)
qqline(data_completed_and_cleaned$exam_score,col = "red",lwd = 2)

shapiro.test(data_completed_and_cleaned$exam_score)

#Transformation to make normal


bestNormalize(data_completed_and_cleaned$exam_score)

#boxcox transformation
bc = boxcox(data_completed_and_cleaned$exam_score~1)
lambda = bc$x[which.max(bc$y)]
lambda

boxcox.var = data_completed_and_cleaned$exam_score^lambda

data_completed_and_cleaned = cbind(data_completed_and_cleaned, boxcox.var)

data_completed_and_cleaned %>% 
  ggplot(aes(x = boxcox.var)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(color = 'red',lwd = 1) + 
  theme_minimal()

shapiro.test(data_completed_and_cleaned$boxcox.var) #It is still non-normal

#Try another transformation methods

shapiro.test(log(data_completed_and_cleaned$exam_score)) #log transform still non-normal

shapiro.test(sqrt(data_completed_and_cleaned$exam_score)) #Square root transform still non-normal

shapiro.test(scale(data_completed_and_cleaned$exam_score)) #Scale, still non - normal 


#Gaussanize transform
install.packages("LambertW")
library(LambertW)
lambert_result = Gaussianize(data_completed_and_cleaned$exam_score, type = "h")

data_completed_and_cleaned$exam_score_lambert = lambert_result

qqnorm(data_completed_and_cleaned$exam_score_lambert,
       main = "Q-Q Plot of Lambert Gaussianized Exam Score",
       col = "blue", pch = 19)
qqline(data_completed_and_cleaned$exam_score_lambert, col = "red", lwd = 2)

shapiro.test(data_completed_and_cleaned$exam_score_lambert) #Gaussanize transform still non-normal


#Remove transformation columns from data
data_completed_and_cleaned = data_completed_and_cleaned[, !names(data_completed_and_cleaned) %in% 
                                                          c("boxcox.var","exam_score_lambert")]





#Do students with a part-time job perform differently than those without?

shapiro.test(data_completed_and_cleaned$exam_score[data_completed_and_cleaned$part_time_job == "Yes"])
shapiro.test(data_completed_and_cleaned$exam_score[data_completed_and_cleaned$part_time_job == "No"])


wilcox.test(exam_score ~ part_time_job, data = data_completed_and_cleaned)


#How do exam scores change depending on parents' education level?

data_completed_and_cleaned %>%
  group_by(parental_education_level) %>%
  summarise(p_value = shapiro.test(exam_score)$p.value)


kruskal.test(exam_score ~ parental_education_level, data = data_completed_and_cleaned)


#Cross-validation , Train Test Split and Statistical Model

summary(data_completed_and_cleaned)

data_for_model = data_completed_and_cleaned[, !names(data_completed_and_cleaned) %in%
                                               c("age", "attendance_percentage", "extracurricular_participation",
                                                 "binary_scores")]

summary(data_for_model)


install.packages("caret")
library(caret)
set.seed(412) 

random_sample = createDataPartition(data_for_model$exam_score, p = 0.8, list = FALSE)


training_dataset = data_for_model[random_sample,]


testing_dataset = data_for_model[-random_sample,]


#Statistical model for Prediction
model = lm(exam_score ~. , data = training_dataset)


summary(model)

#Linearity Test
resettest(model)

#Heteroscedasticity Test
bptest(model)

#Residual Normality Test
shapiro.test(model$residuals)

#Multicollinearity Test
vif(model)


par(mfrow=c(2,2))

plot(model)


model_to_detect = lm(exam_score ~. , data = data_for_model)

#Removing observations which predicted exam score higher than 100
model_to_detect$fitted.values
fitted_vals <- fitted(model_to_detect)
sum(fitted_vals > 100)
valid_idx <- which(fitted_vals <= 100)
data_for_model_cleaned <- data_for_model[valid_idx, ]



#New train and test data set with cleaned data
set.seed(412) 

random_sample = createDataPartition(data_for_model_cleaned$exam_score, p = 0.8, list = FALSE)


training_dataset_cleaned = data_for_model_cleaned[random_sample,]


testing_dataset_cleaned = data_for_model_cleaned[-random_sample,]



cleaned_model = lm(exam_score ~. , data = training_dataset_cleaned)

summary(cleaned_model)

par(mfrow=c(2,2))

plot(cleaned_model)

#Linearity Test
resettest(cleaned_model)

#Heteroscedasticity Test
bptest(cleaned_model)

#Residual Normality Test
shapiro.test(cleaned_model$residuals)

#Multicollinearity Test
vif(cleaned_model)

#AIC score
AIC(cleaned_model)


# Backward elimination 
backward_model <- step(cleaned_model, direction = "backward")

summary(backward_model)

#AIC score
AIC(backward_model)


# Testing

pred = backward_model %>%  predict(testing_dataset_cleaned)
head(pred) 

metrics = data.frame(RMSE = RMSE(pred, testing_dataset_cleaned$exam_score),
                     Rsquared = R2(pred, testing_dataset_cleaned$exam_score),
                     MAE = MAE(pred, testing_dataset_cleaned$exam_score))
metrics


