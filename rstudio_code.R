library(tidyverse)

df <- read.csv("C:/Users/navee/Desktop/Everything/Professional/DATA SCIENCE/Classwork/final-project/rstudio.csv")
View(df)
glimpse(df)

table(df$Diabetes_binary)
table(df$Age)

df$Age <- ifelse(df$Age == 1, "18-24", df$Age)
df$Age <- ifelse(df$Age == 2, "25-29", df$Age)
df$Age <- ifelse(df$Age == 3, "30-34", df$Age)
df$Age <- ifelse(df$Age == 4, "35-39", df$Age)
df$Age <- ifelse(df$Age == 5, "40-44", df$Age)
df$Age <- ifelse(df$Age == 6, "45-49", df$Age)
df$Age <- ifelse(df$Age == 7, "50-54", df$Age)
df$Age <- ifelse(df$Age == 8, "55-59", df$Age)
df$Age <- ifelse(df$Age == 9, "60-64", df$Age)
df$Age <- ifelse(df$Age == 10, "65-69", df$Age)
df$Age <- ifelse(df$Age == 11, "70-74", df$Age)
df$Age <- ifelse(df$Age == 12, "75-79", df$Age)
df$Age <- ifelse(df$Age == 13, "80+", df$Age)

df$count <- 1

df %>% 
  filter(Diabetes_binary == 0) %>% 
  select(Age|count) %>% 
  group_by(Age) %>% 
  summarise(total = sum(count)) -> age_no_diabetes
View(age_no_diabetes)

df %>% 
  filter(Diabetes_binary == 1) %>% 
  select(Age|count) %>% 
  group_by(Age) %>% 
  summarise(total = sum(count)) -> age_yes_diabetes
View(age_yes_diabetes)

df %>% 
  filter(Diabetes_binary == 0) -> no_diabetes

df %>% 
  filter(Diabetes_binary == 1) -> yes_diabetes

names(df)

df %>%
  ggplot(aes(Age))+
  geom_bar(col = 'darkblue', fill = 'lightblue')+
  theme_bw()+
  xlab("Participant Age")+
  ylab("Count")+
  ggtitle("Age Distribution of Survey Participants")

no_diabetes %>%
  ggplot(aes(Age))+
  geom_bar(col = 'darkblue', fill = 'lightblue')+
  theme_bw()+
  xlab("Participant Age")+
  ylab("Count")+
  ggtitle("Age Distribution of People without Diabetes")

yes_diabetes %>%
  ggplot(aes(Age))+
  geom_bar(col = 'darkblue', fill = 'lightblue')+
  theme_bw()+
  xlab("Participant Age")+
  ylab("Count")+
  ggtitle("Age Distribution of People with Diabetes")

df %>% 
  select(Age|count) %>% 
  group_by(Age) %>% 
  summarise(total = sum(count))-> total_number_of_people_in_each_age_group
df %>% 
  filter(Diabetes_binary == 1) %>% 
  select(Age|count) %>% 
  group_by(Age) %>% 
  summarise(total_diabetes = sum(count))-> total_number_of_people_with_diabetes_in_each_age_group
View(total_number_of_people_in_each_age_group)
View(total_number_of_people_with_diabetes_in_each_age_group)

merged_df <- merge(total_number_of_people_in_each_age_group, total_number_of_people_with_diabetes_in_each_age_group, by = "Age")
View(merged_df)

merged_df$pct_with_diabetes = ((merged_df$total_diabetes / merged_df$total) * 100)
sum(merged_df$pct_with_diabetes)

result <- df %>% 
  group_by(Age) %>% 
  summarise(pct_with_diabetes = (sum(Diabetes_binary == 1) / sum(count))*100)
View(result)
sum(result$pct_with_diabetes)

merged_df %>% 
  ggplot(aes(Age, pct_with_diabetes))+
  geom_point(aes(Age, pct_with_diabetes), size = 3)+
  theme_bw()+
  labs(x = "Participant Age", y = "Percent with Diabetes")+
  ggtitle("Percentage with Diabetes by Age")

model_data <- data.frame(model_no = 1:4,
                         training_accuracy = c(.7602, .7615, .7796, .8299),
                         testing_accuracy = c(.7506, .7497, .7294, .7062))

ggplot(data = model_data, aes(x = model_no)) +
  geom_line(aes(y = training_accuracy), color = "blue", linetype = "solid", size = 1, label = "Training Accuracy") +
  geom_line(aes(y = testing_accuracy), color = "red", linetype = "dashed", size = 1, label = "Testing Accuracy") +
  geom_point(aes(y = training_accuracy), color = "blue", size = 2, label = "Training Accuracy")+
  geom_point(aes(y = testing_accuracy), color = "red", size = 2, label = "Testing Accuracy")+
  labs(x = "Model Number", y = "Accuracy") +
  scale_x_continuous(breaks = model_data$model_no) +  # Ensure x-axis has all model numbers
  scale_y_continuous(limits = c(0.7062, 0.8299)) +  # Set y-axis limits
  theme_bw() +
  theme(legend.position = "top") +
  geom_text(data = model_data, aes(x = model_no, y = testing_accuracy, label = testing_accuracy), vjust = -1, hjust = 0, color = "red") +
  geom_text(data = model_data, aes(x = model_no, y = training_accuracy, label = training_accuracy), vjust = 1, hjust = 0, color = "blue")

df %>% 
  filter(Diabetes_binary == 0) -> bmi_no_diabetes

df %>% 
  filter(Diabetes_binary == 1) -> bmi_diabetes

mean(bmi_no_diabetes$BMI)
sd(bmi_no_diabetes$BMI)
qplot(bmi_no_diabetes$BMI)

mean(bmi_diabetes$BMI)
sd(bmi_diabetes$BMI)
qplot(bmi_diabetes$BMI)

t.test(BMI ~ Diabetes_binary, data = df, alternative = "less")

df %>% 
  ggplot(aes(BMI))+
  geom_histogram()+
  theme_bw()+
  facet_wrap(~ Diabetes_binary)

range(bmi_no_diabetes$BMI)

bmi_no_diabetes %>% 
  ggplot(aes(BMI))+
  geom_histogram(fill = "darkgreen", alpha = 0.75, bins = 50)+
  labs(x = "BMI", y = "Count")+
  ggtitle("BMI in respondents without Diabetes")+
  geom_vline(xintercept = 27.80577, color = "green", size = 1.5)+
  xlim(10, 75)+
  theme_bw()

range(bmi_diabetes$BMI)

bmi_diabetes %>% 
  ggplot(aes(BMI))+
  geom_histogram(fill = "darkred", alpha = 0.75, bins = 50)+
  labs(x = "BMI", y = "Count")+
  ggtitle("BMI in respondents with Diabetes")+
  geom_vline(xintercept = 31.94401, color = "red", size = 1.5)+
  xlim(10, 75)+
  theme_bw()