install.packages("nnet")
install.packages("tidyverse")
install.packages("caret")
install.packages("ordinal")
library(tidyverse)
library(caret)
library(nnet)
library(ordinal)
library(ggplot2)
data <- read.csv("Sleep_Efficiency.csv")
age<- data[,2]
gender<- data[,3]
Bedtime<- data[,4]
Wakeup_time<- data[,5]
sleep_duration<- data[,6]
sleep_efficiency<- data[,7]
REM_sleep<- data[,8]
#print(age)
deep_sleep<- data[,9]
light_sleep<- data[,10]
Awakenings<- data[,11]
Caffeine_consumption<- data[,12]
Alcohol_consumption<- data[,13]
Smoking_status<- data[,14]
Exercise_frequency<- data[,15]
Gender_coded<- data[,16]
Smoke_coded<- data[,17]
summary(age)
summary(gender)
summary(Bedtime)
summary(Wakeup_time)
summary(sleep_duration)
summary(sleep_efficiency)
summary(REM_sleep)
summary(deep_sleep)
summary(light_sleep)
summary(Awakenings)
summary(Caffeine_consumption)
summary(Alcohol_consumption)
summary(Smoking_status)
summary(Exercise_frequency)
hist(age, col = "blue")
plot(age, col = "blue")
hist(Gender_coded, col = "blue")
plot(Gender_coded, col = "blue")
hist(Bedtime, col = "blue")
plot(Bedtime, col = "blue")
hist(Wakeup_time, col = "blue")
plot(Wakeup_time, col = "blue")
hist(sleep_duration, col = "blue")
plot(sleep_duration, col = "blue")
hist(sleep_efficiency, col = "blue")
plot(sleep_efficiency, col = "blue")
hist(REM_sleep, col = "blue")
plot(REM_sleep, col = "blue")
hist(deep_sleep, col = "blue")
plot(deep_sleep, col = "blue")
hist(light_sleep, col = "blue")
plot(light_sleep, col = "blue")
hist(Awakenings, col = "blue")
plot(Awakenings, col = "blue")
hist(Caffeine_consumption, col = "blue")
plot(Caffeine_consumption, col = "blue")
hist(Alcohol_consumption, col = "blue")
plot(Alcohol_consumption, col = "blue")
hist(Smoke_coded, col = "blue")
plot(Smoke_coded, col = "blue")
hist(Exercise_frequency, col = "blue")
plot(Exercise_frequency, col = "blue")
#models
mod1=lm(age~sleep_efficiency)#regression between age and sleep efficiency
summary(mod1)
# Subset the data by gender
male_data <- subset(data, Gender == "Male")
#print(male_data)
female_data <- subset(data, Gender == "Female")

# Calculate the mean and standard deviation of deep sleep percentage for each group
male_mean <- mean(male_data$Deep.sleep.percentage)
male_sd <- sd(male_data$Deep.sleep.percentage)
female_mean <- mean(female_data$Deep.sleep.percentage)
female_sd <- sd(female_data$Deep.sleep.percentage)
#t test
t_test_result <- t.test(male_data$Deep.sleep.percentage, female_data$Deep.sleep.percentage, var.equal = TRUE)

# Print the t-test results
print(t_test_result)

mod2=lm(Caffeine_consumption~Awakenings)#regression between the number of awakenings and caffeine consumption
summary(mod2)
mod3=lm(sleep_duration~Exercise_frequency)#regression between the exercise frequency and sleep duration
summary(mod3)
mod4=lm(sleep_duration~Caffeine_consumption+Alcohol_consumption)#regression between caffeine cons, alcohol cons, and sleep duration
summary(mod4)

# Subset the data by smoking status
REM_smoke <- subset(data, Smoke == "1")
#print(REM_smoke)
REM_No_smoke <- subset(data, Smoke == "0")

# Calculate the mean and standard deviation of REM sleep percentage for each group
REM_smoke_mean <- mean(REM_smoke$REM.sleep.percentage)
REM_smoke_sd <- sd(REM_smoke$REM.sleep.percentage)
REM_No_smoke_mean <- mean(REM_No_smoke$REM.sleep.percentage)
REM_No_smoke_sd <- sd(REM_No_smoke$REM.sleep.percentage)
#t test
t_test_result1 <- t.test(REM_smoke$REM.sleep.percentage, REM_No_smoke$REM.sleep.percentage, var.equal = TRUE)

# Print the t-test results
print(t_test_result1)
mean_bedtime=mean(Bedtime)
mean_wakeup_time=mean(Wakeup_time)
print(mean_bedtime)
print(mean_wakeup_time)

mod5=lm(age~Awakenings)#regression between age and the number of awakenings
summary(mod5)
mod6=lm(sleep_efficiency~Exercise_frequency)#regression between sleep efficiency and exercise frequency
summary(mod6)
mod7=lm(REM_sleep~deep_sleep+light_sleep)#regression between REM sleep percentage, deep sleep percentage, and light sleep percentage
summary(mod7)
