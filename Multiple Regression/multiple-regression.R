# Importing Data
data <- read.csv ("insurance.csv")
data <- unique(data)
data$expenses <- log(data$expenses)

set.seed(1)

# Multiple Regression
model <- lm (expenses ~ age + sex + bmi + children + smoker + region, data = data)
summary (model)

# Install Package of Plotting
#install.packages("ggplot2")

# Hot Encoding
#install.packages("caret")
library(caret)
dmy <- dummyVars(" ~ .", data = data, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = data))
dat_transformed


# Multiple Regression with Hot Encoding Variables
model <- lm (expenses ~ age + sexmale + bmi + children + smokeryes + regionnorthwest + regionsoutheast + regionsouthwest, data = dat_transformed)
summary (model)
par(mfrow = c(2, 2))
plot(model)

# Removing Outliers
cooksD <- cooks.distance(model)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Creating Newdataset
library(magrittr)
library (dplyr)
names_of_influential <- names(influential)
outliers <- dat_transformed[names_of_influential,]
dat_transformed_without_outliers <- dat_transformed %>% anti_join(outliers)

# Multiple Regression After Outlier Deletion

dt = sort(sample(nrow(dat_transformed_without_outliers), nrow(dat_transformed_without_outliers)*.75))
train<-dat_transformed_without_outliers[dt,]
test<-dat_transformed_without_outliers[-dt,]

model2 <- lm(expenses ~ ., data = train)
summary(model2)

# Histogram and Normal Q-Q
library(ggplot2)
hist(rstandard(model2))
plot(model2, which=2)

plot(model2, which=3)

predict(model2, newdata = test)

plot_data <- data.frame(Predicted_value = predict(model2, newdata = test),  
                        Observed_value = test$expenses)

# plot predicted values and actual values
ggplot(plot_data, aes(x = Predicted_value, y = Observed_value)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green")

library(lmtest)
dwtest(model2)

library(caret)
vif(model2)
