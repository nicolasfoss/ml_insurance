# Import required libraries

library(readr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(glue, quietly = TRUE)
library(yardstick, quietly = TRUE)
install.packages("naniar")
library(naniar, quietly = TRUE)

# Start coding!

# Read in and explore the dataset

car_insurance <- read_csv("car_insurance.csv")

glimpse(car_insurance)

paste0("--------------------------------------------------")

summary(car_insurance)

# Visualize missing values to assess for systematic or random missingness.
vis_miss(car_insurance)

# There are two columns with a preponderance of values.
# Let's examine their distribution to see what technique we need to use to 
# handle the missingness.

par(mfrow = c(1,2))
plot(density(car_insurance$credit_score, na.rm = TRUE), 
     col = "red", 
     lwd = 2,
     main = "Density Plot of Credit Score"
)
plot(density(car_insurance$annual_mileage, na.rm = TRUE), 
     col = "red", 
     lwd = 2,
     main = "Density Plot of Annual Mileage"
)

# Because the data are normally distributed in the columns that have missingness, we can use mean imputation through the mutate_at function.

car_insurance <- car_insurance %>% 
  mutate_at(c("credit_score", "annual_mileage"), mean, na.rm = TRUE)

glimpse(car_insurance)
summary(car_insurance)

# Check the distribution of these variables again.

par(mfrow = c(1,2))
plot(density(car_insurance$credit_score, na.rm = TRUE), 
     col = "red", 
     lwd = 2,
     main = "Density Plot of Credit Score"
)
plot(density(car_insurance$annual_mileage, na.rm = TRUE), 
     col = "red", 
     lwd = 2,
     main = "Density Plot of Annual Mileage"
)

# Prepare data to build the logistic regression models.

features_df <- data.frame(features = names(car_insurance)[-c(1,19)])

accuracy <- c()

for (var in features_df$features) {
  
  model <- glm(glue('outcome ~ {var}'), data = car_insurance, family = 'binomial')
  predictions <- round(fitted(model))
  accuracy <- mean(predictions == car_insurance$outcome)
  features_df[which(features_df$feature == var), "accuracy"] <- accuracy
  
}

features_df %>% arrange(desc(accuracy))

# Find the most predictive feature for creating a logistic regression model.

best_feature_df <- features_df %>% 
  filter(accuracy == max(accuracy)) %>% 
  rename(best_feature = features,
         best_accuracy = accuracy
  )

best_feature_df
