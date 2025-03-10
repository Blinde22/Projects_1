#ANCOVA - moderated regression (NBA PPG on diffrent positions moderated by age, and MPG)


#Libraries
install.packages("car")
install.packages("kableExtra")

library(lmtest)
library(car)
library(dplyr)
library(ggplot2)
library(kableExtra)



#Loading dataset
setwd("C:/Users/gusta/Desktop/NBA project")
nba_data <- read.csv("nba_data.csv", sep = ";")

#Structure
str(nba_data)
summary(nba_data)

#missing values
print("Missing values:")
print(colSums(is.na(nba_data)))        #No missing values

#EDA

# Descriptive statistics for key variables
desc_stats <- nba_data %>%
  select(PTS, MP) %>%
  summary()

desc_table <- kable(desc_stats, caption = "Descriptive Statistics") %>%
  kable_styling("striped", full_width = FALSE)

print(desc_table)


#distribution of key variables

#PPG
ggplot(nba_data, aes(x = PTS)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Distribution of PPG")

#MPG
ggplot(nba_data, aes(x = MP)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of MPG")

#relationships

#MPG&PPG
ggplot(nba_data, aes(x = MP, y = PTS)) +
  geom_point() +
  labs(title = "Scatterplot of MPG vs. PPG")

#Unique values in Position column

print("Unique values in Position:")
print(unique(nba_data$Pos))

#Selecting specific positions
nba_subset <- nba_data[nba_data$Pos %in% c("PG", "SG", "SF", "PF","C"), ]
nba_subset <- nba_data[nba_data$Pos %in% c("PG", "SF"), ]

#Fitting the model
mod_reg_model <- lm(PTS ~ Pos * MP, data = nba_subset)

#Assumptions

#Linearity
ggplot(nba_data, aes(x = MP, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of PPG vs. MPG", x = "MPG", y = "PPG")

#Indepandance of residuals
plot(mod_reg_model, which = 1)

#Homoscedasticity
plot(mod_reg_model, which = 3)

#Normality of Residuals
plot(mod_reg_model, which = 2)
shapiro.test(residuals(mod_reg_model))

#No Perfect Multicollinearity
vif(mod_reg_model)
alias(mod_reg_model)

#Homogeneity of Regression Slopes
coplot(PTS ~ MP | Pos, data = nba_subset)

waldtest(mod_reg_model, terms = c("Pos:MP"))

#Correlation Analysis
str(nba_subset)
encoded_pos <- model.matrix(~ Pos - 1, data = nba_subset)
numeric_columns <- nba_subset[, c("PTS", "MP", "FG.", "FG", "FT")]
cor_matrix <- cor(cbind(numeric_columns, encoded_pos), use = "complete.obs")
print(cor_matrix)   

#Interpretation of Results
summary(mod_reg_model)


#Results vizualization


ggplot(nba_subset, aes(x = MP, y = PTS, color = Pos)) +
  geom_line() +
  geom_point() +
  labs(title = "Interaction Plot of PTS and MP by Pos",
       x = "Average Minutes Played per Game (MP)",
       y = "Points Per Game (PTS)",
       color = "Playing Position (Pos)")


ggplot(nba_subset, aes(x = MP, y = PTS, color = Pos)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Pos, scales = "free") +
  labs(title = "Conditional Plots", x = "MP", y = "PTS")

