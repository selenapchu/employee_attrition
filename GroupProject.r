#read data
employee.df <- read.csv("HR Employee Attrition.csv", stringsAsFactors = TRUE)
str(employee.df)

#Pre-process the data
anyNA(employee.df)

# Re-code the target variable to indicate which target value is associated with Class 1
employee.df$Attrition <- as.numeric(employee.df$Attrition == "Yes")

# We cannot use INT variables directly because it will affect the computation of coefficient
# transform them to factors
employee.df$Education <- factor(employee.df$Education)
employee.df$EnvironmentSatisfaction <- factor(employee.df$EnvironmentSatisfaction)
employee.df$JobInvolvement <- factor(employee.df$JobInvolvement)
employee.df$JobSatisfaction <- factor(employee.df$JobSatisfaction)
employee.df$PerformanceRating <- factor(employee.df$PerformanceRating)
employee.df$RelationshipSatisfaction <- factor(employee.df$RelationshipSatisfaction)
employee.df$WorkLifeBalance <- factor(employee.df$WorkLifeBalance)

levels(employee.df$Education)
levels(employee.df$EnvironmentSatisfaction)
levels(employee.df$JobInvolvement)
levels(employee.df$JobSatisfaction)
levels(employee.df$PerformanceRating)
levels(employee.df$RelationshipSatisfaction)
levels(employee.df$WorkLifeBalance)
levels(employee.df$BusinessTravel)
levels(employee.df$MaritalStatus)
employee.df$MaritalStatus <- relevel(employee.df$MaritalStatus, ref="Single")

# rename the levels in order
levels(employee.df$Education) <- c("Below College", "College", "Bachelor", "Master", "Doctor")
levels(employee.df$EnvironmentSatisfaction) <- c("Low", "Medium", "High", "Very High")
levels(employee.df$JobInvolvement) <- c("Low", "Medium", "High", "Very High")
levels(employee.df$JobSatisfaction) <- c("Low", "Medium", "High", "Very High")
levels(employee.df$PerformanceRating) <- c("Low", "Good", "Excellent", "Outstanding")
levels(employee.df$RelationshipSatisfaction) <- c("Low", "Medium", "High", "Very High")
levels(employee.df$WorkLifeBalance) <- c("Bad", "Good", "Better", "Best")

levels(employee.df$Education)
levels(employee.df$EnvironmentSatisfaction)
levels(employee.df$JobInvolvement)
levels(employee.df$JobSatisfaction)
levels(employee.df$PerformanceRating)
levels(employee.df$RelationshipSatisfaction)
levels(employee.df$WorkLifeBalance)

set.seed(1234)

# select variables
selected.var <- c(11,14,17,26,3,6,18,21,23,29,31,33,34,2)
selected.df <- employee.df[, selected.var]
str(selected.df)
#cor(employee.df$DistanceFromHome, employee.df$Attrition)
#cor(employee.df$NumCompaniesWorked, employee.df$Attrition)

# partition the data
train.index <- sample(1:nrow(employee.df), nrow(employee.df)*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
nrow(train.df)
nrow(valid.df)

#employee.df.lm <- lm(Attrition~., data = train.df)
#summary(employee.df.lm)

# Fit the logistic regression model
# run logistic model, and show coefficients 
logit.reg <- glm(Attrition ~ ., data = train.df, family = "binomial")
summary(logit.reg)

# Generate outcome by comparing predicted probability with the cutoff probability
# use predict() with type = "response" to compute predicted probabilities
# if type not specified, log-odds will be returned
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
#logit.reg.pred <- predict(logit.reg, train.df, type = "response")
logit.reg.pred
summary(logit.reg.pred)

# Base case: cutoff = 0.5 and it is a more unbias model
# Choose cutoff value and evaluate classification performance
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
pred

# In order to compute sensitivity and specificity
# we need to specify which class is the important class, by using positive = "1"

library(caret)
# Generate confusion matrix
confusionMatrix(factor(pred), factor(valid.df$Attrition), positive = "1")
#confusionMatrix(factor(pred), factor(train.df$Attrition), positive = "1")


# Generate ROC curve
library(pROC)

r <- roc(valid.df$Attrition, logit.reg.pred)
auc(r)
plot.roc(r)

# find the best threshold
coords(r, x = "best")




# Data Visualization
library(ggplot2)
# Bar Chart
ggplot(data=employee.df) + geom_bar(aes(x = YearsSinceLastPromotion, fill=factor(YearsSinceLastPromotion)), 
                                 stat = 'count') + 
  xlab("Years Since Last Promotion") + 
  ylab("Employee Count") +
  facet_grid(cols = vars(Attrition)) + 
  scale_fill_viridis_d("Years")

ggplot(data=employee.df) + geom_bar(aes(x = JobRole, y = MonthlyIncome, fill = JobRole), 
                                 stat = 'summary', fun = 'mean') + 
  xlab("Job Role") + theme(axis.text.x = element_text(angle = 60)) + 
  ylab("Average Monthly Income") +
  scale_fill_viridis_d()

ggplot(data=employee.df) + geom_bar(aes(x = JobRole, fill = Gender), 
                                 stat = 'count') + 
  xlab("Job Role") + theme(axis.text.x = element_text(angle = 60)) + 
  scale_fill_viridis_d()

ggplot(data=employee.df) + geom_bar(aes(x = Department, y = MonthlyIncome, fill = Department), 
                                 stat = 'summary', fun = 'mean') + 
  xlab("Department") + ylab("Average Monthly Income")

ggplot(data=employee.df) + geom_bar(aes(x = Department, fill = Gender), 
    stat = 'count') + 
    xlab("Department") +
    scale_fill_viridis_d()

ggplot(data = employee.df) + 
    geom_bar(aes(x = Age, fill = Attrition),
    stat = 'count') + 
    facet_grid(rows = vars(Gender)) +
    xlab("Age") + 
    scale_fill_viridis_d()

ggplot(data = employee.df) + 
    geom_bar(aes(x = Age, color = OverTime),
    stat = 'count') + 
    facet_grid(rows = vars(Gender)) +
    xlab("Age")

ggplot(data = employee.df) + 
    geom_bar(aes(x = Age, color = MaritalStatus),
    stat = 'count') + 
    facet_grid(rows = vars(Gender)) +
    xlab("Age")

ggplot(data = employee.df) + 
    geom_bar(aes(x = Age, color = factor(WorkLifeBalance)),
    stat = 'count') + 
    facet_grid(rows = vars(Gender)) +
    xlab("Age")

# Attrition count by Education level
ggplot(data = employee.df) + 
  geom_bar(aes(x = Attrition, fill = factor(Attrition)),
           stat = 'count') + 
  facet_grid(cols = vars(Education)) +
  xlab("Education")

# Box plot (to find outliers)
ggplot(data=employee.df) + geom_boxplot(aes(x=factor(Education), y=MonthlyIncome, fill = factor(Education))) +
  xlab("Education") +
  scale_fill_viridis_d()

ggplot(data=employee.df) + geom_boxplot(aes(x=Department, y=MonthlyIncome, fill = Department)) +
  xlab("Department") +
  facet_grid(cols = vars(Attrition))

?ggplot

#Scatter Plot
ggplot(data=employee.df) + 
  geom_point(aes(y = MonthlyIncome, x = Age, shape = Attrition, color = Department, size =3)) + 
  ylab("Monthly Income") + xlab("Age") 

# Histogram chart
ggplot(data=employee.df) + 
    geom_histogram(aes(x = MonthlyIncome, fill = Gender)) + 
    xlab("Monthly Income")

# Plot of the gender distribution of the total employees
ggplot(employee.df) + geom_bar(aes(Gender, fill = Gender), stat = 'count') + 
  labs(title = "Employee count by gender", y = "Count") + 
  theme(legend.title = element_blank()) + 
  scale_fill_discrete(labels=c("Female", "Male"))

# Plotting the chart for the exit_category by employee gender
ggplot(employee.df, aes(Gender, Attrition, fill = Gender), stat = "count") + 
  geom_col() + 
  labs(title = "Exit count per gender", y = "Count") + 
  theme(legend.title = element_blank()) + 
  scale_fill_discrete(labels=c("Female", "Male"))
