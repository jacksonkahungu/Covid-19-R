covid=read.csv(file.choose())
covid
head(covid)
tail(covid)
str(covid)
names(covid)
summary(covid)


Gender=covid$Gender
Age=covid$Age
Virtual=covid$Virtual.working.befor.Covid
Experience=covid$Years.of.Experience
Company=covid$Nature.of.company
Communication=covid$Mode.of.Communication
Problem=covid$Most.Prvalent.Problem
Wellbeing=covid$Eployee.Well.Being
Recommendation=covid$Recoomendation
size=covid$Size.of.Team

# Convert categorical variables to factors
Gender <- as.factor(Gender)
Experience <- as.factor(Experience)
Virtual<- as.factor(Virtual)
Company <- as.factor(Company)
Communication <- as.factor(Communication)
Problem <- as.factor(Problem)
Recommendation <- as.factor(Recommendation)
Wellbeing<-as.factor(Wellbeing)
size<-as.factor(size)


# Load required libraries
library(ggplot2)

# Bar plot for Gender with color
plot1=ggplot(covid, aes(x = Gender)) + 
  geom_bar(fill = c("blue", "pink")) +
  labs(title = "Distribution of Gender", x = "Gender", y = "Frequency")

# Bar plot for Virtual working before Covid with color
plot2=ggplot(covid, aes(x = Virtual.working.befor.Covid)) + 
  geom_bar(fill = c("green", "red")) +
  labs(title = "Distribution of Virtual working before Covid", x = "Virtual working before Covid", y = "Frequency")

# Bar plot for Nature of company with color
plot3=ggplot(covid, aes(x = Nature.of.company)) + 
  geom_bar(fill = c("orange", "purple")) +
  labs(title = "Distribution of Nature of company", x = "Nature of company", y = "Frequency")
#bar plot for recommendation with color
plot4=ggplot(covid, aes(x = Recoomendation)) + 
  geom_bar(fill = c("blue", "green", "red")) +
  labs(title = "Distribution of Recommendation ", x = "Recommendation", y = "Frequency")
#bar plot for mode of communication with color
plot5=ggplot(covid,aes(x =Mode.of.Communication)) + 
  geom_bar(fill = c("blue", "green", "red", "violet")) +
  labs(title = "Distribution of mode of communication ", x = "mode of communication", y = "Frequency")
#bar plot for most prevalent problem with color
plot6=ggplot(covid,aes(x = Most.Prvalent.Problem)) + 
  geom_bar(fill = c("blue", "orange", "red")) +
 labs(title = "Distribution of most prevalent problem", x = "Most prevalent problem", y = "Frequency")
# Bar plot for employee well being with color
plot7=ggplot(covid, aes(x =Eployee.Well.Being)) + 
  geom_bar(fill = c("blue", "pink")) +
  labs(title = "Distribution of employee well being", x = "employee well being", y = "Frequency")
# Create a bar plot for Years of Experience
plot8=ggplot(covid, aes(x = Years.of.Experience)) + 
  geom_bar(fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Years of Experience", x = "Years of Experience", y = "Frequency")
library(gridExtra)

grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol =2)
plot1
plot2
plot3
plot4
plot5
plot6
plot7
plot8
#histogram

par(mfrow=c(2,1))
hist(Age,col="red")
hist(covid$Size.of.Team,col="blue")

#gg plot

plota=ggplot(covid,aes(x=Gender,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
plotb=ggplot(covid,aes(x=Age,y=Eployee.Well.Being))+
    geom_jitter(height=.5,alpha=.1)
plotc=ggplot(covid,aes(x=Years.of.Experience,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
plotd=ggplot(covid,aes(x=Virtual.working.befor.Covid,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
plote=ggplot(covid,aes(x=Size.of.Team,y=Eployee.Well.Being))+
 geom_jitter(height=.5,alpha=.1)
plotf=ggplot(covid,aes(x=Nature.of.company,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
plotg=ggplot(covid,aes(x=Mode.of.Communication,y=Eployee.Well.Being))+
    geom_jitter(height=.5,alpha=.1)
ploth=ggplot(covid,aes(x="Most.Prvalent.Problem" ,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
 ploti=ggplot(covid,aes(x=Recoomendation,y=Eployee.Well.Being))+
  geom_jitter(height=.5,alpha=.1)
grid.arrange(plota,plotb,plotc,plotd,plote,plotf,plotg,ploth,ploti,ncol=2)
plota
plotb
plotc
plotd
plote
plote
plotf
plotg
ploth
ploti
# Define a threshold or criterion
threshold <- "Yes"

# Create a binary column for Employee Well-Being
covid$employee.well.being <- as.integer(covid$Eployee.Well.Being == threshold)
covid$employee.well.being
# Combine the binary column with the original data
data <- covid

# Remove the non-binary column
data <- data[, -which(names(data) == "Eployee.Well.Being")]
data

#model for gender
modelA<-glm(employee.well.being~Gender,data=data,family="binomial")
modelA
summary(modelA)
plot(modelA)

#model for Age
modelB<-glm(employee.well.being~Age,data=data,family="binomial")
modelB
summary(modelB)
plot(modelB)


#model for years of experience
modelC<-glm(employee.well.being~Years.of.Experience,data=data,family="binomial")
modelC
summary(modelC)
plot(modelC)


#model for virtual working before covid
modelD<-glm(employee.well.being~Virtual.working.befor.Covid,data=data,family="binomial")
modelD
summary(modelD)
plot(modelD)


#model for size of team
modelE<-glm(employee.well.being~Size.of.Team,data=data,family="binomial")
modelE
summary(modelE)
plot(modelE)


#model for nature of company
modelF<-glm(employee.well.being~Nature.of.company,data=data,family="binomial")
modelF
summary(modelF)
plot(modelF)


#model for mode of communication
modelG<-glm(employee.well.being~Mode.of.Communication,data=data,family="binomial")
modelG
summary(modelG)
plot(modelG)


#model for most prevalent problem
modelH<-glm(employee.well.being~Most.Prvalent.Problem,data=data,family="binomial")
modelH
summary(modelH)
plot(modelH)



#model for recommendation
modelJ<-glm(employee.well.being~Recoomendation,data=data,family="binomial")
modelJ
summary(modelJ)
plot(modelJ)


# Fit logistic regression model
model <- glm(employee.well.being ~ Gender + Age + Virtual.working.befor.Covid + Years.of.Experience + Size.of.Team + Nature.of.company + Mode.of.Communication + Most.Prvalent.Problem +Recoomendation, data =data, family = binomial)
model
summary(model)
plot(model)

grid.arrange(plot(modelA),plot(modelB),plot(modelC),plot(modelD),plot(modelE),plot(modelF),plot(modelG),plot(modelH),plot(modelj),ncol=1)

colSums(is.na(covid))
# Generate new data for prediction
new_data <- covid[, c("Gender", "Age", "Virtual.working.befor.Covid", "Years.of.Experience", "Size.of.Team", "Nature.of.company", "Mode.of.Communication", "Most.Prvalent.Problem", "Recoomendation")]
new_data
# Predict probabilities
predicted_prob <- predict(model, newdata = new_data, type = "response")
predicted_prob
plot(predicted_prob)

# Calculate log-likelihood for each variable
logLik_gender <- logLik(modelA)
logLik_age <- logLik(modelB)
logLik_experience <- logLik(modelC)
logLik_virtual <- logLik(modelD)
logLik_size <- logLik(modelE)
logLik_company <- logLik(modelF)
logLik_communication <- logLik(modelG)
logLik_problem <- logLik(modelH)
logLik_recommendation <- logLik(modelJ)

# Combine log-likelihoods into a data frame
logLik_df <- data.frame(
  Variable = c("Gender", "Age", "Years.of.Experience", "Virtual.working.befor.Covid", 
               "Size.of.Team", "Nature.of.Company", "Mode.of.Communication", 
               "Most.Prvalent.Problem", "Recoomendation"),
  LogLikelihood = c(logLik_gender, logLik_age, logLik_experience, logLik_virtual, 
                    logLik_size, logLik_company, logLik_communication, 
                    logLik_problem, logLik_recommendation)
)
logLik_df
# Plot log-likelihood for each variable
ggplot(logLik_df, aes(x = Variable, y = LogLikelihood)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Log-Likelihood of Each Variable",
       x = "Variable", y = "Log-Likelihood")

# Calculate log-likelihood for the overall model
logLik_model <- logLik(model)

# Print log-likelihood for the overall model
print(logLik_model)

# Predict probabilities for each variable
logistic_curve_gender <- predict(modelA, newdata = data.frame(Gender = "Male"), type = "response")
logistic_curve_age <- predict(modelB, newdata = data.frame(Age = age_seq), type = "response")
logistic_curve_experience <- predict(modelC, newdata = data.frame(Years.of.Experience = "0 to 3"), type = "response")
logistic_curve_virtual <- predict(modelD, newdata = data.frame(Virtual.working.befor.Covid = "Yes"), type = "response")
logistic_curve_size <- predict(modelE, newdata = data.frame(Size.of.Team = 10), type = "response")
logistic_curve_company <- predict(modelF, newdata = data.frame(Nature.of.company = "Service"), type = "response")
logistic_curve_communication <- predict(modelG, newdata = data.frame(Mode.of.Communication = "Email"), type = "response")
logistic_curve_problem <- predict(modelH, newdata = data.frame(Most.Prvalent.Problem = "Disruptions"), type = "response")
logistic_curve_recommendation <- predict(modelJ, newdata = data.frame(Recoomendation = "Complete resumption"), type = "response")
logistic_curve_overall <- predict(model, newdata = new_data, type = "response")

# Plot log-likelihood for each variable as a straight line
ggplot(logLik_df, aes(x = Variable, y = LogLikelihood)) +
  geom_line(aes(group = 1), color = "skyblue") +
  labs(title = "Log-Likelihood of Each Variable",
       x = "Variable", y = "Log-Likelihood") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Plot the original data points
ggplot(data, aes(x = Gender, y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Gender", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(data, aes(x =Virtual.working.befor.Covid , y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Virtual.working.befor.Covid", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Years.of.Experience, y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Years.of.Experience", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Size.of.Team , y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Size.of.Team ", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Nature.of.company , y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Nature.of.company ", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x =Mode.of.Communication , y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Mode.of.Communication ", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Most.Prvalent.Problem , y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Most.Prvalent.Problem ", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Recoomendation, y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Recoomendation", y = "Probability of Well-being")  # Add labels

# Plot the original data points
ggplot(covid, aes(x = Age, y = employee.well.being)) +
  geom_point() +  # Add points for the original data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Add logistic curve
  labs(title = "Logistic Regression Curve", x = "Age", y = "Probability of Well-being")  # Add labels


library(car)
install.packages("randomForest")
library(randomForest)

data$Gender <- as.factor(data$Gender)
data$Years.of.Experience <- as.factor(data$Years.of.Experience)
data$Virtual.working.befor.Covid <- as.factor(data$Virtual.working.befor.Covid)
data$Nature.of.company <- as.factor(data$Nature.of.company)
data$Mode.of.Communication <- as.factor(data$Mode.of.Communication)
data$Most.Prvalent.Problem <- as.factor(data$Most.Prvalent.Problem)
data$Recoomendation<-as.factor(data$Recoomendation)
data$employee.well.being<-as.factor(data$employee.well.being)
data$Age<-as.factor(data$Age)
data$Size.of.Team<-as.factor(data$Size.of.Team)


# Fit Random Forest classifier
rf_model <- randomForest(employee.well.being ~Gender + Age + Virtual.working.befor.Covid + Years.of.Experience + Size.of.Team + Nature.of.company + Mode.of.Communication + Most.Prvalent.Problem +
  Recoomendation, data = data)
rf_model
summary(rf_model)

# Make predictions on test data
predictions <- predict(rf_model,data)
predictions

# Evaluate model performance
confusion_matrix <- table(data$employee.well.being, predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
plot(rf_model)
varImpPlot(rf_model)
importance(rf_model)



data$Age <- as.numeric(data$Age)
sigmoid <- function(params, x) {
  p <- params[1] + params[2] * x
  fitted_values <- 1 / (1 + exp(-p))
  return(fitted_values)
}
# Plot the data
plot(data$Age,data$employee.well.being, pch = 16, col = "blue", xlab = "Age", ylab = "employee.well.being", main = "Sigmoidal Curve Fitting")

# Extracting the fitted parameters
params <- coef(modelB)
params
# Generating the fitted values
fitted_values <- sigmoid(params, data$Age)
fitted_values
# Plotting the original data and the fitted curve
plot(data$Age, data$employee.well.being, pch = 16, col = "blue", xlab = "Age", ylab = "status", main = "Sigmoidal Curve Fitting")
lines(data$Age, fitted_values, col = "red")


plot(data$employee.well.being)

library(caret)

# Define outcome variable and predictor variables
outcome_var <- "Eployee.Well.Being"
predictor_vars <- c("Gender", "Age", "Virtual.working.befor.Covid", "Years.of.Experience", "Size.of.Team", "Nature.of.company", "Mode.of.Communication", "Most.Prvalent.Problem", "Recoomendation")

# Create training and testing sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(covid[, outcome_var], p = 0.8, list = FALSE)
train_data <- covid[train_index, ]
test_data <- covid[-train_index, ]

# Define models
rf_model <- randomForest(as.factor(employee.well.being) ~ . - Respondent, data = train_data)

logistic_model <- glm(as.factor(employee.well.being) ~ . - Respondent, data = train_data, family = "binomial")
logistic_aic <- AIC(logistic_model)
print(logistic_aic)
