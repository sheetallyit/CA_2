# Firstly storing the file in the working directory,
# followed by reading into a data frame.

heart_record <- read.csv("heart.csv", header = TRUE)
heart_record

# To know the structure of the heart.csv file
str(heart_record)

# To know the data type of the heart.csv file
class(heart_record)

# The next step is to deal with the missing data records.
# In heart.csv file, we do not have any missing record in the file,
# so this step is eliminated still for finding the count of NA values

sum(is.na(heart_record))
# no NA values in the file

# To check whether 
is.na(heart_record)

# To check the head 6 records of the file.
head(heart_record)

# To check the tail 6 records of the file.
tail(heart_record)

# To check the numbers of column present in the file.
ncol(heart_record)

# To check the numbers of row present in the file.
nrow(heart_record)

# To know the column names present in the file.
colnames(heart_record)

# To know the summary of the file.
summary(heart_record)


# To examine the correlation between the variables of file 
# with the help of the psych library
install.packages("psych")
library(psych)
pairs(heart_record, labels = colnames(heart_record), main = "Heart attack plot")

# To know the positive correlation between various parameters      
pairs.panels(heart_record,
             smooth = TRUE,      # If TRUE will return draws less smooths
             scale = FALSE,      # If TRUE will scales the correlation text font
             density = TRUE,     # If TRUE will adds density plots and histograms
             ellipses = TRUE,    # If TRUE will draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, report  ts correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE will adds significance level with stars
             ci = TRUE)   


# To know the continuous and discrete variables in detail
# with the help of Data Explorer package
install.packages("DataExplorer")
library(DataExplorer)

plot_intro(heart_record,title="Dataset Information")

# Converting sex column to factor variable
heart_record$sex <- factor(heart_record$sex, 
labels = c("Female", "Male"))
# Understanding the counts of the data set wrt to sex,age,thalach,chol, cp, fbs and slp
table(heart_record$sex)
table(heart_record$age)
table(heart_record$thalachh)
table(heart_record$chol)
table(heart_record$cp)
table(heart_record$fbs)
table(heart_record$slp)

# The scatter plot for thalachh data variables against age

scatter.smooth(x = heart_record$thalachh, 
               y = heart_record$age, 
               main = "Thalachh ~ Age",
               xlab = "Thalachh",
               ylab = "Age")
# The scatter plot for sex data variables against age
scatter.smooth(x = heart_record$sex, 
               y = heart_record$age, 
               main = "Sex ~ Age",
               xlab = "Sex",
               ylab = "Age")
# The scatter plot for chol data variables against age
scatter.smooth(x = heart_record$chol, 
               y = heart_record$age, 
               main = "Chol ~ Age",
               xlab = "Chol",
               ylab = "Age")
# The scatter plot for slp data variables against age
scatter.smooth(x = heart_record$slp, 
               y = heart_record$age, 
               main = "Slp ~ Age",
               xlab = "Slp",
               ylab = "Age")

# Displaying the correlation values against the age

paste("Correlation of thalachh and Age: ", 
      cor(heart_record$thalachh, heart_record$age))

paste("Correlation of cp and Age: ", 
      cor(heart_record$cp, heart_record$age))

paste("Correlation of chol and Age: ", 
      cor(heart_record$chol, heart_record$age))

paste("Correlation of slp and Age: ", 
      cor(heart_record$slp, heart_record$age))

# For checking the outlier
attach(heart_record)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))

boxplot(heart_record,
        main = "Heart Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(age)$out)) # box plot for heart_record dataset

boxplot(age, 
        main = "age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(age)$out) )# box plot for 'age'

boxplot(thalachh, 
        main = "thallach", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(thalachh)$out) )# box plot for 'thalachh'

boxplot(slp, 
        main = "slp", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(slp)$out) )# box plot for 'slp'


boxplot(chol, 
        main = "chol", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(chol)$out) )# box plot for 'chl'


par <- opar

detach(heart_record)
# we can see from the figure that for chol, there exist outier for value
# 394, 407, 409, 417 and 564. trying to remove it 

nrow(heart_record)
heart_record <- subset(heart_record, heart_record$chol!=394)
heart_record <- subset(heart_record, heart_record$chol!=407)
heart_record <- subset(heart_record, heart_record$chol!=409)
heart_record <- subset(heart_record, heart_record$chol!=417)
heart_record <- subset(heart_record, heart_record$chol!=564)
nrow(heart_record)

#checking whether outlier had been removed or not

boxplot(chol, 
        main = "chol", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(chol)$out) )# box plot for 'distance'
# we can see from the figure that for thallach, there exist outlier for value 71

nrow(heart_record)
heart_record <- subset(heart_record, heart_record$thalachh!=71)
nrow(heart_record)
#checking whether outlier had been removed or not

boxplot(thalachh, 
        main = "thallach", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(thalachh)$out) )# box plot for 'speed'
# now their are no outlier present in the varibles required for analysis
 
# For checking the Skewness, need o install the package e1071
install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2))

# Displaying the density plot and the skewness examination wrt to heart record dataset
plot(density(heart_record$age),
     main = "Density Plot of heart record",
     xlab = "Age ",
     ylab = "Heart Record",
     sub = paste("Skewness:", round(e1071::skewness(heart_record$age), 2)))
# Viewing it in pink
polygon(density(heart_record$age), col = "pink")

# Displaying the density plot wrt to Thalachh variable
plot(density(heart_record$thalachh),
     main = "Density Plot of Thalachh",
     xlab = "Age",
     ylab = "Thalachh",
     sub = paste("Skewness:", round(e1071::skewness(heart_record$thalachh), 2)))

# Viewing it in blue
polygon(density(heart_record$thalachh), col = "blue") #fills 

# Displaying the density plot wrt to Slope variable
plot(density(heart_record$slp),
     main = "Density Plot of Slope",
     xlab = "Age",
     ylab = "Slp",
     sub = paste("Skewness:", round(e1071::skewness(heart_record$slp), 2)))

# Viewing it in green
polygon(density(heart_record$slp), col = "green") #fills 

# Displaying the density plot wrt to sex variable
plot(density(heart_record$sex),
     main = "Density Plot of sex",
     xlab = "Age",
     ylab = "Sex",
     sub = paste("Skewness:", round(e1071::skewness(heart_record$sex), 2)))

# Viewing it in orange
polygon(density(heart_record$sex), col = "orange") #fills 

# Displaying the density plot wrt to cholesterol variable
plot(density(heart_record$chol),
     main = "Density Plot of Cholesterol",
     xlab = "Age",
     ylab = "Chol",
     sub = paste("Skewness:", round(e1071::skewness(heart_record$chol), 2)))

# Viewing it in yellow
polygon(density(heart_record$chol), col = "yellow") #fills 
par <- opar

#building a linear model using the training data
model <- lm(formula = thalachh ~ age + slp, 
            
            data = training_data)
# Ensuring the model is statically stable
model
# Viewing the Summary of the model
summary(model)

# Checking the correlation between the thalach and slope variables with the
# help of corr plot package
cor(heart_record$thalachh, heart_record$slp)
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#png(file="corr2.png",res=150,width=900,height=700)                        
  
# 10 normality using qq  qq norm
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
hist(heart_record$thalachh, 
     main = "Normality proportion of thalachh", 
     xlab = "thalachh")

qqnorm(heart_record$thalachh)
qqline(heart_record$thalachh)
par <- opar

#Setting training and testing the data for predictive analysis
# library("lattice")
# attach(heart_record)
set.seed(1)
no_rows_data <- nrow(heart_record)
sample <- sample(1:no_rows_data, size = round(0.8 * no_rows_data), replace = FALSE)

training_data <- heart_record[sample, ]
testing_data <- heart_record[-sample, ]

# setting training n testing data
nrow(training_data)
nrow(testing_data)

# Relationship between various data variable of dataset
# and displaying the building model  using the training sets
model <- lm(formula = thalachh ~ 
              age + 
              sex + 
              trtbps + 
              chol + 
              fbs + 
              restecg +
              slp +
              oldpeak +
              caa, 
            data = training_data)

# displaying the model
model 
# knowing the summary of the dataset
summary(model)

# after observing the summary, it is seen that the variable thalachh, age 
# and slope have significant relationship with each other. So we consider
#linear model building using the thallach , age and slp variables only.
model <- lm(formula = thalachh ~ age + slp, data = training_data)
model
summary(model)

# Viewing the Akaike’s Information Criterion - AIC (Akaike, 1974)
# and the Bayesian Information Criterion - BIC  (Schwarz, 1978) 
# for knowing the goodness of fit
AIC(model)
BIC(model)

# Predicting data using the model to know their comparison with the testing data
predicted_heart_record <- predict(model, testing_data)

# make actuals_predicted dataframe.
actuals_predictions <- data.frame(cbind(actuals = testing_data$thalachh, 
                                        predicted = predicted_heart_record))
head(actuals_predictions)

# To know the corelation accuracay
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

# To know the minimum and maximum accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

# Calculating the min, max accuracy and mean absolute percentage error (MAPE) 
# which is a measure of prediction accuracy.
mape <- mean(abs((actuals_predictions$predicted - 
                  actuals_predictions$actuals)) / 
               actuals_predictions$actuals)
mape

# To know the internal and cross-validation measures 
# of predictive accuracy  with the help of DAAG library  and use the CVlm() function.
install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = heart_record, 
                                   form.lm = thalachh ~ chol, 
                                   m = 5, 
                                   dots = FALSE, 
                                   seed = 1, 
                                   legend.pos = "topleft", 
                                   printit = FALSE, 
                                   main = "Small symbols are predicted values while bigger ones are actuals."));

# prediction of single value from the model
df <- data.frame(chol = c(100))
predicted_heart_record <- predict(model, df)
predicted_heart_record