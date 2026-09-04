## Step 1: Load data ----
getwd()
# Read the CSV file
df <- read.csv("/Users/ruihanshan/Desktop/data_for_final_assignment_STU33002.csv")

# Look at the structure of the data, first 6 rows of the dataset
#variable name, type (numeric, integer, etc.), and a few example values.
head(df)
str(df)

# Basic summary statistics
#gives you min/median/mean/max for each numeric column.
summary(df)

# Confirming it’s a binary variable for logistic regression.output 0/1
unique(df$loan_status)

## Step 2: Train-test split 80%Train, 20%Test

set.seed(123)  # same random split each time we run

n <- nrow(df)                         # total number of observations
train_size <- floor(0.8 * n)          # 80% for training,choose number of training rows

train_index <- sample(seq_len(n), size = train_size) #Randomly selects train_size row indices from 1 to n

train <- df[train_index, ]            # training set("all rows in train_index.")
test  <- df[-train_index, ]           # test set("all rows NOT in train_index.(remaining rows)")

# Quick check: if data correctly splited into 80% and 20%
nrow(train)   #Shows how many rows are in each split.
nrow(test)

## Step 3: Fit logistic regression model by Generalized Linear Model(glm)----
#response: loan_status (0/1)
# 7 predictors/variables
#predict the probability that loan_status = 1
#building a model that predicts the likelihood a loan will be 
#approved/rejected/defaulted based on borrower + loan characteristics.
loan_model <- glm(
  loan_status ~ loan_size + interest_rate + borrower_income +
    debt_to_income + num_of_accounts + derogatory_marks + total_debt,
  data = train,
  family = binomial     #Using family = binomial tells R to use a logit link 
                        #and model the probability that loan_status = 1 (loan approved).”
)

#Shows which variables matter, 
#how strong they are, and whether the model fits the data well.
#Prints the coefficient estimates, standard errors, z-values and p-values, 
#plus deviance and AIC.
#It also shows that interest_rate and total_debt are dropped due to multicollinearity, 
#so effectively the model uses five variables.”
summary(loan_model)

## Step 4: Predict probabilities on test data ----
#Uses the fitted model to compute predicted probabilities for each observation in the test set.
#type = "response" returns probabilities between 0 and 1.
test$pred_prob <- predict(loan_model, newdata = test, type = "response")

head(test$pred_prob)    #Shows the first few predicted probabilities.
#“This is just to see typical values. Most probabilities are quite low, 
#which is consistent with the low overall approval rate.”

## Step 5: Convert probabilities to classes ----
#Applies your chosen threshold (≈ 0.0463 obtained from ROC curve) to classify each customer:
#If predicted probability ≥ threshold → predict 1 (approved)
#Otherwise → predict 0 (not approved)
test$pred_class <- ifelse(test$pred_prob >= 0.0462666, 1, 0)

#Shows side-by-side the predicted probability and classification for the first few test cases.
#This lets me check that the threshold is being applied correctly.
head(test[, c("pred_prob", "pred_class")])

## Step 6: Confusion matrix & accuracy ----
#Builds a contingency table comparing predicted classes vs actual outcomes on the test set.
conf_mat <- table(Predicted = test$pred_class, Actual = test$loan_status)
conf_mat

# Accuracy
#Compares predicted vs actual for each test observation.
#Takes the mean (proportion correct) → overall accuracy.
accuracy <- mean(test$pred_class == test$loan_status)
accuracy

## Step 7: ROC and AUC ----
#provides tools to compute ROC curves and AUC.
library(pROC)

#Creates an ROC curve object using the true labels (loan_status) and the predicted probabilities (pred_prob).
roc_obj <- roc(test$loan_status, test$pred_prob)
#Here I construct the ROC curve by comparing true outcomes with the predicted probabilities over all possible thresholds.

# AUC value
#“The AUC summarises the ROC curve into a single number between 0.5 and 1. Our AUC of around 0.75 indicates that the model has 
#good discriminatory power in distinguishing approved from rejected applications
auc(roc_obj)

# Plot ROC curve
#Plots the ROC curve: sensitivity vs 1–specificity across thresholds.
#The ROC curve visually shows the trade-off between sensitivity and specificity for all thresholds. 
#The fact that the curve lies above the diagonal line confirms that the model performs better than random guessing.
plot(roc_obj, col = "blue", main = "ROC Curve for Loan Approval Model")

#Just reprints the model summary (useful if you want it in the console after the ROC block).
summary(loan_model)

## Extra metrics from confusion matrix ----

#Pulls out True Negatives, False Positives, False Negatives, and True Positives from the confusion matrix.
TN <- conf_mat[1, 1]  # Predicted 0, Actual 0
FP <- conf_mat[1, 2]  # Predicted 0, Actual 1
FN <- conf_mat[2, 1]  # Predicted 1, Actual 0
TP <- conf_mat[2, 2]  # Predicted 1, Actual 1

sensitivity <- TP / (TP + FN)   # True Positive Rate,how well the model identifies actual positive cases.
specificity <- TN / (TN + FP)   # True Negative Rate
precision   <- TP / (TP + FP)   # Of predicted approvals, how many are really approved?

sensitivity #probability the model says “approved” given it was truly approved.
specificity #probability the model says “not approved” given it was truly not approved.
precision #proportion of predicted approvals that were actually correct.

#Uses the ROC curve to find the “best” threshold according to Youden’s Index 
#(maximising sensitivity + specificity – 1).
#Returns that threshold and the corresponding sensitivity and specificity.
coords(roc_obj, "best", ret=c("threshold","sensitivity","specificity"))

## Step 8: Rating new customers ----

# Example: new applicant profile
#Creates a one-row data frame representing a hypothetical new applicant.
new_applicant <- data.frame(
  loan_size        = 28000,
  interest_rate    = 3.2,
  borrower_income  = 48000,
  debt_to_income   = 0.35,
  num_of_accounts  = 4,
  derogatory_marks = 0,
  total_debt       = 17000
)

# Predicted probability of approval (the "rating")
# Uses the model to compute this applicant’s predicted probability of approval (around 3.3%).
new_prob <- predict(loan_model, newdata = new_applicant, type = "response")
new_prob

# Classification based on the chosen threshold
#Uses the optimal threshold to classify the new applicant as approved (1) or not approved (0).
optimal_threshold <- 0.0462666
new_decision <- ifelse(new_prob >= optimal_threshold, 1, 0)
new_decision

#Plots a histogram of the predicted probabilities on the test set.
#This plot shows how the predicted approval probabilities are distributed. 
#It visually confirms that most probabilities are very low, 
#which explains why a threshold of 0.5 is inappropriate and why a much lower threshold 
#such as 0.0463 is needed in this context.
hist(test$pred_prob, breaks = 50,
     main = "Distribution of Predicted Approval Probabilities",
     xlab = "Predicted Probability",
     col = "lightblue")






