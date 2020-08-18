View(glass)
str(glass)

glass$type=factor(glass$Type)
str(glass)
View(glass)

# table or proportions with more informative labels
round(prop.table(table(glass$type)) * 100, digits = 1)

# summarize any three numeric features
summary(glass[c("RI","Na","Mg")]) 
# After summarize the above 3 numeric features units are different so we need to normalize 

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
#normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
#normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd1 data
glass_n <- as.data.frame(lapply(glass[1:9], normalize))
View(glass_n)
summary(glass_n)
str(glass_n)



# create training and test data
glass_train <- glass_n[1:180, ]
glass_test <- glass_n[181:214, ]

# create labels for training and test data

glass_train_labels <- glass[1:180, 11]
 

glass_test_labels <- glass[181:214, 11]


#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)
?knn

glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=3)
glass_test_pred

##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)
