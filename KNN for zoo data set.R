View(zoo)
str(zoo) ## in data set variables are which data type we can see
names(zoo) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type") # change the data column names as oer our requirement

types <- table(zoo$type)# in type column how many total values in table format 

zoo$animal <- NULL
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
## Let us assuming the in zoo how many  category types of animals 

types# Checking the animal category type name  and quantity   
summary(zoo) ## find  the 1st,2nd qurtile and min and max values
str(zoo) # Check the varibles in which data type 
View(zoo)
# create training and test data
zoo_train <- zoo[1:70, ]
zoo_test <- zoo[71:101, ]

# create labels for training and test data

zoo_train_labels <- zoo[1:70, 17]

zoo_test_labels <- zoo[71:101, 17]


# load the "class" library
library(class)
?knn
## Creating the model 
zoo_test_pred <- knn(train = zoo_train,test=zoo_test,cl= zoo_train_labels,k=10)
                      
zoo_test_pred



##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)
table(zoo_test_pred,zoo_test_labels)

## hence if k=10 ,the model Accuracy is 76.74 

# K=20
zoo_test_pred <- knn(train = zoo_train,test=zoo_test,cl= zoo_train_labels,k=20)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,  prop.chisq=FALSE)
# ################# if k=20 ,the model Accuracy is 35.4%  #####

## K=5
zoo_test_pred <- knn(train = zoo_train,test=zoo_test,cl= zoo_train_labels,k=5)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,  prop.chisq=FALSE)         
# ################# if k=5 ,the model Accuracy is 77.4%  #####       
           
#k=2           
zoo_test_pred <- knn(train = zoo_train,test=zoo_test,cl= zoo_train_labels,k=2)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,  prop.chisq=FALSE)         
# ################# if k=2 ,the model Accuracy is 80.6% #####     

#k=1
zoo_test_pred <- knn(train = zoo_train,test=zoo_test,cl= zoo_train_labels,k=1)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,  prop.chisq=FALSE)         
# ################# if k=1 ,the model Accuracy is 93.5%  ##### 

