library(randomForest)
library(dplyr)
library(mice)
library(h2o)

setwd('C:/Users/dande/Desktop')

# Initializes H2O cluster
h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()

# Reads in training data with H2O
train <- h2o.importFile(path = normalizePath("C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Machine Learning/Project/Data/MLProject_train.csv"), header = TRUE)

# Reads in validation data with H2O
valid <- h2o.importFile(path = normalizePath("C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Machine Learning/Project/Data/MLProject_valid.csv"), header = TRUE)

train['target1'] <- as.factor(train['target1'])
valid['target1'] <- as.factor(valid['target1'])
train['target2'] <- as.factor(train['target2'])
valid['target2'] <- as.factor(valid['target2'])


# Build RF model for target 1
rf1 <- h2o.randomForest(training_frame = train, 
                        validation_frame = valid, 
                        y = 149, 
                        x=1:148, 
                        ntrees = 200, 
                        max_depth = 30, 
                        seed = 1234)
save(rf1, file = 'rf1_model')
summary(rf1)
                                
# Build RF model for target 2
rf2 <- h2o.randomForest(training_frame = train, 
                        validation_frame = valid,
                        y = 150, 
                        x=1:148, 
                        ntrees = 200, 
                        max_depth = 30,
                        seed = 1234)
save(rf2, file = 'rf2_model')
summary(rf2)


# Gradient Boosted Model for target1
gbm1 <- h2o.gbm(
  training_frame = train, 
  validation_frame = valid, 
  x=1:148,                      
  y=149,                    
  ntrees = 200,
  learn_rate = 0.03,
  seed = 1234)  
save(gbm1, file = 'gbm1_model')
summary(gbm1)

# Gradient Boosted Model for target2
gbm2 <- h2o.gbm(
  training_frame = train, 
  validation_frame = valid, 
  x=1:148,                      
  y=150,                    
  model_id = "gbm_target2_1",     ## name the model in H2O
  seed = 1234)   
save(gbm2, file = 'gbm2_model')
summary(gbm2)

# SVM for target1
svm1 <- h2o.psvm(training_frame = train,
                 validation_frame = valid,
                 x = 1:148,
                 y = 149, 
                 seed = 1234)
save(svm1, file = 'svm1_model')
summary(svm1)

# SVM for target2
svm2 <- h2o.psvm(training_frame = train,
                 validation_frame = valid,
                 x = 1:148,
                 y = 150, 
                 seed = 1234)
save(svm2, file = 'svm2_model')
summary(svm2)