library(ElemStatLearn)
library(caret)
library(e1071)
# Dataset ZipCode (Dimensions)
# http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/zip.info

DATASET.train <- as.data.frame(zip.train)
DATASET.test <- as.data.frame(zip.test)
#Filtered Classes 
data.train<- DATASET.train[DATASET.train$V1 >3,]
data.test<- DATASET.test[DATASET.test$V1 >3,]
# Tuning the parameter Choose with least error
tc <- tune.control(cross = 5)
#Linear
prioir_svm <- tune.svm(V1~., data = data.train, 
                       cost = 2^(2:4), 
                       kernel = "linear", tunecontrol = tc)

#Polynomial kernel
prioir_svm_poly <- tune.svm(V1~., data = data.train, type='C-classification',
                        degree = c(2,3,4),
                       kernel = "polynomial", tunecontrol = tc)

# Radial Kernel Default
prioir_svm_radial <- tune.svm(V1~., data = data.train, type='C-classification',
                            gamma = 2^(-1:1), tunecontrol = tc)

# Choosing best model to be of Polynomial with degree 3
svm.fit = svm(V1 ~ ., data = data.train, type='C-classification', kernel='polynomial',degree= 3, scale=FALSE)

pred <- predict(svm.fit, data.test[,-1])

# Check accuracy:
table <- table(pred, data.test$V1)
confusionMatrix(table)
