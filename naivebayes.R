
library(rpart)
library(caret)
library(e1071)
set.seed((40))
#read the file and remove the ? rows
cen.data = read.csv("census-adult.csv",na.strings = " ?")
cen.data = na.omit(cen.data)


#take a sample and split the data into training and test 60:40
indices = sample(1:nrow(cen.data), 2000)
train = indices[801:2000]
test = indices[1:800]
train_set = cen.data[train,]
test_set = cen.data[test,]

bayes.model = naiveBayes(formula = income~. , data = train_set)


x = predict(bayes.model, test_set)
tab = table(as.vector(x), test_set[,15])
rownames(tab) = colnames(tab) 

conf = confusionMatrix(tab)
print(conf$table)
print(conf$overall)
print(conf$byClass)


