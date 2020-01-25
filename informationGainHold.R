library(rpart)
library(caret)
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


train_hold = subset(train_set, select = -c(capital.gain))
test_hold = subset(test_set, select = -c(capital.gain))


tree.income = rpart(formula = income~. , data = train_hold, method = "class", parms = list(split = "information"))
plot(tree.income, uniform=TRUE, 
     main="Income Classification using Information Gain w/o capital-gain")
text(tree.income, use.n=TRUE, all=TRUE)
post(tree.income, file = "gainHold.ps", title = "Income Classification using Information Gain w/o capital-gain")

x = predict(tree.income, test_hold, type = "vector")
tab = table(as.vector(x), test_hold[,14])
rownames(tab) = colnames(tab) 

conf = confusionMatrix(tab)
print(conf$table)
print(conf$overall)
print(conf$byClass)

