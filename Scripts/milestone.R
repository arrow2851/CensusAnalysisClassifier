
library(rpart)
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

newdata = subset(train_set, select  = -c(income))
#print(names(train_set))

formula_list = (paste(names(newdata), sep = '+', collapse = '+'))
formula_list = (paste(c("income",formula_list), collapse= '~'))
#print(formula_list)
formula_exp = formula(formula_list)
tree.income = rpart(formula = income~.-capital.gain , data = train_set, method = "class", parms = list(split = "information"))
plot(tree.income, uniform=TRUE, 
     main="Classification Tree for Income")
text(tree.income, use.n=TRUE, all=TRUE)
post(tree.income, file = "tree1.ps", 
     title = "Classification Tree for Income")
#print(tree.income$frame)


x = predict(tree.income, test_set, type = "vector")
tab = table(x)
TP = 0
FP = 0
TN = 0
FN = 0
for(index in seq(1:800))
{

  if(test_set[index,15] == " <=50K")
  {
    if(x[index] == 1)
      TP = TP+1
    else
      FN = FN+1
    
    }
  else if(test_set[index,15] == " >50K")
  {
    if(x[index] == 2)
      TN = TN+1
    else
      FP = FP+1
    
    }
  
  
  
}
cat(TP,FN,FP,TN, "\n")
print(summary(test_set[,15]))

accuracy = (TP+TN)/(TP+TN+FP+FN)
precision = TP/(TP+FP)
recall = TP/(TP+FN)

fscore = 2*(recall * precision) / (recall + precision)

cat("Accuracy = ",accuracy)
cat("\nPrecision = ",precision)
cat("\nRecall = ",recall)
cat("\nF1 Score = ",fscore)