#takes seed
set.seed((40))
#read the file and remove the ? rows
cen.data = read.csv("D:/Important/Data Mining/Project 1/Census Analysis Project DM/census-adult.csv",na.strings = " ?")
cen.data = na.omit(cen.data)

#take a sample and split the data into training and test 60:40
indices = sample(1:nrow(cen.data), 2000)
train = indices[801:2000]
test = indices[1:800]
test_set = cen.data[test,]
train_set = cen.data[train,]
 
#calculate the entropy
'
entropy = function(column)
{
  values = table(column)
  probablitites = values/length(column)
  
   
  
}

'

entropy <- function(val) {
  freq <- table(val)/length(val)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #print(class(summary(val)))
  #compute entropy
  -sum(vec * log2(vec))
}


#calculate the information gain for each of the columns (use a user-defined function)
infoGain = function(sub_data, split_col)#
{
  gain = 0
  min_entropy = 1
  parent_entopy = entropy(sub_data$income)
  split_list = list()
  total_length = length(sub_data$income)
  #col_type = class(sub_data[,split_col])
  total_entropy = 0
  split_point = -1
  if(is.factor(sub_data[,split_col]))
  {

   split_list = split(sub_data, sub_data[,split_col])
   i = 1
   for(node in split_list)
   {
     temp = entropy(node$income)
     len = length(node$income)
     if(len == 0)
       next
     total_entropy =total_entropy +  (len * entropy(node$income))/total_length
     i = i+1
   }
   gain = parent_entopy - total_entropy
    
  }
  else 
  {
    vect = range(sub_data[,split_col])
    min = vect[1]
    max = vect[2]
    
    skip = 1
    if( max- min > 100000)
      skip = 10000
    else if(max - min > 10000)
      skip = 1000
    else if(max-min > 1000)
      skip = 100
    else if(max- min >100)
      skip = 10
    
    
    for (i in seq(min+1,max, skip)) {
      
      split_a = sub_data[sub_data[,split_col] < i ,]
      split_b = sub_data[sub_data[,split_col] >= i ,]
      w_temp_entropy = (length(split_a)*entropy(split_a$income)) / total_length
      w_temp_entropy = w_temp_entropy + ((length(split_b)*entropy(split_b$income)) / total_length)

      
      if(as.numeric(w_temp_entropy) < min_entropy )
        {
        min_entropy = as.numeric(w_temp_entropy)
        split_point = i
        }
    }
    gain = parent_entopy - min_entropy
    
    
  }
  
  
  c(gain, split_point)
}

' Steps = 
  Calculate the entropy of the label and the entropy of each of the splits
  Check the IG of each node with the income and choose the largest among them
  Mark the n rows of this column as completed/<NA> in a vector/2d array
  Split the node into n categories in a queue where each category conatins lists/vectors of the rows that have to
    be futher split
  


'

#print(entropy(cen.data$income))

X = split(cen.data, cen.data$relationship)

#print(length(X))

max = 90
min = 17
#print(seq(min, max, 10))


end_condition = function(node_list)
{
  flag = TRUE
  #print(node_list)
  for (node in node_list) 
  {
    if(as.numeric(node[3]) > 0.1)
    {
      flag = FALSE
      break
    }
  }
  
  flag
}

decision = function(df, label_col)
{
  #node structue = list(datasubset, colsAlreadySplitBy, Entropy(min  = 0.1) )
  parent = list(df, NA , entropy(df[,label_col]))
  tree_list = list(parent)
  #print(tree_list)
  current_node = 1
  while(!end_condition(tree_list))
  {
    cat("Tree Length = ",length(tree_list),"\n")
    if(current_node>length(tree_list))
      break
    node = tree_list[[current_node]]
    print(node[1])
    
    if(length(node) <= 0)
      break
    
    if(node[3] < 0.5) #if this node satisfies the min entropy then skip
    {
      
      current_node = current_node + 1
      next
    }      

    #skip_vector = node[2]
    
    max_info =0
    split_col = 0
    print(node[2])
    cat("Col = ")
    for(col in seq(1,label_col-1)) # loop through each column and check for best information gain
    {
      #check condition to skip checking already split cols
      if(!is.na(node[2]))
      {
        flag = FALSE
        for(i in node[2])
        {
          if(col %in% i)
          {flag = TRUE}
          break
        }
      if(flag)
        next
      }
      cat(col," ")
      #print(node[[1]])
      
      temp = infoGain(node[[1]], col)
      '
      cat(max_info[1],temp[1],"\n")'
      if(temp[1] > max_info[1] )
      {
        max_info = temp
        split_col = col
      }
      
    }
    
    
    if(max_info[2] >-1)
    {
      sub = node[[1]]

      
      
      split_vector = c(split_col)
      if(!is.na(node[2]))
      {
        split_vector = append(split_vector,node[[2]])
      }  
      new_subset_a = sub[sub[,split_col]<max_info[2],]
      new_subset_b = sub[sub[,split_col]>=max_info[2],]
      tree_list[current_node+1] = list(new_subset_a, split_vector,entropy(new_subset_a[,label_col])) 
      tree_list[current_node+2] = list(new_subset_b, split_vector,entropy(new_subset_b[,label_col]))
      #print(tree_list[2])

    }
    else
    {
      count = 1
      split_list = split(sub_data, sub_data[,split_col])
      for(sub_list in split_list)
      {
        len = length(sub_list[,label_col])
        if(len == 0)
        next
        temp = entropy(sub_list[,label_col])
        if(!is.na(node[2]))
        {
          split_vector = append(split_vector,node[[2]])
        } 
        tree_list[current_node+count] = list(sub_list, split_vector, temp)
        
        count = count+1
      }
      
      
    }
    
    current_node = current_node+1
    cat("\n\nCurrent Node = ",current_node, "\n")
  }
  
}


#print(entropy(cen.data[,15]))
decision(cen.data, 15)
'
print(cen.data)
label = data.frame(cen.data$income)
print(class(cen.data))
print(summary(label))
newdata = subset(cen.data, select  = -c(income))
#print((newdata))
print(names(cen.data))
'

'
formula_list = (paste(names(newdata), sep = '+', collapse = '+'))
formula_list = (paste(c("income",formula_list), collapse= '~'))
print(formula_list)
formula_exp = formula(formula_list)
print(c(class(cen.data), class(formula_exp)))
#print(kyphosis)
tree.income = rpart(formula = formula_exp , data = cen.data, method = "class", parms = list(split = "information"))
plot(tree.income, uniform=TRUE, 
     main="Classification Tree for Income")
text(tree.income, use.n=TRUE, all=TRUE, cex=.8)
post(tree.income, file = "tree.ps", 
     title = "Classification Tree for Income")
print(tree.income)
    
     '
