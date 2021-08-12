#KNN Regression Problem

#Loading the training and test datasets
csvtrain  <-read.csv('AutoTrain.csv',header = TRUE)
csvtest   <-read.csv('AutoTest.csv',header = TRUE)

#Assigning values
x.train <-csvtrain$horsepower # x.train = vector of training predictor values
y.train <- csvtrain$mpg       # y.train = vector of training response values     
x.pred <- csvtest$horsepower  # x.pred  = vector of predictor inputs with unknown

#define values of k to evaluate
kvals <- c(2,5,10,20,30,50,100)
kval2 <- c(2)#  testing for k value of 2

#KNN Regression function to estimate the predictor values 
kNN <- function(k,x.train,y.train,x.pred) #k= the number of neighbors to consider
 {

    ## Initialize:
    n.pred <- length(x.pred);		
    y.pred <- numeric(n.pred)
    
  ## Main Loop
  for (i in 1:n.pred)
  {
      d <- abs(x.train - x.pred[i])
      dstar = d[order(d)[k]]
      y.pred[i] <- mean(y.train[d <= dstar])		
  }
    
    return(y.pred) #y.pred  = predicted response values for x.pred
 }
ypred_list<- list()

#Iterate through values of K
for (k in kvals)
  {
  s <-c( kNN(k,x.train,y.train,x.pred) )
  print(s)
  ypred_list[[length(ypred_list) +1]] <- s
  }
ypred_list


#computing the testing MSE for each value of k. 
mse_list_test <- list()
for (n in 1:length(ypred_list))
 {
  MSE = mean((c(y.test) - c(ypred_list[[n]]))^2)#Computing MSE using the formula MSE=mean((ytest - ypredicted)^2)
  c(y.test)
  mse_list_test[[length(mse_list_test)+1]]<- MSE
 }
mse_list_test #Displays list of MSE Testing values


#Minimum of MSE will perform well at that value of k =4
min_mse  <- min(as.numeric(c(mse_list_test)))
min_mse #Displays minimum MSE value


# for training data
ytrain_list <- list()
for (k in kvals)
 {
  s <-c( kNN(k,x.train,y.train,x.train) )
  print(s)
  ytrain_list[[length(ytrain_list) +1]] <- s
 }

ytrain_list

#Computing training MSE:using MSE=mean((ytrain - ypredicted)^2)
mse_list_train <- list()
for (n in 1:length(ytrain_list))
  {
  MSE = mean((c(y.train) - c(ytrain_list[[n]]))^2)
  c(y.test)
  mse_list_train[[length(mse_list_train)+1]]<- MSE
  }
mse_list_train #Displays list of MSE Training values
mse_list_test  #Displays list of MSE Testing values

#Plotting the  training data, testing data and the best kNN model
plot(x.train, y.train, col="red", xlab="horsepower", ylab="mpg",title('Graph (horsepower vs mpg )'))
points(x.pred, y.test, col="blue", xlab="horsepower", ylab="mpg")

#Best model at k=4
points(x.pred, ypred_list[[4]], col="green", xlab="horsepower", ylab="mpg")

