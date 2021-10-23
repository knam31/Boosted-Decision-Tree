

library(MASS)
attach (Boston)




get_index<-function(attribute,s,y_train){
  greater_than_index <- attribute>=s
  lesser_than_index <- attribute<s
  return(list("greater_than_index"=greater_than_index,"lesser_than_index"=lesser_than_index))
}

#Calculating the mean label mean labels y for the training observations values 
calc_mean<-function(index_values,y_train){
  lower_mean <- mean(y_train[index_values$lesser_than_index])
  lower_mean <- ifelse(is.nan(lower_mean),0,lower_mean)
  upper_mean <- mean(y_train[index_values$greater_than_index])
  upper_mean <- ifelse(is.nan(upper_mean),0,upper_mean)
  return(list("lower_mean"=lower_mean,"upper_mean"=upper_mean))
}

#function to calculate train_RSS
calc_RSS<-function(attribute,s,y_train){
  index_values <- get_index(attribute,s,y_train)
  means <- calc_mean(index_values,y_train)
  less_than <-sum((y_train[index_values$lesser_than_index]-means$lower_mean)^2)
  greater_than <-sum((y_train[index_values$greater_than_index]-means$upper_mean)^2)
  return(less_than+greater_than)
}

#Predicting the labels with test data
predict_yhat <- function(ds,x_test){
  y_hat <- rep(0,length(x_test))
  y_hat[x_test<ds$s] <- ds$lower_mean
  y_hat[x_test>=ds$s] <- ds$upper_mean
  return(y_hat)
}

#Calculating test MSE for DS function
calc_test_MSE <-function(ds_list,x_test,y_test){
  y_hat <- predict_yhat(ds_list,x_test)
  MSE <- mean((y_test-y_hat)^2)
  return(MSE)
}


#DS Algorithm implementation
DS <- function(x_train,y_train){
  lstat_values <- x_train[,1]
  rm_values <- x_train[,2]
  
  s_lstat <- seq(min(lstat_values),max(lstat_values),0.1)
  lstat_rss_list <- rep(0,length(s_lstat))
  s_rm <- seq(min(rm_values),max(rm_values),0.1)
  rm_rss_list <- rep(0,length(s_rm))

  for(s in s_lstat){
    lstat_rss_list[s_lstat==s]<-calc_RSS(lstat_values,s,y_train)
  }
  for(s in s_rm){
    rm_rss_list[s_rm==s]<-calc_RSS(rm_values,s,y_train)
  }
  lstat_rss <- min(lstat_rss_list)
  rm_rss <- min(rm_rss_list)
  
  
  if(lstat_rss<rm_rss){
    s <- s_lstat[lstat_rss_list==lstat_rss][1]
    index_values <- get_index(lstat_values,s,y_train)
    means <- calc_mean(index_values,y_train)
    lower_mean <- means$lower_mean
    upper_mean <- means$upper_mean
    attribute <- 1
   
  }else{
    s <- s_rm[rm_rss_list==rm_rss][1]
    index_values <- get_index(rm_values,s,y_train)
    means <- calc_mean(index_values,y_train)
    lower_mean <- means$lower_mean
    upper_mean <- means$upper_mean
    attribute <- 2
    
  }
  return(list("attribute"=attribute,"s"=s,"upper_mean"=upper_mean,"lower_mean"=lower_mean))
}

#BDS Algorithm implementation 
BDS <- function(x_train,y_train,B,lr){
  r_i <- y_train
  f_caps <- matrix(0, ncol = length(y_train),nrow =B ) 
  ds_values = list()
  for(i in 1:B){
    ds <- DS(x_train,r_i)
    f_caps[i,] <- predict_yhat(ds,x_train[,ds$attribute])
    ds_values <- cbind(ds_values, ds)
    r_i <- r_i - lr*f_caps[i, ]
  }
  return(ds_values)
}

#function to calculate test mse for BDS
BDS_test_mse <-function(ds_values,x_test,y_test,lr){
  y_hat <-  matrix(0, ncol = ncol(ds_values),nrow =length(y_test) ) 
  for(i in 1:ncol(ds_values)){
    y_hat[,i] <- predict_yhat(ds_values[, i],x_test[,ds_values[, i]$attribute])
  }
  y_pred <- rowSums(y_hat*lr)
  mse <- mean((y_pred - y_test)^2)
  return(mse)
}

#function to plot MSE vs B
plot_mse <- function(ds_values,x_test,y_test,lr){
  mse_values <- rep(0,length(points))
  for(i in 1:length(points)){
    mse_values[i] <- BDS_test_mse(ds_values[,1:points[i]],x_test,y_test,lr)
  }
  plot(points,mse_values,type='l',ylab = 'Test_MSE_value',xlab = 'B value',main='MSE vs B plot for learning rate = 0.01')
}


#creating a data frame with requires attributes
x_values <- cbind(lstat,rm)

#seed value in the form MMDD
set.seed(731)


train <- sample(1:nrow(x_values),nrow(x_values)/2)

#splitting the data set into train and test
x_train <- x_values[train,]
x_test <- x_values[-train,]
y_train <- medv[train]
y_test <- medv[-train]


ds_list <- DS(x_train,y_train)


test_MSE <- calc_test_MSE(ds_list,x_test[,ds_list$attribute],y_test)
cat(sprintf("Test MSE for DS = %.3f", test_MSE))

ds_values <- BDS(x_train,y_train,B=1000,lr=0.01)

test_MSE = BDS_test_mse(ds_values,x_test,y_test,0.01)
cat(sprintf("\nTest MSE for BDS = %.3f", test_MSE))

points <- seq(15,10000,20)

ds_values_plot <-  BDS(x_train,y_train,B=10000,lr=0.01)

plot_mse(ds_values_plot,x_test,y_test,lr=0.01)




