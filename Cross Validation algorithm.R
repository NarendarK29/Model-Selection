###########################################################
#                      Data Mining                        #
#                   Home Assignment 2                     #
#                       Your Names                        #
#                        Your IDs                         #
###########################################################
gen_reg=function(n){
  x=rnorm(n,0,1)
  u=rnorm(n,0,1)
  y= 1+x^3+u
  gen_reg=cbind.data.frame(y,x)
  return(gen_reg)
}
set.seed(90)
n=200
data = gen_reg(n)
train=sample(n, 0.8*n)

##plot(train_x,train_y)
plot(y ~ 1+x^3, data=data, subset = train)
curve(1+x^3, add= T)


#train_y=data[,1][train]
#train_x=data[,2][train]
##z=lm.fit$coefficients[-1]
##y_pred=sum(x[-train])*z+lm.fit$coefficients[1]

## EX_2(a)
MSE= function(actual, predicted){
  MSE= mean((actual - predicted)^2)
  return(MSE)
}
lm_fit= glm(y ~ 1+I(x^3), data=data, subset=train)
y_pred= predict(lm_fit, data)[-train]
y_act= data[,1][-train]
y_act
MSE(y_act, y_pred)

## EX_2(b)
library(boot)
my.kfold= function(model, data, k){
  if (nrow(data) %% k == 0)
    my.kfoldk = rep(0,k)
    for (i in 1:k){
      glm_fit= glm(y ~ 1+I(x^3), data=data)
      my.kfoldk[i]=cv.glm(data, model, K=k)$delta[1]
    }
    
  return(my.kfoldk)
}
my.kfold(glm_fit, data, k) ## K is multiple of 10 since nrow(data)=200

## EX_3
seednum = 1:100
n=200
MSE_training= c()
MSE_Val= c()
MSE_CV = c()

my.kfold <- function(data,K){
  cv.error <- c()
  if(nrow(data)%%K==0){
    kfold= sample(nrow(data), nrow(data) - nrow(data)/K, replace=FALSE)
    cv.error=glm(y ~ 1 + poly(x,j), data=data, subset = kfold)
  }else{
    print("Error change K")
    cv.error=NA
  } 
  return(cv.error)
}
