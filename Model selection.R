##################################################
#                  Data Mining                   #
#                                                #
#                 Narendar Kumar                 #
#                                                #
##################################################

# Part 1: Loading the data
## CSV Read Command
## "./" is command to locate in the working directory
house <- read.csv(file="./house_data.csv", 
                  header = TRUE, sep = ";")


# Part 2: Linear Fit
## Linear Fit
lm_fit = lm(price ~ ., data =house )
summary(lm_fit)


# Part 3: OLS function
## OLS ESTIMATION
library(matlab)
## Data is Housing Pricing
## Transforming Table in matrix
X=house[,(2:6)]
x=as.matrix(X)
Y=house[1]
y=as.matrix(Y)
## Default for Intercept in the linear fit
intercept="TRUE"
## Function
OLS=function(x,y, intercept=TRUE){
  if (intercept==TRUE){
    ones = matrix(1, nrow=nrow(x),ncol=1) ## Column of 1 for intercept
    x=cbind(ones,x)
    Beta = solve(t(x)%*%x)%*%t(x)%*%y ## OLS Beta_hat
    variablenames=colnames(house) ## Variable names
    variablenames[1]="intercept"
    rownames(Beta)=variablenames
    colnames(Beta)="Beta"
    y_hat= x%*%Beta ## Estimated Y
    residual= y-y_hat
    rss= t(residual)%*%(residual) ## Residual sum of Square formula
    return(list(Beta, rss))  ## Return Beta_hat and RSS
  
    } else{
    Beta = solve(t(x)%*%x)%*%t(x)%*%y ## Without intercept OLS Beta_hat
    variablenames= colnames(house)
    variablenames=variablenames[2:6]
    rownames(Beta)=variablenames
    colnames(Beta)="Beta"
    y_hat= x%*%Beta
    rss= t(y-y_hat)%*%(y-y_hat)
    return(list(Beta, rss))  
  }
}

## Testing
Beta_hat=OLS(x,y)
Beta_hat
Beta_hat=OLS(x,y, intercept = FALSE)
Beta_hat

# Part 3: Forward Selection
## Import Data
house <- data.frame(read.csv(file="./house_data.csv", sep = ";"))
attach(house)
## At no estimators
null= lm(price ~ 1, data = house) ## Regression on intercept only.
adj_R_null=summary(null)$adj.r.squared ## RSS
## Variable for Loop
{adj_R= c() ## Adjusted R_square
max_R= c() 
loop= c(1:5) ## Our loop (Page 207)
k=1 ## Instead of "0" we are starting from 1 (since our data-set starts from 2)
l=5 ## Last Variable
b=0 ## Vector of indexes of our important estimators
}

## R-Square will be saved and deleted continuouly untill loops ends
for (j in 1:l){
  housing=c()
  adj_R=c()
  d=1
  ## Linear Fit for our good estimatrs with other arbitory estimators.
  ## First We have invidual estimator and that with maximum Rsquare
  ## will used again for the estimation during the loop to find further
  ## good estimators (b).
  for (i in loop){
    housing <- as.data.frame(house[,c(b,i+1)])
    lm_fit= lm(house[,1] ~ ., data = housing)
    adj_R[d]= summary(lm_fit)$adj.r.squared
    d=d+1
  }
  ## Recursive sorting for good estimators
  max_R[k] = max(adj_R)
  b[k]= (loop + 1)[which.max(adj_R)]
  loop= loop[-b[k]]
  k=k+1
}
## Rsquare for set of each good estimators
max_R
b
max(max_R)

## Final Estimators and Our Fit using Forward Model
estimators= b[1:which.max(max_R)]
housing=house[,c(estimators)]
Forwad_Model= lm(house[,1] ~ ., data = housing)
mannual_r= summary(Forwad_Model)$adj.r.squared

## For Testing
## R Built-in Step function for Forward Stepwise function
fit_zero= lm(price ~ 1, data = house)
fit_full= lm(price ~ ., data= house)
Fwd_fit=step(fit_zero, scope=list(lower=fit_zero, upper=fit_full), direction="forward")
step_r=summary(Fwd_fit)$adj.r.squared

## Model's Rsquare == Function's Square 
(mannual_r==step_r)

#### Part 5: Backward Selection ####
## Import Data
house <- data.frame(read.csv(file="./house_data.csv", sep = ";"))
attach(house)

## With no estimator and with all
fit_zero= lm(price ~ 1, data = house)
adj_R_zero=summary(fit_zero)$adj.r.squared ## RSS
fit_full= lm(price ~ ., data= house)
adj_R_full=summary(fit_full)$adj.r.squared ## RSS

## Variable for Loop
{adj_R= c() ## Adjusted R_square
  max_R= c() 
  k=1 ## Instead of "0" we are starting from 1 (since our data-set starts from 2)
  l=5 ## Last Variable
  b=1:5 ## Vector of indexes of our all estimators
  variables= c() ## Estimators which are being left out from the fit
}

## Backward Step
## R-Square will be saved and deleted continuouly untill loops ends
for (j in 1:4){
  housing=c()
  adj_R=c()
  d=1
  ## Linear Fit for our good estimatrs with all other estimators.
  ## First We have all estimators but some will be removed 
  ## one by one from the estimation during the loop to find 
  ## only good estimators
  for (i in 1:length(b)){
    housing <- as.data.frame(house[,(b+1)][-i])
    lm_fit= lm(house[,1] ~ ., data = housing)
    adj_R[d]= summary(lm_fit)$adj.r.squared
    d=d+1
  }
  ## Recursive sorting for good estimators
  max_R[k] = max(adj_R)
  variables[k]= b[which.max(adj_R)]
  b= b[!b %in% which.max(adj_R)]
  k=k+1
}

## Estimation
lm_fit= lm(house[,1] ~ house[,(b+1)])
max_R[k]= summary(lm_fit)$adj.r.squared
## Rsquare for set of each good estimators
max_R
max(max_R)

## Final Estimators and Our Fit using Forward Model
estimators= variables[1:which.max(max_R)]
estimators
housing=house[,-(c(estimators,0)+1)]
Backward_Model= lm(house[,1] ~ ., data = housing)
mannual_r2= summary(Backward_Model)$adj.r.squared

## For Testing
## R Built-in Step function for Forward Stepwise function
fit_zero= lm(price ~ 1, data = house)
fit_full= lm(price ~ ., data= house)
Backward_fit=step(fit_full, data=house, direction="backward")
step_r2=summary(Backward_fit)$adj.r.squared

## Model's Rsquare == Function's RSquare 
(mannual_r2==step_r2)

#### Part 6: Concluding output (which models you choose based on forward stepwise selection and ####
# backward stepwise selection)

## Although, both models do suggest same result for our good estimators and provide the same
## Adjusted R Square
mannual_r
mannual_r2
(mannual_r==mannual_r2)

## Forward Option can work with lower number parameters along lower numbers of estimators
## rather backward requires higher level.
