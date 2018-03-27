# HW 2 


library(MASS)
library(glmnet)
library(ggplot2)
library(glmnet)

# you can write your own code to implement the Lasso
# or you can use the following structure
# in either case, you need to produce a sequence of solutions (of beta_0 and beta_j's) using the X and y defined above.


# now start to write functions to fit the lasso
# prepare the soft thresholding function for updating beta_j (part b)

soft_th <- function(b, lambda)
{
  if(b < 0 & abs(b) > lambda)
    b = b + lambda
  else if(b > 0 & abs(b) > lambda)
    b = b - lambda
  else
    b = 0
  b
}
# initiate lambda as the lambda_max value in part c)

lambda_max = 20

# produce a sequence of lambda values 
lambda = exp(seq(log(lambda_max), log(0.01), length.out = 100))

# if you use this formula, you will need to calculate this for the real data too.

LassoFit <- function(X, y, lambda, tol = 1e-5, maxiter = 100)
{
	# initiate objects to record the values
	mybeta = matrix(NA, ncol(X), length(lambda))
	mybeta0 = rep(NA, length(lambda))
	mylambda = rep(NA, length(lambda))
	nlambda = length(lambda)
	z = colSums(X^2)
	
	current_beta = matrix(0, P, 1)
	# current_beta0 = mean(y)
	
	for (l in 1:nlambda)
	{
	  
		# reduce the current lambda value to a smaller one
		current_lambda = lambda[l]
		
		for (k in 1:maxiter)
		{
		  old_beta = current_beta
			# update the intercept term based on the current beta values. 
			current_beta0 = mean(y - X %*% current_beta)
			
			# start to update each beta_j 
			
			for (j in 1:ncol(X))
			{
				# remove the effect of variable j from model, and compute the residual
			  current_beta_copy = current_beta
			  current_beta_copy[j, ] = 0
				r = y - current_beta0 - X %*% current_beta_copy
				
				# update beta_j using the results in part b)
				current_beta[j, ] = soft_th(sum(r * X[, j]), nrow(X)*current_lambda)/z[j]
				
			}
			
			# check if beta changed more than the tolerance level in this iteration (use tol as the threshold)
			# if not, break out of this loop k
			# you will need to record the beta values at the start of this iteration for comparison. 
			if(max(abs(old_beta-current_beta)) < tol) 
			  break
		}
		
		if(l %% 20 == 0)
		{
		  print(paste0(l, " step over"))
		}
		
		mylambda[l] = current_lambda
		mybeta[, l] = current_beta
		mybeta0[l] = current_beta0
	}
	
	return(list("beta" = mybeta, "b0" = mybeta0, "lambda" = mylambda))
}

# Data Creation step
N = 500
P = 200

Beta = c(seq(1, 0, length.out = 21), rep(0, P-21))
Beta0 = 0.5

# you must set this seed for generating the data
set.seed(1)

# generate X
V = matrix(0.5, P, P)
diag(V) = 1
X = as.matrix(mvrnorm(N, mu = rep(0, P), Sigma = V))

# generate Y
y = Beta0 + X %*% Beta + rnorm(N)

# check OLS
lm(y ~ X)

# now, perform the Lasso mode on the simulated dataset 
res = LassoFit(X, y, lambda)

# Test Data Creation step
N_tst = 1000
P_tst = 200

Beta_tst = c(seq(1, 0, length.out = 21), rep(0, P_tst-21))
Beta0_tst = 0.5

# you must set this seed for generating the data
set.seed(1)

# generate X test
V = matrix(0.5, P_tst, P_tst)
diag(V) = 1
X_tst = as.matrix(mvrnorm(N_tst, mu = rep(0, P_tst), Sigma = V))

# generate Y test
y_tst = Beta0_tst + X_tst %*% Beta_tst + rnorm(N_tst)

nlambdas = length(res$lambda)
res$rmse = rep(0, nlambdas)
for(i in 1:nlambdas)
{
  lmbda = res$lambda[i]
  betas = res$beta[, i]
  beta0 = res$b0[i]
  yhat = X_tst %*% betas
  yhat = yhat + beta0
  res$rmse[i] = sqrt(mean((y_tst - yhat)^2))
}

res$zeros = rep(0, length(res$lambda))
for (i in 1:nlambdas) {
  res$zeros[i] = sum(res$beta[, i]==0)
}

plot(res$lambda, res$zeros, xlab = "lambda", ylab = "number of zero betas", pch = 20)

plot(res$lambda, res$rmse, xlab="lambda", ylab="Test RMSE")

plot(res$lambda[70:100], res$rmse[70:100], xlab="lambda", ylab="Test RMSE", type = "b", main = "Test RMSE vs Lambda for lowest 50 values of lambda", pch = 20)
# lambda that obtains minimum RMSE
res$lambda[which(res$rmse == min(res$rmse))]

###############################################################################
######################  Question 2  ###########################################
###############################################################################


# Data Creation step
N = 500
P = 200

Beta = c(seq(1, 0, length.out = 21), rep(0, P-21))
Beta0 = 0.5

# you must set this seed for generating the data
set.seed(1)

# generate X
V = matrix(0.5, P, P)
diag(V) = 1
X = as.matrix(mvrnorm(N, mu = rep(0, P), Sigma = V))

# generate Y
y = Beta0 + X %*% Beta + rnorm(N)


# a
fit_lasso_cv = cv.glmnet(X, y, alpha = 1, nfolds = 10)
fit_lasso_cv$lambda
# lambda with minimum error or best lambda
best_lambda = fit_lasso_cv$lambda.min
# Number of non zero coefficients
nzero = fit_lasso_cv$nzero[which(fit_lasso_cv$lambda == fit_lasso_cv$lambda.min)]

# b
covariance = 0
y_original = matrix(0, N, 20)
y_hat = matrix(0, N, 20)
for (i in 1:20) {
  y_original[,i] = Beta0 + X %*% Beta + rnorm(N)
  lasso_fit = glmnet(X, y_original[,i], alpha = 1, lambda = best_lambda)
  y_hat[, i] = predict(lasso_fit, X)
}

for (i in 1:N) {
  covariance = covariance + cov(y_original[i,], y_hat[i,])  
}

covariance

# c

# Data Creation step
N = 500
P = 200

Beta = c(seq(1, 0, length.out = 21), rep(0, P-21))
Beta0 = 0.5

# you must set this seed for generating the data
set.seed(1)

# generate X
V = matrix(0.5, P, P)
diag(V) = 1
X = as.matrix(mvrnorm(N, mu = rep(0, P), Sigma = V))

# generate Y
y = Beta0 + X %*% Beta + rnorm(N)

fit_ridge_cv = cv.glmnet(X, y, alpha = 0, nfolds = 10)

# lambda with minimum error or best lambda
best_lambda_ridge = fit_ridge_cv$lambda.min

covariance = 0
y_original = matrix(0, N, 20)
y_hat = matrix(0, N, 20)
for (i in 1:20) {
  y_original[,i] = Beta0 + X %*% Beta + rnorm(N)
  ridge_fit = glmnet(X, y_original[,i], alpha = 0, lambda = best_lambda_ridge)
  y_hat[, i] = predict.glmnet(ridge_fit, X)
}

for (i in 1:N) {
  covariance = covariance + cov(y_original[i,], y_hat[i,])  
}
covariance

# Theoretical DF of ridge
Ident = matrix(0, P, P)
diag(Ident) = 1
xx = solve((t(X) %*% X) + best_lambda_ridge * Ident)
df_mat = X %*% xx %*% t(X)
sum(diag(df_mat))
