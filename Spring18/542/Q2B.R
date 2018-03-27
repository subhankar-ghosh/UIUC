train = read.csv("fashion-mnist_train.csv")
test = read.csv("fashion-mnist_test.csv")

alpha = 0.04

X = train[, -1]
Y = train[, 1]

Xtest = test[, -1]
Ytest = test[, 1]

P = ncol(X)
N = nrow(X)

# Prior Prob
prior = matrix(NA, 10, 1)
muq = matrix(0,  ncol(X),length(unique(Y)))
sigma_k <- list()

for(c in 0:9)
{
  prior[c+1] = nrow(X[which(Y == c), ])/nrow(X)
  
  XX = X[which(Y == c), ]
  muq[,c+1 ] = colMeans(XX) 
  sigma_k[[c +1]] <- cov(XX)
}

covar <- Reduce("+", sigma_k)*(N/10-1)/(N-P)
vals <- list()
#covar = covar 
for (i in 0:9)
{
  reg <- alpha*sigma_k[[i+1]] + (1-alpha)*covar
  inv <- solve(reg)
  w_k_inv <- -0.5*inv
  w_k<- inv%*% muq[,i+1]
  deter <- sum(log(eigen(reg)$values))
  b_k <- -0.5*t(muq[,i+1])%*% inv%*% muq[,i+1] + log(prior[i+1,1]) - 0.5*deter 
  vals[[i+1]]<- list(w_k_inv,w_k, b_k)
  
  
dim(Xtest)

# Result
pred = rep(0, nrow(Xtest))

for(i in 1:nrow(test))
{ 
  y<- as.matrix(test[i,-1])
  r <- rep(NA,10)
  for (i in 0:9)
  {
    r[i+1] <-y%*%vals[[i+1]][[1]]%*%t(y) + y%*% vals[[i+1]][[2]] + vals[[i+1]][[3]] 
    print(r)
  }
  pred[i]<- which(r == max(r)) -1
  
}

qda_acc = 1-misclassification(pred, Ytest)
