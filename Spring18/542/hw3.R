library(quadprog)
library(ggplot2)
library(e1071)
library(MASS)
# Question 3
### a
set.seed(1)
n = 40
p = 2
xpos = matrix(rnorm(n*p ,mean=0,sd=1), n, p)
xneg = matrix(rnorm(n*p ,mean=4,sd=1), n, p)
x = rbind(xpos, xneg)
y = matrix(c(rep(1, n), rep(-1, n)))

ggplot(data=data.frame(x1=x[,1], x2=x[,2], y=y), aes(x=x1, y=x2, color=as.factor(y))) +
  geom_point()

G = (y %*% t(y))*(x %*% t(x))
diag(G) = diag(G) + 1e-5

f = rep(1, n*2)

A1 = t(y)
A2 = diag(n*2)
A = rbind(A1, A2)

b = rep(0, n*2 + 1)

best = solve.QP(Dmat = G, dvec = f, Amat = t(A), bvec = b, meq = 1)


beta = t(best$solution * y) %*% x
sv_ind = which(best$solution > 0)
sv_x = x[sv_ind, ]
sv_y = y[sv_ind]
z = sv_x %*% t(beta)
beta0 = -(max(z[which(sv_y==-1)]) + min(z[which(sv_y==1)]))/2

svm_e = svm(x=x, y=as.factor(y), method = "C-classification", scale = FALSE, kernel = 'linear')
w = t(svm_e$coefs) %*% svm_e$SV
b0 = -svm_e$rho


plot(x = x[,1], y = x[,2], col = as.factor(y), pch = 19, xlab = "x1", ylab = "x2")
abline(a = -beta0/beta[2], b = -beta[1]/beta[2], lty = 1, lwd = 2)
abline(a = -b0/w[2], b = -w[1]/w[2], lty = 4, lwd = 1, col = "blue")


### b
set.seed(1)
n = 40
p = 2
xpos = matrix(rnorm(n*p ,mean=0,sd=1), n, p)
xneg = matrix(rnorm(n*p ,mean=1.8,sd=1), n, p)
x = rbind(xpos, xneg)
y = matrix(c(rep(1, n), rep(-1, n)))


G = (y %*% t(y))*(x %*% t(x))
diag(G) = diag(G) + 1e-5

f = rep(1, n*2)

A1 = t(y)
A2 = diag(n*2)
A3 = -diag(n*2)
A = rbind(A1, A2, A3)

C = 10
b = c(rep(0, n*2 + 1), rep(-C, n*2))


best = solve.QP(Dmat = G, dvec = f, Amat = t(A), bvec = b, meq = 1)


beta = t(best$solution * y) %*% x
sv_ind = which(best$solution > 0)
sv_x = x[sv_ind, ]
sv_y = y[sv_ind]
z = sv_x %*% t(beta)
beta0 = -(max(z[which(sv_y==-1)]) + min(z[which(sv_y==1)]))/2

svm_e = svm(x=x, y=as.factor(y), method = "C-classification", scale = FALSE, kernel = 'linear', cost = 10)
w = t(svm_e$coefs) %*% svm_e$SV
b0 = -svm_e$rho


plot(x = x[,1], y = x[,2], col = as.factor(y), pch = 19, xlab = "x1", ylab = "x2")
abline(a = -beta0/beta[2], b = -beta[1]/beta[2], lty = 1, lwd = 2)
abline(a = -b0/w[2], b = -w[1]/w[2], lty = 4, lwd = 2, col = "blue")


# Question 2
trn = read.csv("fashion-mnist_train.csv")
tst = read.csv("fashion-mnist_test.csv")

############################### FULL ##############################################

X = trn[, -1]
Y = trn[, 1]
dim(X)

Xtest = tst[, -1]
Ytest = tst[, 1]
dim(Xtest)

####################################################################################

misclassification = function(pred, actual)
{
  mean(pred!=actual)
}
############################### DUMMY ##############################################

findRows <- function(zip, n) {
  # Find n (random) rows with zip representing 0,1,2,...,9
  res <- vector(length=10, mode="list")
  names(res) <- 0:9
  ind <- zip[,1]
  for (j in 0:9) {
    res[[j+1]] <- sample( which(ind==j), n ) }
  return(res) 
}


# find 40 samples for each digit for both training and testing data
train.id <- findRows(trn, 300)
train.id = unlist(train.id)

test.id <- findRows(tst, 40)
test.id = unlist(test.id)

X = trn[train.id, -1]
Y = trn[train.id, 1]
dim(X)

Xtest = tst[test.id, -1]
Ytest = tst[test.id, 1]
dim(Xtest)

#########################################################################

P = ncol(X)
N = nrow(X)

# Prior Prob
prior = rep(0, 10)
for(c in unique(Y))
{
  prior[c+1] = nrow(X[which(Y == c), ])/nrow(X)
}
# Mean
mu = matrix(0, length(unique(Y)), ncol(X))
for(c in unique(Y))
{
  XX = X[which(Y == c), ]
  mu[c+1, ] = colMeans(XX) 
}
# Covariance matrix
cov = matrix(0, P, P)
for(c in unique(Y))
{
  XX = X[which(Y == c), ] - mu[c+1, ] # n*P
  cov = cov + t(as.matrix(XX)) %*% as.matrix(XX)
}
cov = cov / (N - 10)
invcov = solve(cov)
# w and b for each class
w = matrix(0, 10, P)
b = matrix(0, 10, 1)
for(c in unique(Y))
{
  w[c+1, ] = mu[c+1, ] %*% invcov
  b[c+1] = -0.5 * w[c+1, ] %*% mu[c+1, ] + log(prior[c+1])
}

res = t(as.matrix(Xtest) %*% t(w))
res = res + b[, 1]
dim(res)


pred = rep(0, ncol(res))
for(i in 1:ncol(res))
{
    pred[i] = which.max(res[, i])-1
}

my.lda.acc = 1-misclassification(pred, Ytest)

dig.lda=lda(X,Y)
Ytest.pred=predict(dig.lda, Xtest)$class
table(Ytest, Ytest.pred)
1-mean(Ytest != Ytest.pred)

## b ###########################################
############# scaling ##########################
means = colMeans(X)
X = scale(X, center = means, scale = TRUE)
Xtest = scale(Xtest, center = means, scale = TRUE)
X[is.nan(X)] = 0
Xtest[is.nan(Xtest)] = 0
################################################
P = ncol(X)
N = nrow(X)

# Prior Prob
priorq = rep(0, 10)
for(c in unique(Y))
{
  priorq[c+1] = nrow(X[which(Y == c), ])/nrow(X)
}
# Mean
muq = matrix(0, length(unique(Y)), ncol(X))
for(c in unique(Y))
{
  XX = X[which(Y == c), ]
  muq[c+1, ] = colMeans(XX) 
}

# Covariance matrix
covq = lapply(1:10, function(x) matrix(0, P, P))
invcovq = lapply(1:10, function(x) matrix(0, P, P))
detr = rep(0, 10)
covar = matrix(0, P, P)
alpha = 0.4

for(c in unique(Y))
{
  # XX = t(t(X[which(Y == c), ]) - muq[c+1, ]) # n*P
  XX = X[which(Y == c), ]
  covq[[c+1]] = cov(XX) # t(as.matrix(XX)) %*% as.matrix(XX)
  covar = covar + covq[[c+1]]
  covq[[c+1]] = covq[[c+1]] / (nrow(XX) - 1)
}

covar = covar / (N - 10)
for(c in unique(Y))
{
  covq[[c+1]] = (alpha*covq[[c+1]]) + ((1-alpha)*covar) # + (1e-5*diag(P))
  invcovq[[c+1]] = solve(covq[[c+1]])
  detr[c+1] = sum(log(svd(covq[[c+1]])$d))
}

det(invcovq[[1]])
detr[4]
dim(Xtest)

# Result
res = matrix(0, nrow(Xtest), 10)
for(c in unique(Y))
{
  x_t = (as.matrix(Xtest) %*% invcovq[[c+1]]) %*% t(as.matrix(Xtest))
  res[, c+1] = rowSums(x_t) + log(priorq[[c+1]]) - 0.5*log(abs(detr[c+1]))
}


pred = rep(0, nrow(res))
for(i in 1:nrow(res))
{
  pred[i] = which.max(res[i, ])-1
}

my.qda.acc = 1-misclassification(pred, Ytest)

dig.qda=qda(X,Y)
Ytest.pred=predict(dig.qda, Xtest)$class
table(Ytest, Ytest.pred)
1-mean(Ytest != Ytest.pred)