library(mlbench)
library(ggplot2)
library(reshape2)
library(leaps)
library(foreach)

# Question 1

## (a)

data("BostonHousing")
summary(BostonHousing)

# min max median mean
s=summary(BostonHousing)
s[c(1,3,4,6),]

# Correlation Plot
corr = round(cor(BostonHousing[,-4]), 2)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

corrplt <- get_lower_tri(corr)
corrplt = melt(corrplt)
# Correlation plot
corrheatmap = ggplot(data = corrplt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", na.value = 'white', 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

corrheatmap + geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

## (b)

n = nrow(BostonHousing)
lmfit = lm(medv ~ ., data = BostonHousing)
step(lmfit, direction="both", k=log(n), trace=0)

## (c)

step(lmfit, direction="forward", k=2, trace=0)

bb = BostonHousing
RSSleaps=regsubsets(medv ~ ., data = BostonHousing, nvmax = NULL, method = "backward")
sumleaps = summary(RSSleaps, matrix = T)
sumleaps$outmat
sumleaps$cp


# Question 2

## (b)
trn = read.csv("fashion-mnist_train.csv")
tst = read.csv("fashion-mnist_test.csv")

y_test = tst$label
x_test = data.matrix(tst[, -1])
x_test = matrix(x_test, nrow = 10000, ncol = 784)

dim(trn)
y_train = trn$label
x_train = data.matrix(trn[, -1])
x_train = matrix(x_train, nrow = 60000, ncol = 784)

dim(x_test)
dim(x_train)

get_misclassification <- function(pred, actual) {
  mean(pred != actual)
}


myknn <- function(k, x_test, x_train, y_test, y_train)
{
  for(i in 1:nrow(x_test))
  {
    ## Euclidean distance
    one_dist = sqrt(colSums((t(x_train) - x_test[i,])^2))
    one_df = as.data.frame(one_dist)
    a = y_train[head(sort(one_df$one_dist, index.return = TRUE, decreasing = FALSE)$ix, k)]
    y_pred[i] = a[max(table(a))]
  }
  y_pred
}

## (c)

k_range = c(4,5,7,14,21,28,35,42,49)
y_pred = rep(x=0, times=length(k_range))
acc = rep(0, times=length(k_range))
for j in range(1:length(k_range))
{
  y_pred = myknn(k_range[j], x_test, x_train, y_test, y_train)
  acc[j] = 1-get_misclassification(y_pred, y_test)
}

res = data.frame(k = k_range, Accuracy = acc)

ggplot(data = res, aes(x=k, y=Accuracy)) +
  geom_line() + geom_point() + 
  theme_bw() + ggtitle("Test Accuracy vs k plot")




