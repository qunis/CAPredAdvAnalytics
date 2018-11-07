library(tidyverse)
library(caret)
library(Metrics)
#devtools::install_github('Zelazny7/isofor')
library(isofor)

# -------------------------------------------------
# Simulate a dataset
# -------------------------------------------------

outlier_freq <- .01

n <- 1000 # number of rows
j <- 20 # number of simulated columns
k <- 10 # number of random components


#rbinom(n, 1, outlier_freq) * rnorm(n, 100, 20)

# k normally distributed vectors with n points
klist <- list()
for(i in 1:k){
    klist[[i]] <- rnorm(n, 
                        runif(1, -10, 10) %>% round(),
                        runif(1, 0, 10) %>% round())
}
outlier_sim <- rbinom(n, 1, outlier_freq) * rnorm(n, 0, 5) + 1
klist[[1]] <- klist[[1]] * outlier_sim

m <- bind_cols(klist)

# j random linear combinations of the k vectors
jlist <- list()
for(i in 1:j){
    jlist[[i]] <-  as.matrix(m) %*% round(runif(k,0,10)) %>% 
        as.numeric
}

df <- bind_cols(jlist)

# -------------------------------------------------
# function definition
# -------------------------------------------------

# reverse pca with n principal components
rev_pca <- function(pca, n = 1){
    t(t(pca$x[,1:n] %*% t(pca$rotation[,1:n, drop =F])) * pca$scale + pca$center)    
}

# compute euclidean distance between original and prediction
pca_dist <- function(df, pca, n = 2){
    apply((df - rev_pca(pca, n))^2, 1 , sum) %>% sqrt()    
}

# compute weighted distance with all reproductions
pca_dist_weighted <- function(df, pca){
    w <- pca$sdev / sum(pca$sdev)
    sapply(seq_along(w), function(x) pca_dist(df, pca, x)) %*% w %>% 
        as.numeric()
}

# -------------------------------------------------
# Anomaly detection
# -------------------------------------------------

thresh <- .99   # the quantile threshhold on the error measure

# plot first two variables
df %>% mutate(outlier = outlier_sim != 1) %>% 
    ggplot(aes(V1, V3, color=outlier)) + geom_point()

# perform pca
pca <- prcomp(df, center = TRUE, scale = TRUE)

# how many prin.comps to use to reach threshhold of variance
n_pca <- which(cumsum(pca$sdev / sum(pca$sdev)) > thresh) %>% min()

# calculate error
error <- pca_dist(df, pca, 1)

# error boxplot with rising number of pcomps
outlier <- error > quantile(error, probs = thresh)

confusionMatrix(as.factor(outlier), 
                (outlier_sim!=1) %>% as.factor())

fit <- iForest(df)

pred <- (predict(fit, df) > .5) %>% as.factor()
confusionMatrix(pred, 
                (outlier_sim!=1) %>% as.factor())


pca$x %>% as_tibble() %>% 
    mutate(outlier = outlier) %>% 
    ggplot(aes(PC1, PC2, color=outlier)) + geom_point()


df %>%
    mutate(outlier = outlier) %>% 
    ggplot(aes(V1, V2, color=outlier)) + geom_point()

    
