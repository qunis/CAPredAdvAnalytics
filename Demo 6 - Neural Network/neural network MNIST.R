library(keras)
mnist <- dataset_mnist()
x_train_raw <- mnist$train$x
y_train_raw <- mnist$train$y
x_test_raw <- mnist$test$x
y_test_raw <- mnist$test$y

rotate <- function(x) t(apply(x, 2, rev))
image(rotate(x_train_raw[sample(100,1),,]), col=grey.colors(50, start=1, end=0))

# reshape
x_train <- array_reshape(x_train_raw, c(nrow(x_train_raw), 784))
x_test <- array_reshape(x_test_raw, c(nrow(x_test_raw), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train_raw, 10)
y_test <- to_categorical(y_test_raw, 10)

model <- keras_model_sequential() 
model %>% 
    layer_dense(units = 16, activation = 'relu', input_shape = c(784)) %>% 
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 10, activation = 'softmax')

summary(model)


model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
)

history <- model %>% fit(
    x_train, y_train, 
    epochs = 30, batch_size = 128, 
    validation_split = 0.2
)

model %>% evaluate(x_test, y_test)

predict_model <- function(n){
    image(rotate(x_test_raw[n,,]), col=grey.colors(50, start=1, end=0))
    ans <- apply(predict_on_batch(model, t(x_test[n,])),1, which.max) -1
    text(.95,.92, ans, col="red", cex =5)
}
    
predict_model(sample(100,1))
