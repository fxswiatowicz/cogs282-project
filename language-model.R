# initializing keras and tensorflow for the neural network
# and ggplot2 for data visualization
library(keras)
library(ggplot2)
library(tidyr)
use_virtualenv('~/.virtualenvs/r-tensorflow')

source("gen-full-lang-data.R")
#source("gen-partial-lang-data.R")
source("gen-test-data.R")

# visualizing the distribution of numerical values in the input train matrix
sums <- apply(input.train[,1:250], 1, sum)
df <- data.frame(Activated.nodes = sums, Number=c(rep(1,10000),rep(2,10000),rep(3,10000),
                                                  rep(4,10000),rep(5,10000),rep(6,10000),
                                                  rep(7,10000),rep(8,10000),rep(9,10000),
                                                  rep(10,10000)))

df$Number <- factor(df$Number)
ggplot(df, aes(x=Activated.nodes,color=Number))+
  geom_density(bw=0.5)+
  scale_color_hue()+
  theme_bw()

# creating a shuffled.order variable that will be used to randomize the order
# of the input.train and output.train matrices. This is necessary for the
# validation_split argument in the fit method, because validation_split uses
# the last XX% of training data to set apart, and if the data is not randomized
# , accuracy and loss metrics cannot be checked on the validation data.
shuffled.order <- sample(nrow(input.train))
input.train <- input.train[shuffled.order,]
output.train <- output.train[shuffled.order,]

# Initializing the model with a hidden layer and output layer
language.model <- keras_model_sequential()
language.model %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(260)) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(language.model)

# compiling the model with loss, optimizer, and metrics parameters set
language.model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# training the model (10 epochs, batch size of 32)
history <- language.model %>% fit(
  input.train, output.train, 
  epochs = 10, batch_size = 32,
  verbose = 1,
  # validating the data using input and output test data sets
  validation_data = list(input.test, output.test)
)

# plotting the accuracy of training over time
plot(history)
# evaluating model performance on the test data sets
language.model %>% evaluate(input.test, output.test)


# predict the output of test data rows 6001-7000
predict.input <- language.model %>% predict_classes(input.test[6001:7000,])
predict.input


full.acc.results <- data.frame(accuracy = history$metrics$val_acc)
#partial.acc.results <- data.frame(accuracy = history$metrics$val_acc)
save(full.acc.results, file = "full-acc.Rdata")
#save(partial.acc.results, file = "partial-acc.Rdata")
#load("full-acc.Rdata")
#load("partial-acc.Rdata")

# acc <- data.frame(full.language.accuracy = full.acc.results$accuracy,
#                   partial.language.accuracy = partial.acc.results$accuracy)
#
#
# acc <- acc %>% gather('model','accuracy',1:2)
# acc$epoch <- c(1:10,1:10)
# ggplot(acc, aes(x=epoch, y = accuracy, color=model))+geom_line()+
#   scale_color_hue(), labels=c("Full Language", "Partial Language"))+
#   theme_bw()+
#   ggtitle("Accuracy of training with nonsymbolic input only")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   theme(text = element_text(size=13))+
#   labs(x = "Epoch")+
#   labs(y = "Accuracy")+
#   ylim(.3, .85)+
#   xlim(1, 10)

w <- language.model %>% get_weights()
image(w[[1]][251:260, 1:10])
image(w[[3]])
