# function to add noise to the input data
add.noise <- function(v, prob){
  switch.p <- rbinom(length(v), 1, prob)
  for(i in 1:length(v)){
    if(switch.p[i] == 1){
      v[i] = 1-v[i]
    }
  }
  return(v)
}

# creating training data matrices
input.train <- matrix(0, nrow = 100000, ncol = 260)
output.train <- matrix(0, nrow = 100000, ncol = 10)

# populating matrices with values
for(j in 0:9)
  for(i in 1:10000) {
    # randomly assign (j+1) * 10 entries of row[i] with a value of 1
    input.train[i+j * 10000, sample(1:250, (j+1)*10)] <- 1
    # add noise to the data
    input.train[i+j*10000,] <- add.noise(input.train[i+j*10000,], 0.05)
    #symbolic/nonsymbolic input tested 75% of the time
    if(i <= 7500) {
      input.train[i+j *10000, 250+j+1] <- 1
    }
    output.train[i+j*10000, j+1] <- 1
  }