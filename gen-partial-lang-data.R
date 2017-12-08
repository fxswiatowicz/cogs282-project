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
    # symbolic/nonsymbolic input tested 75% of the time,
    # when the nonsymbolic input presented is 1, the symbolic input is 1,
    # and when the nonsymbolic input presented is 2, the symbolic input is 2.
    if(i <= 7500 && j <= 1) {
      input.train[i+j *10000, 250+j+1] <- 1
    }
    # replicating the many system of the Piraha language,
    # where numbers greater than 2 are represented as 'many',
    # for this model, the third column of symbolic input is considered 'many'.
    else if(i <= 7500 && j > 1) {
      input.train[i+j *10000, 253] <- 1
    }
    # the first 10,000 rows of the output data are populated with 1 in the 
    # j+1'th column and so forth(to 100,000)
    output.train[i+j*10000, j+1] <- 1
  }