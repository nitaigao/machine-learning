cost <- function(y, m, x, b) {
  result = (y - (m * x + b)) ** 2
  return(result)
}

error <- function(b, m, points) {
  totalError = 0;
  for (i in 1:length(samples$x)) { 
    x = samples$x[i] 
    y = samples$y[i] 
    totalError = totalError + cost(y, m, x, b)
  }
  averageError = totalError / length(points)
  return (averageError)
}


bestValues <- function(m, b, samples, rate) {
  m_gradient = 0
  b_gradient = 0
  N = length(samples$x)
  for (i in 1:length(samples$x)) {
    x = samples$x[i] 
    y = samples$y[i] 
    b_gradient = b_gradient + -(2/N) * (y - ((m * x) + b))
    m_gradient = m_gradient + -(2/N) * x * (y - ((m * x) + b))
  } 
  new_m = m - (rate * m_gradient)
  new_b = b - (rate * b_gradient)
  return (list(m=new_m, b=new_b))
  #return (c(new_m, new_b))
}

runRate <- function(m, b, samples, steps, rate) {
  errors = c()
  ms = c()
  bs = c()
  for (i in 0:steps) {
    result = bestValues(m, b, samples, rate)
    m = result$m
    b = result$b
    e = error(b, m, samples)
    #print(c(m, b, e))
    errors = c(errors, e)
    ms = c(ms, m)
    bs = c(bs, b)
  }
  
  return(list(errors=errors, ms=ms, bs=bs, m=m, b=b))
}

samples <- read.table("/Users/nk/Desktop/samples.csv", header=TRUE, sep=",")
result <- runRate(-1.0, 0.0, samples, 300, 0.06)

layout(matrix(c(1, 1, 2, 2), 2, 2))
plot(result$errors)
plot(samples) 

points = c()

for (x in 1:length(samples$x)) {
  y = x * result$m + result$b;
  points = c(points, y)
}

lines(points)