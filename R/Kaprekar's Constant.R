kaprekar <- function(n, max_iter = 20) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n > 9999) {
    stop("n must be a single integer from 0 to 9999")
  }
  n <- as.integer(n)
  
  pad4  <- function(x) sprintf("%04d", x)
  digits <- function(x) strsplit(pad4(x), "")[[1]]
  
  one_step <- function(x) {
    d <- digits(x)
    asc  <- as.integer(paste0(sort(d), collapse = ""))
    desc <- as.integer(paste0(rev(sort(d)), collapse = ""))
    list(high = desc, low = asc, diff = desc - asc)
  }
  
  # If all digits identical, it goes to 0 immediately and never reaches 6174
  if (length(unique(digits(n))) == 1) {
    return(list(
      reached_6174 = FALSE,
      iterations = 0,
      steps = data.frame(iteration = integer(), high = integer(),
                         low = integer(), diff = integer())
    ))
  }
  
  steps <- data.frame(iteration = integer(),
                      high = integer(), low = integer(), diff = integer())
  
  cur <- n
  for (i in 1:max_iter) {
    s <- one_step(cur)
    steps[i, ] <- c(i, s$high, s$low, s$diff)
    cur <- s$diff
    if (cur == 6174) {
      return(list(reached_6174 = TRUE, iterations = i, steps = steps))
    }
  }
  
  list(reached_6174 = FALSE, iterations = max_iter, steps = steps)
}

jill <- function()
{
  ans <- c()
  
  for (i in 1:9998)
  {
    temp <- kaprekar(i)
    ans <- c(ans, temp$diff[nrow(temp)])
  }
  
  return(ans)
}

# Examples:
kaprekar(3524)   # should reach 6174 in 3 steps
kaprekar(1789)   # another example
kaprekar(2111)   # reaches 6174; shows each step
kaprekar(1111)   # all digits same -> never reaches 6174 (goes to 0)

