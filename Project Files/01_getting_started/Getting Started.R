roll <- function(){
  sample(1:6, size = 1, replace = TRUE, prob = c(1/10, 1/10, 1/10, 1/10, 1/10, 5/10))
}

roll()
