n_group <- 12

n_i <- 20

n_tot <- n_i * n_group


vect_op <- ceiling(runif(n_group, min = 0, max = 1e5))
j <- 0
time <- vector()
ID <- vector()
for (gr in 1:n_group){
  for (ind in 1:n_i){
    
    j <- j + 1
    ID[j] <- gr
    it <- 0
    tic()
    while (it <= vect_op[gr]){
      
      
      it <- it + 1
      a <- 3 * 4 - 1
      
      
    }
    b <- toc()
    time[j] <- b$toc - b$tic
  }
  }

data <- data.frame(cbind(time, ID))

fit <- lmer(time ~ 1 + (1|ID))
ranef(fit)
