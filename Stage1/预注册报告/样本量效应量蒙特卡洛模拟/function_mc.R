r2_mc <- function(power, n_sims, n_s, beta, intercept, R.sq) {
  i <- 2
  p_vals <- c()
  power_at_n <- c(0)
  while (power_at_n[i - 1] < power) {
    for (sim in 1:n_sims) {
      df <- data.frame(x1 = rnorm(n_s))
      var.epsilon <- (beta^2) * (1 - R.sq) / R.sq
      df$epsilon <- rnorm(n_s, sd = sqrt(var.epsilon))
      df$y <- with(df, intercept + beta * x1 + epsilon)
      m <- lm(y ~ x1, df)
      output <- summary(m)
      p_vals[sim] <- output[["coefficients"]][2, 4]
    }
    power_at_n[i] <- mean(p_vals < 0.05)
    n_s <- n_s + 1
    i <- i + 1
  }
  power_at_n <- power_at_n[-1]
  plot_power <- plot(5:(n_s - 1), power_at_n, xlab = "Number of participants", ylab = "Power", ylim = c(0, 1), axes = TRUE)
  abline(h = power, col = "red")
  plot_power
  return(paste("When the R square is", R.sq, " and the power reach", power, "the number of sample size is", n_s - 1))
}


############函数使用###############
r2_mc(power = 0.8, n_sims = 1000, n_s = 5, beta = 0.5, intercept = 0.5, R.sq = 0.3)
#power: expected power
#n_sims:number of simulation
#n_s: initial number of sample size
#beta: coefficient
#intercept
#R.sq: expected R square
