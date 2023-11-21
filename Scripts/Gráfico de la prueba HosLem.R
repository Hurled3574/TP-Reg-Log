#create density curve
x <- 3.4834
curve(dchisq(x, 8), from = 0, to = 40,
      main = 'Prueba de Bondad de Ajuste Hosmer-Lemeshow (gl = 8)', xlab = 'Estadístico C',
      ylab = 'Densidad de Probabilidad',
      lwd = 2)

#create vector of x values
x_teor <- seq(qchisq(0.95,8), 40)

#create vector of chi-square density values
p_vector <- dchisq(x_teor, 8)

#fill in portion of the density plot from 0 to 40
polygon(c(x_teor, rev(x_teor)), c(p_vector, rep(0, length(p_vector))),
        col = adjustcolor('red', alpha=0.3), border = NA)

#create vector of x values
x_obs <- seq(x, 40)

#create vector of chi-square density values
p_vector <- dchisq(x_obs, 8)

#fill in portion of the density plot from 0 to 40
polygon(c(x_obs, rev(x_obs)), c(p_vector, rep(0, length(p_vector))),
        col = adjustcolor('green', alpha=0.3), border = NA)
