library(mcmc)
library(tseries)
library(caret)

# data -----------------------------
set.seed(123)
trainIndex.Bayes = createDataPartition(mydata1$Damage.x, p = 0.7, list = FALSE)
data.training.Bayes = mydata1[trainIndex.Bayes, ]
data.testing.Bayes = mydata1[-trainIndex.Bayes, ]

n1 = data.training.Bayes$Damage.x
n2 = data.training.Bayes$Fire.x
n3 = data.training.Bayes$Other.x
n4 = data.training.Bayes$Theft.x
n5 = data.training.Bayes$TPL.x
n6 = data.training.Bayes$Windscreen.x
x1 = data.training.Bayes$DrivAge
x2 = data.training.Bayes$VehAge
x3 = data.training.Bayes$VehGas
x3 = ifelse(x3 == "Diesel", 0, 1)
x4 = data.training.Bayes$Region
x4 = ifelse(x4 == "Center or Headquarters", 0, 1)
x5 = data.training.Bayes$BonusMalus
x5 = ifelse(x5 == "Bonus", 0, 1)
x6 = data.training.Bayes$Garage
x6 = ifelse(x6 == "Closed parking", 0, 1)
x = rep(1, times = length(x1))
x = matrix(c(x, x1, x2, x3, x4, x5, x6), ncol = 7)
n = length(n1)
summary(n1)
summary(x)



n1.test = data.testing.Bayes$Damage.x
n2.test = data.testing.Bayes$Fire.x
n3.test = data.testing.Bayes$Other.x
n4.test = data.testing.Bayes$Theft.x
n5.test = data.testing.Bayes$TPL.x
n6.test = data.testing.Bayes$Windscreen.x
x1.test = data.testing.Bayes$DrivAge
x2.test = data.testing.Bayes$VehAge
x3.test = data.testing.Bayes$VehGas
x3.test = ifelse(x3.test == "Diesel", 0, 1)
x4.test = data.testing.Bayes$Region
x4.test = ifelse(x4.test == "Center or Headquarters", 0, 1)
x5.test = data.testing.Bayes$BonusMalus
x5.test = ifelse(x5.test == "Bonus", 0, 1)
x6.test = data.testing.Bayes$Garage
x6.test = ifelse(x6.test == "Closed parking", 0, 1)
x.test = rep(1, times = length(x1.test))
x.test = matrix(c(x.test, x1.test, x2.test, x3.test, x4.test, x5.test, x6.test), ncol = 7)
summary(n2.test)
summary(x.test)

# prior beta1, theta0, y_0 -------------
sd_41 = abs(log(mean(n4))/mean(x[, 1]))
sd_42 = abs(log(mean(n4))/mean(x[, 2]))
sd_43 = abs(log(mean(n4))/mean(x[, 3]))
sd_44 = abs(log(mean(n4))/mean(x[, 4]))
sd_45 = abs(log(mean(n4))/mean(x[, 5]))
sd_46 = abs(log(mean(n4))/mean(x[, 6]))
sd_46.init = abs(log(mean(n4))/mean(x))
sd_47 = abs(log(mean(n4))/mean(x[, 7]))
sd_51 = abs(log(mean(n5))/mean(x[, 1]))
sd_52 = abs(log(mean(n5))/mean(x[, 2]))
sd_53 = abs(log(mean(n5))/mean(x[, 3]))
sd_54 = abs(log(mean(n5))/mean(x[, 4]))
sd_55 = abs(log(mean(n5))/mean(x[, 5]))
sd_56 = abs(log(mean(n5))/mean(x[, 6]))
sd_57 = abs(log(mean(n5))/mean(x[, 7]))
sd_61 = abs(log(mean(n6))/mean(x[, 1]))
sd_62 = abs(log(mean(n6))/mean(x[, 2]))
sd_63 = abs(log(mean(n6))/mean(x[, 3]))
sd_64 = abs(log(mean(n6))/mean(x[, 4]))
sd_65 = abs(log(mean(n6))/mean(x[, 5]))
sd_66 = abs(log(mean(n6))/mean(x[, 6]))
sd_67 = abs(log(mean(n6))/mean(x[, 7]))


set.seed(123)
beta_41 = rnorm(1, mean = 0, sd = sd_41)
beta_42 = rnorm(1, mean = 0, sd = sd_42)
beta_43 = rnorm(1, mean = 0, sd = sd_43)
beta_44 = rnorm(1, mean = 0, sd = sd_44)
beta_45 = rnorm(1, mean = 0, sd = sd_45)
beta_46 = rnorm(1, mean = 0, sd = sd_46.init)
beta_47 = rnorm(1, mean = 0, sd = sd_47)
beta_51 = rnorm(1, mean = 0, sd = sd_51)
beta_52 = rnorm(1, mean = 0, sd = sd_52)
beta_53 = rnorm(1, mean = 0, sd = sd_53)
beta_54 = rnorm(1, mean = 0, sd = sd_54)
beta_55 = rnorm(1, mean = 0, sd = sd_55)
beta_56 = rnorm(1, mean = 0, sd = sd_56)
beta_57 = rnorm(1, mean = 0, sd = sd_57)
beta_61 = rnorm(1, mean = 0, sd = sd_61)
beta_62 = rnorm(1, mean = 0, sd = sd_62)
beta_63 = rnorm(1, mean = 0, sd = sd_63)
beta_64 = rnorm(1, mean = 0, sd = sd_64)
beta_65 = rnorm(1, mean = 0, sd = sd_65)
beta_66 = rnorm(1, mean = 0, sd = sd_66)
beta_67 = rnorm(1, mean = 0, sd = sd_67)
beta_4 = c(beta_41, beta_42, beta_43, beta_44, beta_45, beta_46, beta_47) 
# -1.45129226 -0.01504555  0.58579849  0.33151269  1.14419584  0.63197500  1.47789593
beta_5 = c(beta_51, beta_52, beta_53, beta_54, beta_55, beta_56, beta_57) 
# -0.77586976 -0.01063379 -0.03967054  1.36316658  0.75422345  7.05935451  0.08405834
beta_6 = c(beta_61, beta_62, beta_63, beta_64, beta_65, beta_66, beta_67) 
# -0.48284692   0.03918406   0.06276868  -3.10198592   2.08229446 -11.79557992  -1.14863510


sigma.beta4 = matrix(c(sd_41^2, 0, 0, 0, 0, 0, 0,
                       0, sd_42^2, 0, 0, 0, 0, 0,
                       0, 0, sd_43^2, 0, 0, 0, 0,
                       0, 0, 0, sd_44^2, 0, 0, 0,
                       0, 0, 0, 0, sd_45^2, 0, 0,
                       0, 0, 0, 0, 0, sd_46^2, 0,
                       0, 0, 0, 0, 0, 0, sd_47^2), nrow = 7)
sigma.beta5 = matrix(c(sd_51^2, 0, 0, 0, 0, 0, 0,
                       0, sd_52^2, 0, 0, 0, 0, 0,
                       0, 0, sd_53^2, 0, 0, 0, 0,
                       0, 0, 0, sd_54^2, 0, 0, 0,
                       0, 0, 0, 0, sd_55^2, 0, 0,
                       0, 0, 0, 0, 0, sd_56^2, 0,
                       0, 0, 0, 0, 0, 0, sd_57^2), nrow = 7)
sigma.beta6 = matrix(c(sd_61^2, 0, 0, 0, 0, 0, 0,
                       0, sd_62^2, 0, 0, 0, 0, 0,
                       0, 0, sd_63^2, 0, 0, 0, 0,
                       0, 0, 0, sd_64^2, 0, 0, 0,
                       0, 0, 0, 0, sd_65^2, 0, 0,
                       0, 0, 0, 0, 0, sd_66^2, 0,
                       0, 0, 0, 0, 0, 0, sd_67^2), nrow = 7)

a = 0.1
b = 0.1
theta_0 = rgamma(1e3, shape = a, rate = b) 
theta_0 = mean(theta_0) # 0.9795449


summary(pmin(n1, n2, n3, n4, n5, n6))
summary(pmin(n1, n2))
summary(pmin(n1, n3))
summary(pmin(n1, n4))
summary(pmin(n1, n5))
summary(pmin(n1, n6))
summary(pmin(n2, n4))
summary(pmin(n2, n5))
summary(pmin(n2, n6))
summary(pmin(n3, n4))
summary(pmin(n3, n5))
summary(pmin(n3, n6))
summary(pmin(n4, n5))
summary(pmin(n4, n6))
summary(pmin(n5, n6))

summary(pmin(n1, n3, n5))
summary(pmin(n1, n3, n6))
summary(pmin(n1, n4, n6))
summary(pmin(n3, n4, n6))
summary(pmin(n4, n5, n6))

summary(pmin(n1, n2, n3, n4))


y_0 = rpois(n, theta_0)
summary(y_0)
y_0 = pmin(pmax(y_0, 0), pmin(n4, n5, n6))  # pmin untuk menilai min setiap row dari data
# 0.0000000 0.0000000 0.0000000 0.0005616 0.0000000 1.0000000 

# likelihood y0
lg_y0 = function(theta0_init, y0_init, data_klaim1, data_klaim2, data_klaim3, x, 
                 beta1_init, beta2_init, beta3_init){
  theta1 = exp(beta1_init[1]*x[, 1] + beta1_init[2]*x[, 2] + beta1_init[3]*x[, 3] + 
                 beta1_init[4]*x[, 4] + beta1_init[5]*x[, 5] + beta1_init[6]*x[, 6] + beta1_init[7]*x[, 7])
  theta2 = exp(beta2_init[1]*x[, 1] + beta2_init[2]*x[, 2] + beta2_init[3]*x[, 3] + 
                 beta2_init[4]*x[, 4] + beta2_init[5]*x[, 5] + beta2_init[6]*x[, 6] + beta2_init[7]*x[, 7])
  theta3 = exp(beta3_init[1]*x[, 1] + beta3_init[2]*x[, 2] + beta3_init[3]*x[, 3] + 
                 beta3_init[4]*x[, 4] + beta3_init[5]*x[, 5] + beta3_init[6]*x[, 6] + beta3_init[7]*x[, 7])
  y0 = exp(
    y0_init*log(theta0_init) - log(factorial(y0_init)) - log(factorial(data_klaim1 - y0_init)) - 
      log(factorial(data_klaim2 - y0_init)) - log(factorial(data_klaim3 - y0_init)) - 
      y0_init*log(theta1) - y0_init*log(theta2) - y0_init*log(theta3)
  )
  y0_final = pmin(pmax(y0, 0), pmin(data_klaim1, data_klaim2, data_klaim3))
  return(y0_final)
}

# generate theta0
fungsi_theta0 = function(a, b, n, y0_init){
  theta0 = rgamma(1e3, shape = a+sum(y0_init), rate = b+n)
}


# log-likelihood beta
lg_beta = function(beta_init, x, n, y0, sigma){
  theta_i = exp(beta_init[1]*x[, 1] + beta_init[2]*x[, 2] + beta_init[3]*x[, 3] + 
                  beta_init[4]*x[, 4] + beta_init[5]*x[, 5] + beta_init[6]*x[, 6] + beta_init[7]*x[, 7])
  inv_sigma = solve(sigma)
  prior = -0.5 * t(beta_init) %*% inv_sigma %*% (beta_init)
  return(-sum(theta_i) + sum(log(theta_i)*(n-y0)) + prior)
}


# log-likelihood metrop
lg_metrop = function(n, x, y0, sigma) function(beta){
  
  loglik = lg_beta(beta_init = beta, x = x, n = n, y0 = y0, sigma = sigma)
  
  return(loglik - sum(beta^2)/8)
}


# Looping N kali -------------
N = 5

theta.0 = matrix(NA, nrow = 1e3, ncol = N)
theta0_out = matrix(NA, nrow = N, ncol = 5)

# tambah penyimpanan theta.0 dan theta0_out
theta.0 = cbind(theta.0, matrix(NA, nrow = 1e3, ncol = 5))
theta0_out = rbind(theta0_out, matrix(NA, nrow = 5, ncol = 5))

y0 = lg_y0(theta0_init = theta_0, y0_init = y_0, data_klaim1 = n4, data_klaim2 = n5, data_klaim3 = n6, x = x, 
           beta1_init = beta_4, beta2_init = beta_5, beta3_init = beta_6)
summary(y0)   # 0.0000000 0.0000000 0.0000000 0.0007289 0.0000000 1.0000000 

beta = matrix(NA, nrow = N+1, ncol = 21)
beta[1, 1:7] = beta_4
beta[1, 8:14] = beta_5
beta[1, 15:21] = beta_6
colnames(beta) = c("beta_41", "beta_42", "beta_43", "beta_44", "beta_45", "beta_46", "beta_47",
                   "beta_51", "beta_52", "beta_53", "beta_54", "beta_55", "beta_56", "beta_57", 
                   "beta_61", "beta_62", "beta_63", "beta_64", "beta_65", "beta_66", "beta_67")

# tambah row beta untuk menyimpan lanjutan 5 looping MCMC
beta = rbind(beta, matrix(NA, nrow = 5, ncol = 21))


conf.interval = matrix(NA, nrow = N, ncol = 42)
colnames(conf.interval) = c("beta_41.a", "beta_41.b", "beta_42.a", "beta_42.b", "beta_43.a", "beta_43.b", 
                            "beta_44.a", "beta_44.b", "beta_45.a", "beta_45.b", "beta_46.a", "beta_46.b", 
                            "beta_47.a", "beta_47.b",
                            "beta_51.a", "beta_51.b", "beta_52.a", "beta_52.b", "beta_53.a", "beta_53.b", 
                            "beta_54.a", "beta_54.b", "beta_55.a", "beta_55.b", "beta_56.a", "beta_56.b", 
                            "beta_57.a", "beta_57.b",
                            "beta_61.a", "beta_61.b", "beta_62.a", "beta_62.b", "beta_63.a", "beta_63.b", 
                            "beta_64.a", "beta_64.b", "beta_65.a", "beta_65.b", "beta_66.a", "beta_66.b",
                            "beta_67.a", "beta_67.b")

# tambahan row untuk conf.interval 5 looping tambahan MCMC
conf.interval = rbind(conf.interval, matrix(NA, nrow = 5, ncol = 42))


stasioner = matrix(NA, nrow = N, ncol = 21)

colnames(stasioner) = c("beta_41", "beta_42", "beta_43", "beta_44", "beta_45", "beta_46", "beta_47",
                        "beta_51", "beta_52", "beta_53", "beta_54", "beta_55", "beta_56", "beta_57", 
                        "beta_61", "beta_62", "beta_63", "beta_64", "beta_65", "beta_66", "beta_67")

# menyimpan hasil test stasioner di 5 looping pertama dan menambahkan penyimpanan stasioner
stasioner_1 = stasioner
stasioner = matrix("stasioner", nrow = N, ncol = 21)
stasioner = rbind(stasioner, matrix(NA, nrow = 5, ncol = 21))

colnames(stasioner) = c("beta_41", "beta_42", "beta_43", "beta_44", "beta_45", "beta_46", "beta_47",
                        "beta_51", "beta_52", "beta_53", "beta_54", "beta_55", "beta_56", "beta_57", 
                        "beta_61", "beta_62", "beta_63", "beta_64", "beta_65", "beta_66", "beta_67")


# test manual --------------
# n4 -------
set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)   # masalahnya ada di beta 46 yang terlalu besar dan semakin divergen
                                                     # makanya dibuat nilai beta 46 yg kecil dgn standar deviasi init /E[X]
out$accept   # 0.004
plot(ts(out$batch))
out = metrop(out, scale = 0.1)
out$accept   # 0.012

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
out = metrop(out, scale = 0.02)
out$accept   # 0.064

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
out = metrop(out, scale = 0.01)
out$accept   # 0.138

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
out = metrop(out, scale = 0.009)
out$accept   # 0.143

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
out = metrop(out, scale = 0.008)
out$accept   # 0.169

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.007)    # nnti setelah blen = 400, out$acceptnya cuma 0.1983875
out$accept   # 0.202
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0.007")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 1000, main = paste("Beta 4", i))
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 450, main = paste("Beta 4", i))
}
# blen = 400
out = metrop(out, blen = 450)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0.006")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 350, main = paste("Beta 4", i))
}
out$accept  # 0.1928533




set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 1e3)
out$accept   # 0.004
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.006)
out$accept   # 0.214
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0,006")
}

par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 4", i))
}
# blen = 400
out = metrop(out, blen = 400)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0,006")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 350, main = paste("Beta 4", i))
}
out$accept  # 0.2283175



set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:7], nbatch = 3e3)
out$accept   # 0.001333333
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.006)
out$accept   # 0.219
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 0,006")
}

par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 1200, main = paste("Beta 4", i-1))
}
par(mfrow = c(1, 1))
acf(out$batch[, 7], lag.max = 1200, main = paste("Beta 4", i-1))

# blen = 1200
out = metrop(out, blen = 1200)  # jam 11:38 - 2:14
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[500:3000, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 0,006")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 350, main = paste("Beta 4", i-1))
}
out$accept  # 0.2286286


adf.test(out$batch[500:3000, 1])
adf.test(out$batch[500:3000, 2])
adf.test(out$batch[500:3000, 3])
adf.test(out$batch[500:3000, 4])
adf.test(out$batch[500:3000, 5])
adf.test(out$batch[500:3000, 6])
adf.test(out$batch[500:3000, 7])
out$time





# n5 --------

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out$accept # 0.014
out = metrop(out, scale = 0.1)
out$accept # 0.034

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out$accept # 0.014
out = metrop(out, scale = 0.02)
out$accept # 0.098

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out$accept # 0.014
plot(ts(out$batch))
out = metrop(out, scale = 0.01)
out$accept # 0.183
plot(ts(out$batch))


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out$accept # 0.014
plot(ts(out$batch))
out = metrop(out, scale = 0.009)
out$accept # 0.2
plot(ts(out$batch))
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 5", i))
}
out = metrop(out, blen = 400)
plot(ts(out$batch))
plot(ts(out$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out$accept  # 0.0347725
out$blen
out$scale


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
plot(ts(out1$batch))
out1 = metrop(out1, scale = 0.008)
out1$accept # 0.204
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
out1 = metrop(out1, blen = 350)
out1$accept  # 0.04177429

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
plot(ts(out1$batch))
out1 = metrop(out1, scale = 0.007)
out1$accept # 0.232
plot(ts(out1$batch))
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 400, main = paste("Beta 5", i))
}
out1 = metrop(out1, blen = 400)
plot(ts(out1$batch))
plot(ts(out1$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.05182
out1$blen
out1$scale


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
plot(ts(out1$batch))
out1 = metrop(out1, scale = 0.005)
out1$accept # 0.282
plot(ts(out1$batch))
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 400, main = paste("Beta 5", i))
}
out1 = metrop(out1, blen = 400)
plot(ts(out1$batch))
plot(ts(out1$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.083325
out1$blen
out1$scale


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
plot(ts(out1$batch))
out1 = metrop(out1, scale = 0.004)
out1$accept # 0.297
plot(ts(out1$batch))
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
out1 = metrop(out1, blen = 350)
plot(ts(out1$batch))
plot(ts(out1$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.11372
out1$blen
out1$scale

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
plot(ts(out1$batch))
out1 = metrop(out1, scale = 0.003)
out1$accept # 0.329
plot(ts(out1$batch))
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 400, main = paste("Beta 5", i))
}
par(mfrow = c(1,1))
acf(out1$batch[, 7], lag.max = 350)
out1 = metrop(out1, blen = 400)
plot(ts(out1$batch))
plot(ts(out1$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.160585
out1$blen
out1$scale


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 1e3)
out1$accept # 0.014
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[, i]), ylab = paste("Beta 5", i))
  title("Time series plot of MCMC output with scale 1")
}
out1 = metrop(out1, scale = 0.002)
out1$accept # 0.406
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[, i]), ylab = paste("Beta 5", i))
  title("Time series plot of MCMC output with scale 0,002")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 400, main = paste("Beta 5", i))
}
par(mfrow = c(1,1))
acf(out1$batch[, 7], lag.max = 350)
out1 = metrop(out1, blen = 350)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[, i]), ylab = paste("Beta 5", i))
  title("Time series plot of MCMC output with scale 0,002")
}
plot(ts(out1$batch[200:1000, ]))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept    # 0.2474143
out1$scale
out1$blen
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.2474143
out1$blen
out1$scale


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out1 = metrop(lgMetrop, beta[1, 8:14], nbatch = 3e3)
out1$accept # 0.006666667
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out1 = metrop(out1, scale = 0.002)
out1$accept # 0.2783333
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0,002")
}
par(mfrow = c(4, 2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 1200, main = paste("Beta 5", i-1))
}
par(mfrow = c(1,1))
acf(out1$batch[, 7], lag.max = 1200)
out1 = metrop(out1, blen = 1200)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out1$batch[500:3000, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0,002")
}
plot(ts(out1$batch[500:3000, ]))

t.test(out1$batch[500:3000, 1])$conf.int  # -0.8262967 -0.8199038
t.test(out1$batch[500:3000, 2])$conf.int  # 0.001426171 0.001530576
t.test(out1$batch[500:3000, 3])$conf.int  # 0.007618245 0.007802276
t.test(out1$batch[500:3000, 4])$conf.int  # 0.06325629 0.06607714
t.test(out1$batch[500:3000, 5])$conf.int  # 0.09532984 0.09823827
t.test(out1$batch[500:3000, 6])$conf.int  # 0.2774261 0.2843092
t.test(out1$batch[500:3000, 7])$conf.int  # 0.02150656 0.02501205

quantile(out1$batch[500:3000, 1], c(0.025, 0.975)) # -0.9839831 -0.6630343 
quantile(out1$batch[500:3000, 2], c(0.025, 0.975)) # -0.001134834  0.004176903 
quantile(out1$batch[500:3000, 3], c(0.025, 0.975)) # 0.003150755 0.012453083 
quantile(out1$batch[500:3000, 4], c(0.025, 0.975)) # -0.005189284  0.136443987 
quantile(out1$batch[500:3000, 5], c(0.025, 0.975)) # 0.02441474 0.16978127 
quantile(out1$batch[500:3000, 6], c(0.025, 0.975)) # 0.1040119 0.4448434 
quantile(out1$batch[500:3000, 7], c(0.025, 0.975)) # -0.06522332  0.10854150 

hist(out1$batch[500:3000, 1])
hist(out1$batch[500:3000, 2])
hist(out1$batch[500:3000, 3])
hist(out1$batch[500:3000, 4])
hist(out1$batch[500:3000, 5])
hist(out1$batch[500:3000, 6])
hist(out1$batch[500:3000, 7])

par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept    # 0.2460283
out1$scale
out1$blen
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out1$batch[200:1000, i], lag.max = 350, main = paste("Beta 5", i))
}
out1$accept  # 0.2460283
out1$blen
out1$scale


# n6 ----
set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.1)
out$accept # 0.028

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.02)
out$accept # 0.082

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out$accept  # 0.005
plot(ts(out$batch))
out = metrop(out, scale = 0.01)
out$accept # 0.152
plot(ts(out$batch))

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.007)
out$accept # 0.179

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.006)
out$accept # 0.205
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 0,006")
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 6", i))
}
out = metrop(out, blen = 400)
out$accept   # 0.0789125



set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out2$accept  # 0.005
plot(ts(out2$batch))
out2 = metrop(out2, scale = 0.005)
out2$accept # 0.23
plot(ts(out2$batch))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 400, main = paste("Beta 6", i))
}
# sampai sini
out2 = metrop(out2, blen = 400)
plot(ts(out2$batch))
out2$blen
out2$scale
out2$accept # 0.1012275

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out2$accept  # 0.005
plot(ts(out2$batch))
out2 = metrop(out2, scale = 0.004)
out2$accept # 0.281
plot(ts(out2$batch))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 400, main = paste("Beta 6", i))
}
out2 = metrop(out2, blen = 400)
plot(ts(out2$batch))
out2$blen
out2$scale
out2$accept # 0.13441

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out2$accept  # 0.005
plot(ts(out2$batch))
out2 = metrop(out2, scale = 0.003)
out2$accept # 0.314
plot(ts(out2$batch))
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 400, main = paste("Beta 6", i))
}
out2 = metrop(out2, blen = 400)
plot(ts(out2$batch))
out2$blen
out2$scale
out2$accept # 0.187495

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 1e3)
out2$accept  # 0.005
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 1")
}
out2 = metrop(out2, scale = 0.002)
out2$accept # 0.351
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 0,002")
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 1000, main = paste("Beta 6", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 400, main = paste("Beta 6", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 350, main = paste("Beta 6", i))
}
par(mfrow = c(1, 1))
acf(out2$batch[, 1], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 2], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 3], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 4], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 5], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 6], lag.max = 350, main = paste("Beta 6", i))
acf(out2$batch[, 7], lag.max = 350, main = paste("Beta 6", i))
out2 = metrop(out2, blen = 350)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 0,002")
}
out2$blen
out2$scale
out2$accept # 0.2822943


adf.test(out2$batch[, 1])
adf.test(out2$batch[200:1000, 1])
adf.test(out2$batch[, 2])
adf.test(out2$batch[, 3])
adf.test(out2$batch[, 4])
adf.test(out2$batch[, 5])
adf.test(out2$batch[, 6])
adf.test(out2$batch[600:1000, 6])
adf.test(out2$batch[, 7])


set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 2e3)
out2$accept  # 0.0045
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 1")
}
out2 = metrop(out2, scale = 0.002)
out2$accept # 0.317
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 0,002")
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 2000, main = paste("Beta 6", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 700, main = paste("Beta 6", i))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 550, main = paste("Beta 6", i))
}
par(mfrow = c(1, 1))
acf(out2$batch[, 1], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 2], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 3], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 4], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 5], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 6], lag.max = 700, main = paste("Beta 6", i))
acf(out2$batch[, 7], lag.max = 700, main = paste("Beta 6", i))
out2 = metrop(out2, blen = 700)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[1000:2000, i]), ylab = paste("Beta 6", i))
  title("Time series plot of MCMC output with scale 0,002")
}
out2$blen
out2$scale
out2$accept # kalau blen = 700, accept = 0.2820157


set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out2 = metrop(lgMetrop, beta[1, 15:21], nbatch = 3e3)
out2$accept  # 0.003666667
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out2 = metrop(out2, scale = 0.002)
out2$accept # 0.3016667
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0,002")
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 3000, main = paste("Beta 6", i-1))
}
par(mfrow = c(4,2))
for (i in 1:7) {
  acf(out2$batch[, i], lag.max = 1200, main = paste("Beta 6", i-1))
}
par(mfrow = c(1, 1))
acf(out2$batch[, 1], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 2], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 3], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 4], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 5], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 6], lag.max = 1200, main = paste("Beta 6", i))
acf(out2$batch[, 7], lag.max = 1200, main = paste("Beta 6", i))
out2 = metrop(out2, blen = 1200)
par(mfrow = c(4, 2))
for (i in 1:7) {
  plot(ts(out2$batch[500:3000, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0,002")
}
out2$blen
out2$scale
out2$accept # kalau blen = 1200, accept = 0.2826175

adf.test(out2$batch[, 1])
adf.test(out2$batch[500:3000, 1])
adf.test(out2$batch[500:3000, 2])
adf.test(out2$batch[500:3000, 3])
adf.test(out2$batch[500:3000, 4])
adf.test(out2$batch[500:3000, 5])
adf.test(out2$batch[, 6])
adf.test(out2$batch[500:3000, 6])
adf.test(out2$batch[500:3000, 7])

# Looping ----------------------

out.batch.beta4 = matrix(NA, nrow = 3e3, ncol = 70)
out.batch.beta5 = matrix(NA, nrow = 3e3, ncol = 70)
out.batch.beta6 = matrix(NA, nrow = 3e3, ncol = 70)
colnames(out.batch.beta4) = c("b40.L1", "b41.L1", "b42.L1", "b43.L1", "b44.L1", "b45.L1", "b46.L1", 
                              "b40.L2", "b41.L2", "b42.L2", "b43.L2", "b44.L2", "b45.L2", "b46.L2", 
                              "b40.L3", "b41.L3", "b42.L3", "b43.L3", "b44.L3", "b45.L3", "b46.L3", 
                              "b40.L4", "b41.L4", "b42.L4", "b43.L4", "b44.L4", "b45.L4", "b46.L4", 
                              "b40.L5", "b41.L5", "b42.L5", "b43.L5", "b44.L5", "b45.L5", "b46.L5", 
                              "b40.L6", "b41.L6", "b42.L6", "b43.L6", "b44.L6", "b45.L6", "b46.L6", 
                              "b40.L7", "b41.L7", "b42.L7", "b43.L7", "b44.L7", "b45.L7", "b46.L7", 
                              "b40.L8", "b41.L8", "b42.L8", "b43.L8", "b44.L8", "b45.L8", "b46.L8", 
                              "b40.L9", "b41.L9", "b42.L9", "b43.L9", "b44.L9", "b45.L9", "b46.L9", 
                              "b40.L10", "b41.L10", "b42.L10", "b43.L10", "b44.L10", "b45.L10", "b46.L10")
colnames(out.batch.beta5) = c("b50.L1", "b51.L1", "b52.L1", "b53.L1", "b54.L1", "b55.L1", "b56.L1",
                              "b50.L2", "b51.L2", "b52.L2", "b53.L2", "b54.L2", "b55.L2", "b56.L2",
                              "b50.L3", "b51.L3", "b52.L3", "b53.L3", "b54.L3", "b55.L3", "b56.L3",
                              "b50.L4", "b51.L4", "b52.L4", "b53.L4", "b54.L4", "b55.L4", "b56.L4",
                              "b50.L5", "b51.L5", "b52.L5", "b53.L5", "b54.L5", "b55.L5", "b56.L5",
                              "b50.L6", "b51.L6", "b52.L6", "b53.L6", "b54.L6", "b55.L6", "b56.L6",
                              "b50.L7", "b51.L7", "b52.L7", "b53.L7", "b54.L7", "b55.L7", "b56.L7",
                              "b50.L8", "b51.L8", "b52.L8", "b53.L8", "b54.L8", "b55.L8", "b56.L8",
                              "b50.L9", "b51.L9", "b52.L9", "b53.L9", "b54.L9", "b55.L9", "b56.L9",
                              "b50.L10", "b51.L10", "b52.L10", "b53.L10", "b54.L10", "b55.L10", "b56.L10")
colnames(out.batch.beta6) = c("b60.L1", "b61.L1", "b62.L1", "b63.L1", "b64.L1", "b65.L1", "b66.L1",
                              "b60.L2", "b61.L2", "b62.L2", "b63.L2", "b64.L2", "b65.L2", "b66.L2",
                              "b60.L3", "b61.L3", "b62.L3", "b63.L3", "b64.L3", "b65.L3", "b66.L3",
                              "b60.L4", "b61.L4", "b62.L4", "b63.L4", "b64.L4", "b65.L4", "b66.L4",
                              "b60.L5", "b61.L5", "b62.L5", "b63.L5", "b64.L5", "b65.L5", "b66.L5",
                              "b60.L6", "b61.L6", "b62.L6", "b63.L6", "b64.L6", "b65.L6", "b66.L6",
                              "b60.L7", "b61.L7", "b62.L7", "b63.L7", "b64.L7", "b65.L7", "b66.L7",
                              "b60.L8", "b61.L8", "b62.L8", "b63.L8", "b64.L8", "b65.L8", "b66.L8",
                              "b60.L9", "b61.L9", "b62.L9", "b63.L9", "b64.L9", "b65.L9", "b66.L9",
                              "b60.L10", "b61.L10", "b62.L10", "b63.L10", "b64.L10", "b65.L10", "b66.L10")
out.accept.beta4 = matrix(NA, nrow = 10, ncol = 1)
out.accept.beta5 = matrix(NA, nrow = 10, ncol = 1)
out.accept.beta6 = matrix(NA, nrow = 10, ncol = 1)
colnames(out.accept.beta4) = c("accept.Beta_4")
colnames(out.accept.beta5) = c("accept.Beta_5")
colnames(out.accept.beta6) = c("accept.Beta_6")
rownames(out.accept.beta4) = c("Loop 1", "Loop 2", "Loop 3", "Loop 4", "Loop 5", "Loop 6", "Loop 7", "Loop 8", "Loop 9", "Loop 10")
rownames(out.accept.beta5) = c("Loop 1", "Loop 2", "Loop 3", "Loop 4", "Loop 5", "Loop 6", "Loop 7", "Loop 8", "Loop 9", "Loop 10")
rownames(out.accept.beta6) = c("Loop 1", "Loop 2", "Loop 3", "Loop 4", "Loop 5", "Loop 6", "Loop 7", "Loop 8", "Loop 9", "Loop 10")


for (i in 1:N) {
  
  # perbaharui nilai theta0
  set.seed(123)
  theta.0[, i] = fungsi_theta0(a = 0.1, b = 0.1, n = n, y0_init = y0)
  theta0_out[i, 1] = mean(theta.0[, i])
  theta0_out[i, 2:3] = t.test(theta.0[, i])$conf.int
  theta0_out[i, 4:5] = quantile(theta.0[, i], c(0.025, 0.975))
  
  
  # metropolis hasting beta 4
  set.seed(123)
  lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
  out = metrop(lgMetrop, beta[i, 1:7], nbatch = 3e3)
  out = metrop(out, scale = 0.006)
  out = metrop(out, blen = 1200)
  out.batch.beta4[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta4[i, ] = out$accept
  beta[i+1, 1:7] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 1:2] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 3:4] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 5:6] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 7:8] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 9:10] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 11:12] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 13:14] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 5
  set.seed(123)
  lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
  out = metrop(lgMetrop, beta[i, 8:14], nbatch = 3e3)
  out = metrop(out, scale = 0.002)
  out = metrop(out, blen = 1200)
  out.batch.beta5[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta5[i, ] = out$accept
  beta[i+1, 8:14] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 15:16] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 17:18] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 19:20] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 21:22] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 23:24] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 25:26] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 27:28] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j+7] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 6
  set.seed(123)
  lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
  out = metrop(lgMetrop, beta[i, 15:21], nbatch = 3e3)
  out = metrop(out, scale = 0.002)
  out = metrop(out, blen = 1200)
  out.batch.beta6[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta6[i, ] = out$accept
  beta[i+1, 15:21] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 39:30] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 31:32] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 33:34] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 35:36] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 37:38] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 39:40] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 41:42] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j+14] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # perbaharui nilai y0
  y0 = lg_y0(theta0_init = theta0_out[i, 1], y0_init = y0, data_klaim1 = n4, data_klaim2 = n5, data_klaim3 = n6, 
             x = x, beta1_init = beta[i+1, 1:7], beta2_init = beta[i+1, 8:14], beta3_init = beta[i+1, 15:21])
  
}
# jam 01:18 - 9:25
# jam 14:04 (senin) - 
# jam 3:50 (kamis) - 17:10 (Jumat)


# Looping 6:10 -------------
for (i in 6:10) {
  
  # perbaharui nilai theta0
  set.seed(123)
  theta.0[, i] = fungsi_theta0(a = 0.1, b = 0.1, n = n, y0_init = y0)
  theta0_out[i, 1] = mean(theta.0[, i])
  theta0_out[i, 2:3] = t.test(theta.0[, i])$conf.int
  theta0_out[i, 4:5] = quantile(theta.0[, i], c(0.025, 0.975))
  
  
  # metropolis hasting beta 4
  set.seed(123)
  lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
  out = metrop(lgMetrop, beta[i, 1:7], nbatch = 3e3)
  out = metrop(out, scale = 0.006)
  out = metrop(out, blen = 1200)
  out.batch.beta4[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta4[i, ] = out$accept
  beta[i+1, 1:7] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 1:2] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 3:4] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 5:6] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 7:8] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 9:10] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 11:12] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 13:14] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 5
  set.seed(123)
  lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
  out = metrop(lgMetrop, beta[i, 8:14], nbatch = 3e3)
  out = metrop(out, scale = 0.002)
  out = metrop(out, blen = 1200)
  out.batch.beta5[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta5[i, ] = out$accept
  beta[i+1, 8:14] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 15:16] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 17:18] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 19:20] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 21:22] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 23:24] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 25:26] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 27:28] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j+7] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 6
  set.seed(123)
  lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
  out = metrop(lgMetrop, beta[i, 15:21], nbatch = 3e3)
  out = metrop(out, scale = 0.002)
  out = metrop(out, blen = 1200)
  out.batch.beta6[, ((i-1)*7+1):(i*7)] = out$batch
  out.accept.beta6[i, ] = out$accept
  beta[i+1, 15:21] = apply(out$batch[500:3000, ], 2, mean)[1:7]
  conf.interval[i, 39:30] = t.test(out$batch[500:3000, 1])$conf.int
  conf.interval[i, 31:32] = t.test(out$batch[500:3000, 2])$conf.int
  conf.interval[i, 33:34] = t.test(out$batch[500:3000, 3])$conf.int
  conf.interval[i, 35:36] = t.test(out$batch[500:3000, 4])$conf.int
  conf.interval[i, 37:38] = t.test(out$batch[500:3000, 5])$conf.int
  conf.interval[i, 39:40] = t.test(out$batch[500:3000, 6])$conf.int
  conf.interval[i, 41:42] = t.test(out$batch[500:3000, 7])$conf.int
  
  for (j in 1:7) {
    result = adf.test(out$batch[500:3000, j]) 
    stasioner[i, j+14] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # perbaharui nilai y0
  y0 = lg_y0(theta0_init = theta0_out[i, 1], y0_init = y0, data_klaim1 = n4, data_klaim2 = n5, data_klaim3 = n6, 
             x = x, beta1_init = beta[i+1, 1:7], beta2_init = beta[i+1, 8:14], beta3_init = beta[i+1, 15:21])
  
}
# Sabtu, Jam 15.22 - Selasa, jam 12.46


out.accept.beta4
out.accept.beta5
out.accept.beta6
par(mfrow = c(3, 7))
for(i in 1:21){
  plot(ts(out.batch.beta4[500:3000, i]), ylab = paste("Beta 4", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(3, 7))
for(i in 22:42){
  plot(ts(out.batch.beta4[500:3000, i]), ylab = paste("Beta 4", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  plot(ts(out.batch.beta4[500:3000, i]), ylab = paste("Beta 4", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}

par(mfrow = c(3, 7))
for(i in 1:21){
  plot(ts(out.batch.beta5[500:3000, i]), ylab = paste("Beta 5", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(3, 7))
for(i in 22:42){
  plot(ts(out.batch.beta5[500:3000, i]), ylab = paste("Beta 5", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  plot(ts(out.batch.beta5[500:3000, i]), ylab = paste("Beta 5", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}

par(mfrow = c(3, 7))
for(i in 1:21){
  plot(ts(out.batch.beta6[500:3000, i]), ylab = paste("Beta 6", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(3, 7))
for(i in 22:42){
  plot(ts(out.batch.beta6[500:3000, i]), ylab = paste("Beta 6", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  plot(ts(out.batch.beta6[500:3000, i]), ylab = paste("Beta 6", ifelse(i %% 7 == 0, 6, (i-1) %% 7)))
}


par(mfrow = c(3, 7))
for(i in 1:21){
  acf(out.batch.beta4[, i], lag.max = 100, main = paste("Beta 4", i-1))
}
for(i in 22:42){
  acf(out.batch.beta4[, i], lag.max = 100, main = paste("Beta 4", i))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  acf(out.batch.beta4[, i], lag.max = 100, main = paste("Beta 4", i))
}

par(mfrow = c(3, 7))
for(i in 1:21){
  acf(out.batch.beta5[, i], lag.max = 100, main = paste("Beta 5", i))
}
for(i in 22:42){
  acf(out.batch.beta5[, i], lag.max = 100, main = paste("Beta 5", i))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  acf(out.batch.beta5[, i], lag.max = 100, main = paste("Beta 5", i))
}

par(mfrow = c(3, 7))
for(i in 1:21){
  acf(out.batch.beta6[, i], lag.max = 100, main = paste("Beta 6", i))
}
for(i in 22:42){
  acf(out.batch.beta6[, i], lag.max = 100, main = paste("Beta 6", i))
}
par(mfrow = c(4, 7))
for(i in 43:70){
  acf(out.batch.beta6[, i], lag.max = 100, main = paste("Beta 6", i))
}


par(mfrow = c(1, 1))

stasioner = factor(stasioner)
summary(stasioner)
theta0_out  # nilai theta 0 kecil karena nilai a = 0.1, tp b = 0.1 + n, jadi nilainya kecil bgt. Y0 = 0.
beta
conf.interval
plot(ts(theta0_out))
plot(ts(beta[2:11, 1]))

tes.normal = matrix(data = NA, nrow = 70, ncol = 3) 
for (i in 1:3) {
  for (j in 1:70) {
    tes.normal[j, i] = ifelse(i == 1,
                              ifelse(shapiro.test(out.batch.beta4[500:3000, j])$p.value > 0.05, "normal", "bukan normal"),
                              ifelse(i == 2, 
                                     ifelse(shapiro.test(out.batch.beta5[500:3000, j])$p.value > 0.05, "normal", "bukan normal"),
                                     ifelse(shapiro.test(out.batch.beta6[500:3000, j])$p.value > 0.05, "normal", "bukan normal")))
    
  }
}
shapiro.test(out.batch.beta5[500:3000, 58])$p.value
tes.normal


b11 = mean(beta[2:6, 1])  # -2.423725
b12 = mean(beta[2:6, 2])  # -0.008347399
b13 = mean(beta[2:6, 3])  # -0.05435635
b14 = mean(beta[2:6, 4])  # 0.291906
b15 = mean(beta[2:6, 5])  # 0.3371416
b16 = mean(beta[2:6, 6])  # -0.1644855
b17 = mean(beta[2:6, 7])  # 0.2766909
b21 = mean(beta[2:6, 8])  # -0.8152656
b22 = mean(beta[2:6, 9])  # 0.001352823
b23 = mean(beta[2:6, 10])  # 0.007520178
b24 = mean(beta[2:6, 11])  # 0.06364868
b25 = mean(beta[2:6, 12])  # 0.09493269
b26 = mean(beta[2:6, 13])  # 0.2732562
b27 = mean(beta[2:6, 14])  # 0.02420601
b31 = mean(beta[2:6, 15])  # -0.5307998
b32 = mean(beta[2:6, 16])  # -0.003141737
b33 = mean(beta[2:6, 17])  # 0.009260021
b34 = mean(beta[2:6, 18])  # -0.2431156
b35 = mean(beta[2:6, 19])  # -0.2217275
b36 = mean(beta[2:6, 20])  # -0.4962055
b37 = mean(beta[2:6, 21])  # -0.09969451

b11 = mean(beta[2:11, 1])   # -2.432091
b12 = mean(beta[2:11, 2])   # -0.008254826
b13 = mean(beta[2:11, 3])   # -0.05434446
b14 = mean(beta[2:11, 4])   # 0.2923239
b15 = mean(beta[2:11, 5])   # 0.3379092
b16 = mean(beta[2:11, 6])   # -0.1534303
b17 = mean(beta[2:11, 7])   # 0.2810991
b21 = mean(beta[2:11, 8])   # -0.8228182
b22 = mean(beta[2:11, 9])   # 0.001448828
b23 = mean(beta[2:11, 10])  # 0.007662449
b24 = mean(beta[2:11, 11])  # 0.06353139
b25 = mean(beta[2:11, 12])  # 0.09557502
b26 = mean(beta[2:11, 13])  # 0.273514
b27 = mean(beta[2:11, 14])  # 0.02699733
b31 = mean(beta[2:11, 15])  # -0.5296086
b32 = mean(beta[2:11, 16])  # -0.003138822
b33 = mean(beta[2:11, 17])  # 0.009201634
b34 = mean(beta[2:11, 18])  # -0.2428849
b35 = mean(beta[2:11, 19])  # -0.2234239
b36 = mean(beta[2:11, 20])  # -0.4848643
b37 = mean(beta[2:11, 21])  # -0.100691


b11 = beta[6, 1]  # -2.386241 
b12 = beta[6, 2]  # -0.008615374
b13 = beta[6, 3]  # -0.05446901
b14 = beta[6, 4]  # 0.2863628
b15 = beta[6, 5]  # 0.3336632
b16 = beta[6, 6]  # -0.1856063
b17 = beta[6, 7]  # 0.2518432
b21 = beta[6, 8]  # -0.8152656
b22 = beta[6, 9]  # 0.001352823
b23 = beta[6, 10]  # 0.007520178
b24 = beta[6, 11]  # 0.06364868
b25 = beta[6, 12]  # 0.09493269
b26 = beta[6, 13]  # 0.2732562
b27 = beta[6, 14]  # 0.02420601
b31 = beta[6, 15]  # -0.5307998
b32 = beta[6, 16]  # -0.003141737
b33 = beta[6, 17]  # 0.009260021
b34 = beta[6, 18]  # -0.2431156
b35 = beta[6, 19]  # -0.2217275
b36 = beta[6, 20]  # -0.4962055
b37 = beta[6, 21]  # -0.09969451

b11 = beta[11, 1]   # -2.458948 
b12 = beta[11, 2]   # -0.008173387
b13 = beta[11, 3]   # -0.05410094
b14 = beta[11, 4]   # 0.2915126
b15 = beta[11, 5]   # 0.3449095
b16 = beta[11, 6]   # -0.2011337 
b17 = beta[11, 7]   # 0.3063546 
b21 = beta[11, 8]   # -0.8366566 
b22 = beta[11, 9]   # 0.001626919 
b23 = beta[11, 10]  # 0.007806343 
b24 = beta[11, 11]  # 0.06448287
b25 = beta[11, 12]  # 0.09671416
b26 = beta[11, 13]  # 0.2908504
b27 = beta[11, 14]  # 0.03140096
b31 = beta[11, 15]  # -0.5233444
b32 = beta[11, 16]  # -0.003204345
b33 = beta[11, 17]  # 0.009065148
b34 = beta[11, 18]  # -0.2414627
b35 = beta[11, 19]  # -0.2268762
b36 = beta[11, 20]  # -0.4735193
b37 = beta[11, 21]  # -0.1040199


quantile(out.batch.beta4[500:3000, 64], c(0.025, 0.975)) # -2.891958 -2.042010 
quantile(out.batch.beta4[500:3000, 65], c(0.025, 0.975)) # -0.0154579406 -0.0005393342
quantile(out.batch.beta4[500:3000, 66], c(0.025, 0.975)) # -0.06718784 -0.04058722 
quantile(out.batch.beta4[500:3000, 67], c(0.025, 0.975)) # 0.1080769 0.4823444 
quantile(out.batch.beta4[500:3000, 68], c(0.025, 0.975)) # 0.1519976 0.5298888 
quantile(out.batch.beta4[500:3000, 69], c(0.025, 0.975)) # -0.7737066  0.3350135 
quantile(out.batch.beta4[500:3000, 70], c(0.025, 0.975)) # 0.05340115 0.54610218 

quantile(out.batch.beta5[500:3000, 64], c(0.025, 0.975)) # -1.001547 -0.658155 
quantile(out.batch.beta5[500:3000, 65], c(0.025, 0.975)) # -0.001111211  0.004223531 
quantile(out.batch.beta5[500:3000, 66], c(0.025, 0.975)) # 0.002907604 0.012555037 
quantile(out.batch.beta5[500:3000, 67], c(0.025, 0.975)) # -0.004725201  0.133832981 
quantile(out.batch.beta5[500:3000, 68], c(0.025, 0.975)) # 0.02183764 0.17121375 
quantile(out.batch.beta5[500:3000, 69], c(0.025, 0.975)) # 0.1149152 0.4655443
quantile(out.batch.beta5[500:3000, 70], c(0.025, 0.975)) # -0.06139768  0.12337931 

quantile(out.batch.beta6[500:3000, 64], c(0.025, 0.975)) # -0.7296721 -0.3441351 
quantile(out.batch.beta6[500:3000, 65], c(0.025, 0.975)) # -6.221659e-03  1.228007e-05
quantile(out.batch.beta6[500:3000, 66], c(0.025, 0.975)) # 0.003340003 0.014863265 
quantile(out.batch.beta6[500:3000, 67], c(0.025, 0.975)) # -0.3250576 -0.1553093 
quantile(out.batch.beta6[500:3000, 68], c(0.025, 0.975)) # -0.3242085 -0.1333686
quantile(out.batch.beta6[500:3000, 69], c(0.025, 0.975)) # -0.7916846 -0.2145325
quantile(out.batch.beta6[500:3000, 70], c(0.025, 0.975)) # -0.216451860  0.002037766

hist(out.batch.beta4[500:3000, 64])
hist(out.batch.beta4[500:3000, 65])
hist(out.batch.beta4[500:3000, 66])
hist(out.batch.beta4[500:3000, 67])
hist(out.batch.beta4[500:3000, 68])
hist(out.batch.beta4[500:3000, 69])
hist(out.batch.beta4[500:3000, 70])

hist(out.batch.beta5[500:3000, 64])
hist(out.batch.beta5[500:3000, 65])
hist(out.batch.beta5[500:3000, 66])
hist(out.batch.beta5[500:3000, 67])
hist(out.batch.beta5[500:3000, 68])
hist(out.batch.beta5[500:3000, 69])
hist(out.batch.beta5[500:3000, 70])

hist(out.batch.beta6[500:3000, 64])
hist(out.batch.beta6[500:3000, 65])
hist(out.batch.beta6[500:3000, 66])
hist(out.batch.beta6[500:3000, 67])
hist(out.batch.beta6[500:3000, 68])
hist(out.batch.beta6[500:3000, 69])
hist(out.batch.beta6[500:3000, 70])

t1 = exp(b11*x.test[, 1] + b12*x.test[, 2] + b13*x.test[, 3] + b14*x.test[, 4] + 
           b15*x.test[, 5] + b16*x.test[, 6] + b17*x.test[, 7])
t2 = exp(b21*x.test[, 1] + b22*x.test[, 2] + b23*x.test[, 3] + b24*x.test[, 4] + 
           b25*x.test[, 5] + b26*x.test[, 6] + b27*x.test[, 7])
t3 = exp(b31*x.test[, 1] + b32*x.test[, 2] + b33*x.test[, 3] + b34*x.test[, 4] + 
           b35*x.test[, 5] + b36*x.test[, 6] + b37*x.test[, 7])


summary(t1)
t0 = mean(theta0_out[, 1])   # 0.0004403276
t0 = theta0_out[10, 1]   # 0.0003738732

# RMSE Model GLM -------------------------------------------------------
# Model N1, N2, ..., NT Poisson
test.N1 = predict(mDamage.N1, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N2 = predict(mFire.N2, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N3 = predict(mOther.N3, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N4 = predict(mTheft.N4, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N5 = predict(mTPL.N5, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N6 = predict(mWindscreen.N6, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.NT = predict(mTotal.NT, newdata = data.testing.Bayes, type = "response", interval = "confidence")

# Model X1, X2, ..., XT Poisson-Gamma ----------------------
test.Y1.new = predict(mDamage.Y1, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y2.new = predict(mFire.Y2, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y3.new = predict(mOther.Y3, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y4.new = predict(mTheft.Y4, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y5.new = predict(mTPL.Y5, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y6.new = predict(mWindscreen.Y6, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.YT.new = predict(mTotal.YT, newdata = data.testing.Bayes, type = "response", interval = "confidence")

yhat.X1 = test.N1*test.Y1.new
yhat.X2 = test.N2*test.Y2.new
yhat.X3 = test.N3*test.Y3.new
yhat.X4 = test.N4*test.Y4.new
yhat.X5 = test.N5*test.Y5.new
yhat.X6 = test.N6*test.Y6.new
yhat.XT = test.NT*test.YT.new

summary(yhat.X1)
summary(yhat.X2)
summary(yhat.X3)
summary(yhat.X4)
summary(yhat.X5)
summary(yhat.X6)
summary(yhat.XT)

RMSE(data.testing.Bayes$Damage.y, yhat.X1)
RMSE(data.testing.Bayes$Fire.y, yhat.X2)
RMSE(data.testing.Bayes$Other.y, yhat.X3)
RMSE(data.testing.Bayes$Theft.y, yhat.X4)
RMSE(data.testing.Bayes$TPL.y, yhat.X5)
RMSE(data.testing.Bayes$Windscreen.y, yhat.X6)
RMSE(data.testing.Bayes$Total.y, yhat.XT)


# Model X1, X2, ..., XT Poisson-Normal ---------------------------
test.Y1.Normal.new = predict(mDamage.Y1.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y2.Normal.new = predict(mFire.Y2.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y3.Normal.new = predict(mOther.Y3.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y4.Normal.new = predict(mTheft.Y4.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y5.Normal.new = predict(mTPL.Y5.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y6.Normal.new = predict(mWindscreen.Y6.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.YT.Normal.new = predict(mTotal.YT.Normal, newdata = data.testing.Bayes, type = "response", interval = "confidence")

summary(test.Y1.Normal.new)
summary(test.Y2.Normal.new)
summary(test.Y3.Normal.new)
summary(test.Y4.Normal.new)
summary(test.Y5.Normal.new)
summary(test.Y6.Normal.new)
summary(test.YT.Normal.new)

yhat.X1.PoiNorm = test.N1*test.Y1.Normal.new
yhat.X2.PoiNorm = test.N2*test.Y2.Normal.new
yhat.X3.PoiNorm = test.N3*test.Y3.Normal.new
yhat.X4.PoiNorm = test.N4*test.Y4.Normal.new
yhat.X5.PoiNorm = test.N5*test.Y5.Normal.new
yhat.X6.PoiNorm = test.N6*test.Y6.Normal.new
yhat.XT.PoiNorm = test.NT*test.YT.Normal.new

RMSE(data.testing.Bayes$Damage.y, yhat.X1.PoiNorm)
RMSE(data.testing.Bayes$Fire.y, yhat.X2.PoiNorm)
RMSE(data.testing.Bayes$Other.y, yhat.X3.PoiNorm)
RMSE(data.testing.Bayes$Theft.y, yhat.X4.PoiNorm)
RMSE(data.testing.Bayes$TPL.y, yhat.X5.PoiNorm)
RMSE(data.testing.Bayes$Windscreen.y, yhat.X6.PoiNorm)
RMSE(data.testing.Bayes$Total.y, yhat.XT.PoiNorm)


# Model X1, X2, ..., XT Poisson-Inverse Gaussian -----------------------
test.Y1.InvGaussian.new = predict(mDamage.Y1.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y2.InvGaussian.new = predict(mFire.Y2.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y3.InvGaussian.new = predict(mOther.Y3.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y4.InvGaussian.new = predict(mTheft.Y4.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y5.InvGaussian.new = predict(mTPL.Y5.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y6.InvGaussian.new = predict(mWindscreen.Y6.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.YT.InvGaussian.new = predict(mTotal.YT.InvGaussian, newdata = data.testing.Bayes, type = "response", interval = "confidence")

yhat.X1.PoiInvG = test.N1*test.Y1.InvGaussian.new
yhat.X2.PoiInvG = test.N2*test.Y2.InvGaussian.new
yhat.X3.PoiInvG = test.N3*test.Y3.InvGaussian.new
yhat.X4.PoiInvG = test.N4*test.Y4.InvGaussian.new
yhat.X5.PoiInvG = test.N5*test.Y5.InvGaussian.new
yhat.X6.PoiInvG = test.N6*test.Y6.InvGaussian.new
yhat.XT.PoiInvG = test.NT*test.YT.InvGaussian.new

summary(yhat.X1.PoiInvG)
summary(yhat.X2.PoiInvG)
summary(yhat.X3.PoiInvG)
summary(yhat.X4.PoiInvG)
summary(yhat.X5.PoiInvG)
summary(yhat.X6.PoiInvG)
summary(yhat.XT.PoiInvG)

RMSE(data.testing.Bayes$Damage.y, yhat.X1.PoiInvG)
RMSE(data.testing.Bayes$Fire.y, yhat.X2.PoiInvG)
RMSE(data.testing.Bayes$Other.y, yhat.X3.PoiInvG)
RMSE(data.testing.Bayes$Theft.y, yhat.X4.PoiInvG)
RMSE(data.testing.Bayes$TPL.y, yhat.X5.PoiInvG)
RMSE(data.testing.Bayes$Windscreen.y, yhat.X6.PoiInvG)
RMSE(data.testing.Bayes$Total.y, yhat.XT.PoiInvG)


# Theft ----------
ekspektasi_N4 = t1+t0
RMSE(ekspektasi_N4, data.testing.Bayes$Theft.x)                   # 0.2825118
RMSE(test.N4, data.testing.Bayes$Theft.x)                         # 0.2824322
abs(mean(ekspektasi_N4) - mean(data.testing.Bayes$Theft.x))       # 0.0002123524
abs(mean(test.N4) - mean(data.testing.Bayes$Theft.x))             # 0.0008632623

N4.Bayes = ekspektasi_N4*test.Y4.new                              
RMSE(N4.Bayes, data.testing.Bayes$Theft.y)                        # 852.5481
RMSE(yhat.X4, data.testing.Bayes$Theft.y)                         # 852.4663
abs(mean(N4.Bayes) - mean(data.testing.Bayes$Theft.y))            # 8.812319
abs(mean(yhat.X4) - mean(data.testing.Bayes$Theft.y))             # 7.906904

N4.Bayes.Normal = ekspektasi_N4*test.Y4.Normal.new
RMSE(N4.Bayes.Normal, data.testing.Bayes$Theft.y)                 # 852.5758
RMSE(yhat.X4.PoiNorm, data.testing.Bayes$Theft.y)                 # 852.2913
abs(mean(N4.Bayes.Normal) - mean(data.testing.Bayes$Theft.y))     # 7.933681
abs(mean(yhat.X4.PoiNorm) - mean(data.testing.Bayes$Theft.y))     # 5.99961

N4.Bayes.InvGaussian = ekspektasi_N4*test.Y4.InvGaussian.new  
RMSE(N4.Bayes.InvGaussian, data.testing.Bayes$Theft.y)                  # 853.2136
RMSE(yhat.X4.PoiInvG, data.testing.Bayes$Theft.y)                       # 853.07
abs(mean(N4.Bayes.InvGaussian) - mean(data.testing.Bayes$Theft.y))      # 11.33491
abs(mean(yhat.X4.PoiInvG) - mean(data.testing.Bayes$Theft.y))           # 10.09065

# TPL ----------
ekspektasi_N5 = t2+t0
RMSE(ekspektasi_N5, data.testing.Bayes$TPL.x)                      # 0.588121
RMSE(test.N5, data.testing.Bayes$TPL.x)                            # 0.5873714
abs(mean(ekspektasi_N5) - mean(data.testing.Bayes$TPL.x))          # 0.01179937
abs(mean(test.N5) - mean(data.testing.Bayes$TPL.x))                # 0.009498639

N5.Bayes = ekspektasi_N5*test.Y5.new
RMSE(N5.Bayes, data.testing.Bayes$TPL.y)                           # 1630.43
RMSE(yhat.X5, data.testing.Bayes$TPL.y)                            # 1631.086
abs(mean(N5.Bayes) - mean(data.testing.Bayes$TPL.y))               # 227.9497
abs(mean(yhat.X5) - mean(data.testing.Bayes$TPL.y))                # 231.1662

N5.Bayes.Normal = ekspektasi_N5*test.Y5.Normal.new
RMSE(N5.Bayes.Normal, data.testing.Bayes$TPL.y)                    # 1615.35
RMSE(yhat.X5.PoiNorm, data.testing.Bayes$TPL.y)                    # 1615.651
abs(mean(N5.Bayes.Normal) - mean(data.testing.Bayes$TPL.y))        # 223.9173
abs(mean(yhat.X5.PoiNorm) - mean(data.testing.Bayes$TPL.y))        # 227.5213

N5.Bayes.InvGaussian = ekspektasi_N5*test.Y5.InvGaussian.new
RMSE(N5.Bayes.InvGaussian, data.testing.Bayes$TPL.y)               # 1618.578
RMSE(yhat.X5.PoiInvG, data.testing.Bayes$TPL.y)                    # 1618.321
abs(mean(N5.Bayes.InvGaussian) - mean(data.testing.Bayes$TPL.y))   # 224.5092
abs(mean(yhat.X5.PoiInvG) - mean(data.testing.Bayes$TPL.y))        # 227.4878


# Windscreen --------------
ekspektasi_N6 = t3+t0
RMSE(ekspektasi_N6, data.testing.Bayes$Windscreen.x)               # 0.5380496
RMSE(test.N6, data.testing.Bayes$Windscreen.x)                     # 0.5371733
abs(mean(ekspektasi_N6) - mean(data.testing.Bayes$Windscreen.x))   # 0.007922842
abs(mean(test.N6) - mean(data.testing.Bayes$Windscreen.x))         # 0.007522401

N6.Bayes = ekspektasi_N6*test.Y6.new
RMSE(N6.Bayes, data.testing.Bayes$Windscreen.y)                    # 170.3353
RMSE(yhat.X6, data.testing.Bayes$Windscreen.y)                     # 170.1049
abs(mean(N6.Bayes) - mean(data.testing.Bayes$Windscreen.y))        # 14.28735
abs(mean(yhat.X6) - mean(data.testing.Bayes$Windscreen.y))         # 14.46317

N6.Bayes.Normal = ekspektasi_N6*test.Y6.Normal.new
RMSE(N6.Bayes.Normal, data.testing.Bayes$Windscreen.y)                 # 170.802
RMSE(yhat.X6.PoiNorm, data.testing.Bayes$Windscreen.y)                 # 170.4715
abs(mean(N6.Bayes.Normal) - mean(data.testing.Bayes$Windscreen.y))     # 14.23747
abs(mean(yhat.X6.PoiNorm) - mean(data.testing.Bayes$Windscreen.y))     # 14.2662

N6.Bayes.InvGaussian = ekspektasi_N6*test.Y6.InvGaussian.new
RMSE(N6.Bayes.InvGaussian, data.testing.Bayes$Windscreen.y)                 # 170.274
RMSE(yhat.X6.PoiInvG, data.testing.Bayes$Windscreen.y)                      # 170.0413
abs(mean(N6.Bayes.InvGaussian) - mean(data.testing.Bayes$Windscreen.y))     # 14.19227
abs(mean(yhat.X6.PoiInvG) - mean(data.testing.Bayes$Windscreen.y))          # 14.36593


# Total ---------------- 
ekspektasi_NT = t1+t2+t3+t0
RMSE(ekspektasi_NT, data.testing.Bayes$Total.x)
RMSE(test.NT, data.testing.Bayes$Total.x)
abs(mean(ekspektasi_NT) - mean(data.testing.Bayes$Total.x))
abs(mean(test.NT) - mean(data.testing.Bayes$Total.x))

NT.Bayes = ekspektasi_NT*test.YT.new
RMSE(NT.Bayes, data.testing.Bayes$Total.y)
RMSE(test.NT, data.testing.Bayes$Total.y)

