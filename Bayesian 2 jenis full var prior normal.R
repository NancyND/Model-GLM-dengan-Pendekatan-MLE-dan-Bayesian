library(mcmc)
library(tseries)
library(caret)

# data -----------------------------
set.seed(123)
trainIndex.Bayes = createDataPartition(mydata1$Damage.x, p = 0.7, list = FALSE)
data.training.Bayes = mydata1[trainIndex.Bayes, ]
data.testing.Bayes = mydata1[-trainIndex.Bayes, ]

cor(data.training.Bayes[, 1:6])

n5 = data.training.Bayes$TPL.x
n6 = data.training.Bayes$Windscreen.x
x1 = data.training.Bayes$BonusMalus
x1 = ifelse(x1 == "Bonus", 0, 1)
x2 = data.training.Bayes$VehAge
x3 = data.training.Bayes$VehGas
x3 = ifelse(x3 == "Diesel", 0, 1)
x4 = data.training.Bayes$Garage
x4 = ifelse(x4 == "Closed parking", 0, 1)
x5 = data.training.Bayes$Region
x5 = ifelse(x5 == "Center or Headquarters", 0, 1)
x = rep(1, times = length(x1))
x = matrix(c(x, x1, x2, x3, x4, x5), ncol = 6)
n = length(n5)
summary(n5)
summary(x)


n5.test = data.testing.Bayes$TPL.x
n6.test = data.testing.Bayes$Windscreen.x
x1.test = data.testing.Bayes$BonusMalus
x1.test = ifelse(x1.test == "Bonus", 0, 1)
x2.test = data.testing.Bayes$VehAge
x3.test = data.testing.Bayes$VehGas
x3.test = ifelse(x3.test == "Diesel", 0, 1)
x4.test = data.testing.Bayes$Garage
x4.test = ifelse(x4.test == "Closed parking", 0, 1)
x5.test = data.testing.Bayes$Region
x5.test = ifelse(x5.test == "Center or Headquarters", 0, 1)
x.test = rep(1, times = length(x1.test))
x.test = matrix(c(x.test, x1.test, x2.test, x3.test, x4.test, x5.test), ncol = 6)
summary(n5.test)
summary(x.test)


# prior beta1, theta0, y_0 -------------
sd_51 = abs(log(mean(n5))/mean(x[, 1]))
sd_52 = abs(log(mean(n5))/mean(x[, 2]))
sd_53 = abs(log(mean(n5))/mean(x[, 3]))
sd_54 = abs(log(mean(n5))/mean(x[, 4]))
sd_55 = abs(log(mean(n5))/mean(x[, 5]))
sd_56 = abs(log(mean(n5))/mean(x[, 6]))
sd_61 = abs(log(mean(n6))/mean(x[, 1]))
sd_62 = abs(log(mean(n6))/mean(x[, 2]))
sd_63 = abs(log(mean(n6))/mean(x[, 3]))
sd_64 = abs(log(mean(n6))/mean(x[, 4]))
sd_65 = abs(log(mean(n6))/mean(x[, 5]))
sd_66 = abs(log(mean(n6))/mean(x[, 6]))

set.seed(123)
beta_51 = rnorm(1, mean = 0, sd = sd_51)
beta_52 = rnorm(1, mean = 0, sd = sd_52)
beta_53 = rnorm(1, mean = 0, sd = sd_53)
beta_54 = rnorm(1, mean = 0, sd = sd_54)
beta_55 = rnorm(1, mean = 0, sd = sd_55)
beta_56 = rnorm(1, mean = 0, sd = sd_56)
beta_61 = rnorm(1, mean = 0, sd = sd_61)
beta_62 = rnorm(1, mean = 0, sd = sd_62)
beta_63 = rnorm(1, mean = 0, sd = sd_63)
beta_64 = rnorm(1, mean = 0, sd = sd_64)
beta_65 = rnorm(1, mean = 0, sd = sd_65)
beta_66 = rnorm(1, mean = 0, sd = sd_66)
beta_5 = c(beta_51, beta_52, beta_53, beta_54, beta_55, beta_56)
# -0.34374313 -4.05444174  0.13874821  0.07851982  0.09818798  3.59503202
beta_6 = c(beta_61, beta_62, beta_63, beta_64, beta_65, beta_66)
# 0.40038773 -31.56176414  -0.08659798  -0.70295184   1.31671858   1.06827124
a = 0.1
b = 0.1
theta_0 = rgamma(1e3, shape = a, rate = b)
theta_0 = mean(theta_0)
# 0.9695462

sigma.beta5 = matrix(c(sd_51^2, 0, 0, 0, 0, 0,
                       0, sd_52^2, 0, 0, 0, 0,
                       0, 0, sd_53^2, 0, 0, 0,
                       0, 0, 0, sd_54^2, 0, 0,
                       0, 0, 0, 0, sd_55^2, 0,
                       0, 0, 0, 0, 0, sd_56^2), nrow = 6)
sigma.beta6 = matrix(c(sd_61^2, 0, 0, 0, 0, 0,
                       0, sd_62^2, 0, 0, 0, 0,
                       0, 0, sd_63^2, 0, 0, 0,
                       0, 0, 0, sd_64^2, 0, 0,
                       0, 0, 0, 0, sd_65^2, 0,
                       0, 0, 0, 0, 0, sd_66^2), nrow = 6)


summary(pmin(n5, n6))


set.seed(123)
y_0 = rpois(n, theta_0)
summary(y_0)
y_0 = pmin(pmax(y_0, 0), pmin(n5, n6))  # pmin untuk menilai min setiap row dari data
# 0.00000 0.00000 0.00000 0.03407 0.00000 1.00000 

# likelihood y0
lg_y0 = function(theta0_init, y0_init, data_klaim1, data_klaim2, x, 
                 beta1_init, beta2_init){
  theta1 = exp(beta1_init[1]*x[, 1] + beta1_init[2]*x[, 2] + beta1_init[3]*x[, 3] + beta1_init[4]*x[, 4] + 
                 beta1_init[5]*x[, 5] + beta1_init[6]*x[, 6])
  theta2 = exp(beta2_init[1]*x[, 1] + beta2_init[2]*x[, 2] + beta2_init[3]*x[, 3] + beta2_init[4]*x[, 4] + 
                 beta2_init[5]*x[, 5] + beta2_init[6]*x[, 6])
  y0 = exp(
    y0_init*log(theta0_init) - log(factorial(y0_init)) - log(factorial(data_klaim1 - y0_init)) - 
      log(factorial(data_klaim2 - y0_init)) - y0_init*log(theta1) - y0_init*log(theta2)
  )
  y0_final = pmin(pmax(y0, 0), pmin(data_klaim1, data_klaim2))
  return(y0_final)
}

# generate theta0
fungsi_theta0 = function(a, b, n, y0_init){
  theta0 = rgamma(1e3, shape = a+sum(y0_init), rate = b+n)
}


# log-posterior beta
lg_beta = function(beta_init, x, n, y0, sigma){
  theta_i = exp(beta_init[1]*x[, 1] + beta_init[2]*x[, 2] + beta_init[3]*x[, 3] + beta_init[4]*x[, 4] + 
                  beta_init[5]*x[, 5] + beta_init[6]*x[, 6])
  inv_sigma = solve(sigma)
  prior = -0.5 * t(beta_init) %*% inv_sigma %*% (beta_init)
  return(-sum(theta_i) + sum(log(theta_i)*(n-y0)) + prior)
}



# log-posterior metrop
lg_metrop = function(n, x, y0, sigma) function(beta){
  
  loglik = lg_beta(beta_init = beta, x = x, n = n, y0 = y0, sigma = sigma)
  
  return(loglik - sum(beta^2)/8)
}


# Looping N kali -----
N = 10

theta.0 = matrix(NA, nrow = 1e3, ncol = N)
theta0_out = matrix(NA, nrow = N, ncol = 5)

y0 = lg_y0(theta0_init = theta_0, y0_init = y_0, data_klaim1 = n5, data_klaim2 = n6, x = x, 
           beta1_init = beta_5, beta2_init = beta_6)
summary(y0)  # 0.00000 0.00000 0.00000 0.02824 0.00000 1.00000

beta = matrix(NA, nrow = N+1, ncol = 12)
beta[1, 1:6] = beta_5
beta[1, 7:12] = beta_6
colnames(beta) = c("beta_51", "beta_52", "beta_53", "beta_54", "beta_55", "beta_56", 
                   "beta_61", "beta_62", "beta_63", "beta_64", "beta_65", "beta_66")

conf.interval = matrix(NA, nrow = N, ncol = 24)
colnames(conf.interval) = c("beta_51.a", "beta_51.b", "beta_52.a", "beta_52.b", "beta_53.a", "beta_53.b", "beta_54.a", "beta_54.b",
                            "beta_55.a", "beta_55.b", "beta_56.a", "beta_56.b", 
                            "beta_61.a", "beta_61.b", "beta_62.a", "beta_62.b", "beta_63.a", "beta_63.b", "beta_64.a", "beta_64.b", 
                            "beta_65.a", "beta_65.b", "beta_66.a", "beta_66.b")

stasioner = matrix(NA, nrow = N, ncol = 12)
colnames(stasioner) = c("beta_51", "beta_52", "beta_53", "beta_54", "beta_55", "beta_56",
                        "beta_61", "beta_62", "beta_63", "beta_64", "beta_65", "beta_66")


# test manual -------------

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:6], nbatch = 1e3)
out$accept # 0.011
plot(ts(out$batch))
out = metrop(out, scale = 0.1)
out$accept # 0.022


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:6], nbatch = 1e3)
out$accept # 0.011
out = metrop(out, scale = 0.02)
out$accept # 0.139

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:6], nbatch = 1e3)
out$accept # 0.011
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out$scale
out = metrop(out, scale = 0.01)
out$accept # 0.261
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 5", i-1))    # blen = 400
}
out$scale
out = metrop(out, blen = 400)  # tulisnya: burn-in sebanyak 2000 sampel dan mengambil 1000 sampel 
# yang dicatat setiap 400 sampling
out$accept # kalau blen = 400 maka accept = 0.2460325
out$scale
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[200:1000, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 5", i-1))    # blen = 400
}
adf.test(out$batch[, 1])
adf.test(out$batch[, 2])
adf.test(out$batch[, 3])
adf.test(out$batch[, 4])
adf.test(out$batch[, 5])
adf.test(out$batch[, 6])
mean(out$batch[, 1])  # -0.8817376
mean(out$batch[, 2])  # 0.2232088
mean(out$batch[, 3])  # 0.009057813
mean(out$batch[, 4])  # 0.08421262
mean(out$batch[, 5])  # 0.06720036
mean(out$batch[, 6])  # 0.1324207


set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 7:12], nbatch = 1e3)
out$accept # 0.009
out = metrop(out, scale = 0.1)
out$accept # 0.025

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 7:12], nbatch = 1e3)
out$accept # 0.009
out = metrop(out, scale = 0.02)
out$accept # 0.166

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 7:12], nbatch = 1e3)
out$accept # 0.009
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.01)
out$accept # 0.275
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
acf(out$batch[, 2], lag.max = 380)  # blen = 380
acf(out$batch[, 4], lag.max = 380)  
par(mfrow = c(3, 2))
for (i in 1:6) {
  acf(out$batch[, i], lag.max = 1000, main = paste("Beta 6", i-1))
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  acf(out$batch[, i], lag.max = 400, main = paste("Beta 6", i-1))
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  acf(out$batch[, i], lag.max = 350, main = paste("Beta 6", i))   # beta 66 belum kecil autocorrelasinya
}
par(mfrow = c(1, 1))
acf(out$batch[, 4], lag.max = 350)
out = metrop(out, blen = 400)  # ini sudah menggunakan scale = 0.01, nnti kalau metrop(lgMetrop) akan pakai scale default yaitu 1 
out$accept # kalau blen = 400 maka accept = 0.2887225
out$scale
out$blen
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(ts(out$batch[200:1000, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
acf(out$batch)
adf.test(out$batch[, 1])
adf.test(out$batch[, 2])
adf.test(out$batch[, 3])
adf.test(out$batch[, 4])
adf.test(out$batch[, 5])
adf.test(out$batch[, 6])


# looping --------------

out.batch.beta5 = matrix(NA, nrow = 1e3, ncol = 60)
out.batch.beta6 = matrix(NA, nrow = 1e3, ncol = 60)
colnames(out.batch.beta5) = c("b50.L1", "b51.L1", "b52.L1", "b53.L1", "b54.L1", "b55.L1", 
                              "b50.L2", "b51.L2", "b52.L2", "b53.L2", "b54.L2", "b55.L2",
                              "b50.L3", "b51.L3", "b52.L3", "b53.L3", "b54.L3", "b55.L3",
                              "b50.L4", "b51.L4", "b52.L4", "b53.L4", "b54.L4", "b55.L4",
                              "b50.L5", "b51.L5", "b52.L5", "b53.L5", "b54.L5", "b55.L5",
                              "b50.L6", "b51.L6", "b52.L6", "b53.L6", "b54.L6", "b55.L6",
                              "b50.L7", "b51.L7", "b52.L7", "b53.L7", "b54.L7", "b55.L7",
                              "b50.L8", "b51.L8", "b52.L8", "b53.L8", "b54.L8", "b55.L8",
                              "b50.L9", "b51.L9", "b52.L9", "b53.L9", "b54.L9", "b55.L9",
                              "b50.L10", "b51.L10", "b52.L10", "b53.L10", "b54.L10", "b55.L10")
colnames(out.batch.beta6) = c("b60.L1", "b61.L1", "b62.L1", "b63.L1", "b64.L1", "b65.L1", 
                              "b60.L2", "b61.L2", "b62.L2", "b63.L2", "b64.L2", "b65.L2",
                              "b60.L3", "b61.L3", "b62.L3", "b63.L3", "b64.L3", "b65.L3",
                              "b60.L4", "b61.L4", "b62.L4", "b63.L4", "b64.L4", "b65.L4",
                              "b60.L5", "b61.L5", "b62.L5", "b63.L5", "b64.L5", "b65.L5",
                              "b60.L6", "b61.L6", "b62.L6", "b63.L6", "b64.L6", "b65.L6",
                              "b60.L7", "b61.L7", "b62.L7", "b63.L7", "b64.L7", "b65.L7",
                              "b60.L8", "b61.L8", "b62.L8", "b63.L8", "b64.L8", "b65.L8",
                              "b60.L9", "b61.L9", "b62.L9", "b63.L9", "b64.L9", "b65.L9",
                              "b60.L10", "b61.L10", "b62.L10", "b63.L10", "b64.L10", "b65.L10")
out.accept.beta5 = matrix(NA, nrow = 10, ncol = 1)
out.accept.beta6 = matrix(NA, nrow = 10, ncol = 1)
colnames(out.accept.beta5) = c("accept.Beta_5")
colnames(out.accept.beta6) = c("accept.Beta_6")
rownames(out.accept.beta5) = c("Loop 1", "Loop 2", "Loop 3", "Loop 4", "Loop 5", "Loop 6", "Loop 7", "Loop 8", "Loop 9", "Loop 10")
rownames(out.accept.beta6) = c("Loop 1", "Loop 2", "Loop 3", "Loop 4", "Loop 5", "Loop 6", "Loop 7", "Loop 8", "Loop 9", "Loop 10")


for (i in 1:N) {
  
  # perbaharui nilai theta0
  set.seed(123)
  theta.0[, i] = fungsi_theta0(a = 0.1, b = 0.1, n = n, y0_init = y0)
  theta0_out[i, 1] = mean(theta.0[, i])
  theta0_out[i, 2:3] = t.test(theta.0[, i])$conf.int
  theta0_out[i, 4:5] = quantile(theta.0[, i], c(0.025, 0.975))
  
  
  # metropolis hasting beta 5
  set.seed(123)
  lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
  out = metrop(lgMetrop, beta[i, 1:6], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, nbatch = 1e3, blen = 400)
  out.accept.beta5[i, ] = out$accept
  out.batch.beta5[, ((i-1)*6+1):(i*6)] = out$batch
  beta[i+1, 1:6] = apply(out$batch[200:1000, ], 2, mean)[1:6]
  conf.interval[i, 1:2] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 3:4] = t.test(out$batch[200:1000, 2])$conf.int
  conf.interval[i, 5:6] = t.test(out$batch[200:1000, 3])$conf.int
  conf.interval[i, 7:8] = t.test(out$batch[200:1000, 4])$conf.int
  conf.interval[i, 9:10] = t.test(out$batch[200:1000, 5])$conf.int
  conf.interval[i, 11:12] = t.test(out$batch[200:1000, 6])$conf.int
  
  for (j in 1:6) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 6
  set.seed(123)
  lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
  out = metrop(lgMetrop, beta[i, 7:12], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, nbatch = 1e3, blen = 400)
  out.accept.beta6[i, ] = out$accept
  out.batch.beta6[, ((i-1)*6+1):(i*6)] = out$batch
  beta[i+1, 7:12] = apply(out$batch[200:1000, ], 2, mean)[1:6]
  conf.interval[i, 13:14] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 15:16] = t.test(out$batch[200:1000, 2])$conf.int
  conf.interval[i, 17:18] = t.test(out$batch[200:1000, 3])$conf.int
  conf.interval[i, 19:20] = t.test(out$batch[200:1000, 4])$conf.int
  conf.interval[i, 21:22] = t.test(out$batch[200:1000, 5])$conf.int
  conf.interval[i, 23:24] = t.test(out$batch[200:1000, 6])$conf.int
  
  for (j in 1:6) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j+6] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  
  # perbaharui nilai y0
  y0 = lg_y0(theta0_init = theta0_out[i, 1], y0_init = y0, data_klaim1 = n5, data_klaim2 = n6, 
             x = x, beta1_init = beta[i+1, 1:6], beta2_init = beta[i+1, 7:12])
  
}
# jam 00.04 - 9.00
# jam 12.55 - 6.28
# jam 02:12 - 8:40


out.accept.beta5
out.accept.beta6
plot(ts(out.batch.beta5[, 60]))
plot(ts(out.batch.beta6[, 60]))
par(mfrow = c(3, 6))
for (i in 1:18) {
  plot(ts(out.batch.beta5[200:1000, i]), ylab = paste("Beta 5", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}
par(mfrow = c(3, 6))
for (i in 19:36) {
  plot(ts(out.batch.beta5[200:1000, i]), ylab = paste("Beta 5", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}
par(mfrow = c(4, 6))
for (i in 37:60) {
  plot(ts(out.batch.beta5[200:1000, i]), ylab = paste("Beta 5", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}

par(mfrow = c(3, 6))
for (i in 1:18) {
  plot(ts(out.batch.beta6[200:1000, i]), ylab = paste("Beta 6", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}
par(mfrow = c(3, 6))
for (i in 19:36) {
  plot(ts(out.batch.beta6[200:1000, i]), ylab = paste("Beta 6", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}
par(mfrow = c(4, 6))
for (i in 37:60) {
  plot(ts(out.batch.beta6[200:1000, i]), ylab = paste("Beta 6", ifelse(i %% 6 == 0, 5, (i-1) %% 6)))
}


stasioner = factor(stasioner)
summary(stasioner)
theta0_out  # nilai theta 0 kecil karena nilai a = 0.1, tp b = 0.1 + n, jadi nilainya kecil bgt. Y0 = 0.
# theta 0 = 0.02763935 
# confidence intervalnya: 0.02750094 0.02777776
beta
conf.interval
plot(ts(theta0_out))
plot(ts(beta[2:11, 12]))

quantile(out.batch.beta5[200:1000, 55], c(0.025, 0.975)) # -0.9531376 -0.7533080 
quantile(out.batch.beta5[200:1000, 56], c(0.025, 0.975)) # 0.1101864 0.4221072
quantile(out.batch.beta5[200:1000, 57], c(0.025, 0.975)) # 0.004223632 0.013138747 
quantile(out.batch.beta5[200:1000, 58], c(0.025, 0.975)) # 0.0575330 0.1616232 
quantile(out.batch.beta5[200:1000, 59], c(0.025, 0.975)) # -0.04625672  0.11015217
quantile(out.batch.beta5[200:1000, 60], c(0.025, 0.975)) # 0.04851623 0.16416886 

quantile(out.batch.beta6[200:1000, 55], c(0.025, 0.975)) # -0.8424466 -0.6326063
quantile(out.batch.beta6[200:1000, 56], c(0.025, 0.975)) # -0.7831782 -0.2202168
quantile(out.batch.beta6[200:1000, 57], c(0.025, 0.975)) # 0.005548299 0.016246256
quantile(out.batch.beta6[200:1000, 58], c(0.025, 0.975)) # -0.2726380 -0.1547654
quantile(out.batch.beta6[200:1000, 59], c(0.025, 0.975)) # -0.19870764 -0.03081187
quantile(out.batch.beta6[200:1000, 60], c(0.025, 0.975)) # -0.2990446 -0.1517597 

hist(out.batch.beta5[200:1000, 55])
hist(out.batch.beta5[200:1000, 56])
hist(out.batch.beta5[200:1000, 57])
hist(out.batch.beta5[200:1000, 58])
hist(out.batch.beta5[200:1000, 59])
hist(out.batch.beta5[200:1000, 60])

hist(out.batch.beta6[200:1000, 55])
hist(out.batch.beta6[200:1000, 56])
hist(out.batch.beta6[200:1000, 57])
hist(out.batch.beta6[200:1000, 58])
hist(out.batch.beta6[200:1000, 59])
hist(out.batch.beta6[200:1000, 60])

tes.normal = matrix(data = NA, nrow = 60, ncol = 2) 
for (i in 1:2) {
  for (j in 1:60) {
    tes.normal[j, i] = ifelse(i == 1, 
                              ifelse(shapiro.test(out.batch.beta5[200:1000, j])$p.value > 0.05, "normal", "bukan normal"),
                              ifelse(shapiro.test(out.batch.beta6[200:1000, j])$p.value > 0.05, "normal", "bukan normal"))
  }
}
shapiro.test(out.batch.beta5[200:1000, 58])$p.value
tes.normal

b11 = mean(beta[2:11, 1])  # -0.8597864
b12 = mean(beta[2:11, 2])  # 0.2675786
b13 = mean(beta[2:11, 3])  # 0.008699237
b14 = mean(beta[2:11, 4])  # 0.09805749
b15 = mean(beta[2:11, 5])  # 0.0403567
b16 = mean(beta[2:11, 6])  # 0.1112565
b21 = mean(beta[2:11, 7])  # -0.7502553
b22 = mean(beta[2:11, 8])  # -0.5252269
b23 = mean(beta[2:11, 9])  # 0.01093924
b24 = mean(beta[2:11, 10]) # -0.2201293
b25 = mean(beta[2:11, 11]) # -0.09347995
b26 = mean(beta[2:11, 12]) # -0.2191585

b11 = beta[11, 1]  # -0.8513707 
b12 = beta[11, 2]  # 0.2795338
b13 = beta[11, 3]  # 0.00861913
b14 = beta[11, 4]  # 0.1053322 
b15 = beta[11, 5]  # 0.02773074
b16 = beta[11, 6]  # 0.1036579
b21 = beta[11, 7]  # -0.7362142
b22 = beta[11, 8]  # -0.4828888
b23 = beta[11, 9]  # 0.01078705
b24 = beta[11, 10] # -0.2122317
b25 = beta[11, 11] # -0.1130114
b26 = beta[11, 12] # -0.2280734


t1 = exp(b11*x.test[, 1] + b12*x.test[, 2] + b13*x.test[, 3] + 
           b14*x.test[, 4] + b15*x.test[, 5] + b16*x.test[, 6])
t2 = exp(b21*x.test[, 1] + b22*x.test[, 2] + b23*x.test[, 3] + 
           b24*x.test[, 4] + b25*x.test[, 5] + b26*x.test[, 6])


summary(t1)
summary(t2)
t0 = mean(theta0_out[, 1]) # 0.02776996
t0 = theta0_out[10, 1]   # 0.02763935
theta0_out[10, 4:5]   # 0.02342510 0.03231622

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


# TPL ----------
ekspektasi_N5 = t1+t0
RMSE(ekspektasi_N5, data.testing.Bayes$TPL.x)               # 0.5884387
RMSE(test.N5, data.testing.Bayes$TPL.x)                     # 0.5873714
abs(mean(ekspektasi_N5) - mean(data.testing.Bayes$TPL.x))   # 0.0114159
abs(mean(test.N5) - mean(data.testing.Bayes$TPL.x))         # 0.009498639

N5.Bayes = ekspektasi_N5*test.Y5.new
RMSE(N5.Bayes, data.testing.Bayes$TPL.y)                    # 1628.092
RMSE(yhat.X5, data.testing.Bayes$TPL.y)                     # 1631.086
abs(mean(N5.Bayes) - mean(data.testing.Bayes$TPL.y))        # 225.1881
abs(mean(yhat.X5) - mean(data.testing.Bayes$TPL.y))         # 231.1662

N5.Bayes.Normal = ekspektasi_N5*test.Y5.Normal.new
RMSE(N5.Bayes.Normal, data.testing.Bayes$TPL.y)             # 1615.7
RMSE(yhat.X5.PoiNorm, data.testing.Bayes$TPL.y)             # 1615.651
abs(mean(N5.Bayes.Normal) - mean(data.testing.Bayes$TPL.y)) # 224.518
abs(mean(yhat.X5.PoiNorm) - mean(data.testing.Bayes$TPL.y)) # 227.5213

N5.Bayes.InvGaussian = ekspektasi_N5*test.Y5.InvGaussian.new
RMSE(N5.Bayes.InvGaussian, data.testing.Bayes$TPL.y)                # 1618.728
RMSE(yhat.X5.PoiInvG, data.testing.Bayes$TPL.y)                     # 1618.321
abs(mean(N5.Bayes.InvGaussian) - mean(data.testing.Bayes$TPL.y))    # 224.7005
abs(mean(yhat.X5.PoiInvG) - mean(data.testing.Bayes$TPL.y))         # 227.4878


# Windscreen ----------
ekspektasi_N6 = t2+t0
RMSE(ekspektasi_N6, data.testing.Bayes$Windscreen.x)                 # 0.5371603
RMSE(test.N6, data.testing.Bayes$Windscreen.x)                       # 0.5371733
abs(mean(ekspektasi_N6) - mean(data.testing.Bayes$Windscreen.x))     # 0.008377527
abs(mean(test.N6) - mean(data.testing.Bayes$Windscreen.x))           # 0.007522401

N6.Bayes = ekspektasi_N6*test.Y6.new
RMSE(N6.Bayes, data.testing.Bayes$Windscreen.y)                   # 169.9964
RMSE(yhat.X6, data.testing.Bayes$Windscreen.y)                    # 170.1049
abs(mean(N6.Bayes) - mean(data.testing.Bayes$Windscreen.y))       # 14.15181
abs(mean(yhat.X6) - mean(data.testing.Bayes$Windscreen.y))        # 14.46317

N6.Bayes.Normal = ekspektasi_N6*test.Y6.Normal.new                   
RMSE(N6.Bayes.Normal, data.testing.Bayes$Windscreen.y)               # 170.3385
RMSE(yhat.X6.PoiNorm, data.testing.Bayes$Windscreen.y)               # 170.4715
abs(mean(N6.Bayes.Normal) - mean(data.testing.Bayes$Windscreen.y))   # 13.93633
abs(mean(yhat.X6.PoiNorm) - mean(data.testing.Bayes$Windscreen.y))   # 14.2662

N6.Bayes.InvGaussian = ekspektasi_N6*test.Y6.InvGaussian.new
RMSE(N6.Bayes.InvGaussian, data.testing.Bayes$Windscreen.y)                 # 169.9374
RMSE(yhat.X6.PoiInvG, data.testing.Bayes$Windscreen.y)                      # 170.0413
abs(mean(N6.Bayes.InvGaussian) - mean(data.testing.Bayes$Windscreen.y))     # 14.06029
abs(mean(yhat.X6.PoiInvG) - mean(data.testing.Bayes$Windscreen.y))          # 14.36593

# boxplot --------------
boxplot(data.testing.Bayes$Damage.x, test.N1, names = c("Data Asli", "Prediksi MLE"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Damage")
boxplot(data.testing.Bayes$Fire.x, test.N2, names = c("Data Asli", "Prediksi MLE"),
        main = "Boxplot Jumlah Klaim Jenis Jaminan Fire")
boxplot(data.testing.Bayes$Other.x, test.N3, names = c("Data Asli", "Prediksi MLE"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Other")
boxplot(data.testing.Bayes$Theft.x, test.N4, names = c("Data Asli", "Prediksi MLE"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Theft")
boxplot(data.testing.Bayes$TPL.x, test.N5, ekspektasi_N5, names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan TPL")
mean_value = c(mean(data.testing.Bayes$TPL.x), mean(test.N5), mean(ekspektasi_N5))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(test.N5, ekspektasi_N5, names = c("Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan TPL")
mean_value = c(mean(test.N5), mean(ekspektasi_N5))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$Windscreen.x, test.N6, ekspektasi_N6, names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Windscreen")
mean_value = c(mean(data.testing.Bayes$Windscreen.x), mean(test.N6), mean(ekspektasi_N6))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(test.N6, ekspektasi_N6, names = c("Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Windscreen")
mean_value = c(mean(test.N6), mean(ekspektasi_N6))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)

boxplot(data.testing.Bayes$TPL.y, yhat.X5, N5.Bayes, ylim = c(0, 2000), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan TPL dari Distribusi Poisson-Gamma")
mean_value = c(mean(data.testing.Bayes$TPL.y), mean(yhat.X5), mean(N5.Bayes))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$TPL.y, yhat.X5.PoiNorm, N5.Bayes.Normal, ylim = c(0, 2000), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan TPL dari Distribusi Poisson-Normal")
mean_value = c(mean(data.testing.Bayes$TPL.y), mean(yhat.X5.PoiNorm), mean(N5.Bayes.Normal))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$TPL.y, yhat.X5.PoiInvG, N5.Bayes.InvGaussian, ylim = c(0, 2000), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan TPL dari Distribusi Poisson-Inverse Gaussian")
mean_value = c(mean(data.testing.Bayes$TPL.y), mean(yhat.X5.PoiInvG), mean(N5.Bayes.InvGaussian))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$Windscreen.y, yhat.X6, N6.Bayes, ylim = c(0, 600), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan Windscreen dari Distribusi Poisson-Gamma")
mean_value = c(mean(data.testing.Bayes$Windscreen.y), mean(yhat.X6), mean(N6.Bayes))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$Windscreen.y, yhat.X6.PoiNorm, N6.Bayes.Normal, ylim = c(0, 600), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan Windscreen dari Distribusi Poisson-Normal")
mean_value = c(mean(data.testing.Bayes$Windscreen.y), mean(yhat.X6.PoiNorm), mean(N6.Bayes.Normal))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
boxplot(data.testing.Bayes$Windscreen.y, yhat.X6.PoiInvG, N6.Bayes.InvGaussian, ylim = c(0, 500), 
        names = c("Data Asli", "Prediksi MLE", "Prediksi Bayesian"), 
        main = "Boxplot Total Klaim Jenis Jaminan Windscreen dari Distribusi Poisson-Inverse Gaussian")
mean_value = c(mean(data.testing.Bayes$Windscreen.y), mean(yhat.X6.PoiInvG), mean(N6.Bayes.InvGaussian))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)

summary(data.testing.Bayes$Windscreen.y)
quantile(data.testing.Bayes$Windscreen.y, c(0.025, 0.975))
quantile(data.testing.Bayes$Windscreen.y, 0.5)

