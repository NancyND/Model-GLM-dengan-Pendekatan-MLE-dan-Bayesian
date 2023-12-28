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
x1 = data.training.Bayes$VehAge
x = rep(1, times = length(x1))
x = matrix(c(x, x1), ncol = 2)
n = length(n1)
summary(n1)
summary(x)

n1.test = data.testing.Bayes$Damage.x
n2.test = data.testing.Bayes$Fire.x
n3.test = data.testing.Bayes$Other.x
n4.test = data.testing.Bayes$Theft.x
n5.test = data.testing.Bayes$TPL.x
n6.test = data.testing.Bayes$Windscreen.x
x1.test = data.testing.Bayes$VehAge
x.test = rep(1, times = length(x1.test))
x.test = matrix(c(x.test, x1.test), ncol = 2)
summary(n2.test)
summary(x.test)

# prior beta1, theta0, y_0 -------------
sd_41 = abs(log(mean(n4))/mean(x[, 1]))
sd_42 = abs(log(mean(n4))/mean(x[, 2]))
sd_51 = abs(log(mean(n5))/mean(x[, 1]))
sd_52 = abs(log(mean(n5))/mean(x[, 2]))
sd_61 = abs(log(mean(n6))/mean(x[, 1]))
sd_62 = abs(log(mean(n6))/mean(x[, 2]))

set.seed(123)
beta_41 = rnorm(1, mean = 0, sd = sd_41)
beta_42 = rnorm(1, mean = 0, sd = sd_42)
beta_51 = rnorm(1, mean = 0, sd = sd_51)
beta_52 = rnorm(1, mean = 0, sd = sd_52)
beta_61 = rnorm(1, mean = 0, sd = sd_61)
beta_62 = rnorm(1, mean = 0, sd = sd_62)
beta_4 = c(beta_41, beta_42) 
# -1.451292 -0.086506
beta_5 = c(beta_51, beta_52) 
# 0.955965300 0.006276295
beta_6 = c(beta_61, beta_62) 
# 0.1123094 0.2162343
a = 0.1
b = 0.1
theta_0 = rgamma(1e3, shape = a, rate = b) 
theta_0 = mean(theta_0) # 0.9651053

sigma.beta4 = matrix(c(sd_41^2, 0, 
                       0, sd_42^2), nrow = 2)
sigma.beta5 = matrix(c(sd_51^2, 0, 
                       0, sd_52^2), nrow = 2)
sigma.beta6 = matrix(c(sd_61^2, 0, 
                       0, sd_62^2), nrow = 2)


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
  theta1 = exp(beta1_init[1]*x[, 1] + beta1_init[2]*x[, 2])
  theta2 = exp(beta2_init[1]*x[, 1] + beta2_init[2]*x[, 2])
  theta3 = exp(beta3_init[1]*x[, 1] + beta3_init[2]*x[, 2])
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
  theta_i = exp(beta_init[1]*x[, 1] + beta_init[2]*x[, 2])
  inv_sigma = solve(sigma)
  prior = -0.5 * t(beta_init) %*% inv_sigma %*% (beta_init)
  return(-sum(theta_i) + sum(log(theta_i)*(n-y0)) + prior)
}


# log-likelihood metrop
lg_metrop = function(n, x, y0, sigma) function(beta){
  
  loglik = lg_beta(beta_init = beta, x = x, n = n, y0 = y0, sigma = sigma)
  
  return(loglik - sum(beta^2)/8)
}


# Looping N kali -------
N = 10

theta.0 = matrix(NA, nrow = 1e3, ncol = N)
theta0_out = matrix(NA, nrow = N, ncol = 5)

y0 = lg_y0(theta0_init = theta_0, y0_init = y_0, data_klaim1 = n4, data_klaim2 = n5, data_klaim3 = n6, x = x, 
           beta1_init = beta_4, beta2_init = beta_5, beta3_init = beta_6)
summary(y0)  # 0.0000000 0.0000000 0.0000000 0.0008806 0.0000000 1.0000000 

beta = matrix(NA, nrow = N+1, ncol = 6)
beta[1, 1:2] = beta_4
beta[1, 3:4] = beta_5
beta[1, 5:6] = beta_6
colnames(beta) = c("beta_41", "beta_42", "beta_51", "beta_52", "beta_61", "beta_62")

conf.interval = matrix(NA, nrow = N, ncol = 12)
colnames(conf.interval) = c("beta_41.a", "beta_41.b", "beta_42.a", "beta_42.b", 
                            "beta_51.a", "beta_51.b", "beta_52.a", "beta_52.b", 
                            "beta_61.a", "beta_61.b", "beta_62.a", "beta_62.b")

#sigmasq = matrix(NA, nrow = N+1, ncol = 12)
#colnames(sigmasq) = c("beta_11", "beta_12", "beta_21", "beta_22", "beta_31", "beta_32", "beta_41", "beta_42", 
#                      "beta_51", "beta_52", "beta_61", "beta_62")
stasioner = matrix(NA, nrow = N, ncol = 6)
colnames(stasioner) = c("beta_41", "beta_42", "beta_51", "beta_52", "beta_61", "beta_62")


# test manual ----------

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.1)
out$accept   # 0.08

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.09)
out$accept   # 0.094

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.08)
out$accept   # 0.089

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.07)
out$accept   # 0.117

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.06)
out$accept   # 0.121

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
out = metrop(out, scale = 0.05)
out$accept   # 0.164

set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.04)
out$accept   # 0.207
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0.04")
}
par(mfrow = c(2, 1))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 300, main = paste("Beta 4", i))    # blen = 100
}
out = metrop(out, blen = 100)
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i))
  title("Time series plot of MCMC output with scale 0.04")
}
acf(out$batch)
adf.test(out$batch[, 1])
adf.test(out$batch[, 2])
out$accept  # 0.19546
out$time


set.seed(123)
lgMetrop = lg_metrop(n = n4, x = x, y0 = y0, sigma = sigma.beta4)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept   # 0.007
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.03)
out$accept   # 0.274
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 0,03")
}
par(mfrow = c(2, 1))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 300, main = paste("Beta 4", i-1))    # blen = 150
}
out = metrop(out, blen = 150)
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 4", i-1))
  title("Time series plot of MCMC output with scale 0,03")
}
acf(out$batch)
adf.test(out$batch[, 1])
adf.test(out$batch[, 2])
out$accept   # 0.2635533
out$time


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.004
out = metrop(out, scale = 0.1)
out$accept # 0.013

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.004
out = metrop(out, scale = 0.02)
out$accept # 0.149

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.004
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.01)
out$accept # 0.29
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0,01")
}
par(mfrow = c(2, 1))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 300, main = paste("Beta 5", i-1))    # blen = 150
}
out = metrop(out, blen = 150)
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0,01")
}
acf(out$batch)
out$accept  # 0.2621333


set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 5:6], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.1)
out$accept # 0.027

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 5:6], nbatch = 1e3)
out$accept  # 0.005
out = metrop(out, scale = 0.02)
out$accept # 0.163

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 5:6], nbatch = 1e3)
out$accept  # 0.005
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.01)
out$accept # 0.326
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0,01")
}
par(mfrow = c(2, 1))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 200, main = paste("Beta 6", i-1))    # blen = 200
}
out = metrop(out, blen = 200)
par(mfrow = c(2, 1))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0,01")
}
acf(out$batch)
out$accept # 0.29999
adf.test(out$batch[, 1])
adf.test(out$batch[, 2])

# Looping -----------

out.batch.beta4 = matrix(NA, nrow = 1e3, ncol = 20)
out.batch.beta5 = matrix(NA, nrow = 1e3, ncol = 20)
out.batch.beta6 = matrix(NA, nrow = 1e3, ncol = 20)
colnames(out.batch.beta4) = c("b40.L1", "b41.L1", 
                              "b40.L2", "b41.L2", 
                              "b40.L3", "b41.L3", 
                              "b40.L4", "b41.L4", 
                              "b40.L5", "b41.L5", 
                              "b40.L6", "b41.L6", 
                              "b40.L7", "b41.L7", 
                              "b40.L8", "b41.L8", 
                              "b40.L9", "b41.L9", 
                              "b40.L10", "b41.L10")
colnames(out.batch.beta5) = c("b50.L1", "b51.L1", 
                              "b50.L2", "b51.L2", 
                              "b50.L3", "b51.L3", 
                              "b50.L4", "b51.L4", 
                              "b50.L5", "b51.L5", 
                              "b50.L6", "b51.L6", 
                              "b50.L7", "b51.L7", 
                              "b50.L8", "b51.L8", 
                              "b50.L9", "b51.L9", 
                              "b50.L10", "b51.L10")
colnames(out.batch.beta6) = c("b60.L1", "b61.L1", 
                              "b60.L2", "b61.L2", 
                              "b60.L3", "b61.L3", 
                              "b60.L4", "b61.L4", 
                              "b60.L5", "b61.L5", 
                              "b60.L6", "b61.L6", 
                              "b60.L7", "b61.L7", 
                              "b60.L8", "b61.L8", 
                              "b60.L9", "b61.L9", 
                              "b60.L10", "b61.L10")
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
  out = metrop(lgMetrop, beta[i, 1:2], nbatch = 1e3)
  out = metrop(out, scale = 0.03)
  out = metrop(out, blen = 150)
  out.batch.beta4[, ((i-1)*2+1):(i*2)] = out$batch
  out.accept.beta4[i, ] = out$accept
  beta[i+1, 1:2] = apply(out$batch[200:1000, ], 2, mean)[1:2]
  #sigmasq[i+1, 1:2] = apply(out$batch, 2, mean)[3:4] - beta[i+1, 1:2]^2
  conf.interval[i, 1:2] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 3:4] = t.test(out$batch[200:1000, 2])$conf.int
  
  for (j in 1:2) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 5
  set.seed(123)
  lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
  out = metrop(lgMetrop, beta[i, 3:4], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, blen = 150)
  out.batch.beta5[, ((i-1)*2+1):(i*2)] = out$batch
  out.accept.beta5[i, ] = out$accept
  beta[i+1, 3:4] = apply(out$batch[200:1000, ], 2, mean)[1:2]
  #sigmasq[i+1, 3:4] = apply(out$batch, 2, mean)[3:4] - beta[i+1, 3:4]^2
  conf.interval[i, 5:6] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 7:8] = t.test(out$batch[200:1000, 2])$conf.int
  
  for (j in 1:2) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j+2] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 6
  set.seed(123)
  lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
  out = metrop(lgMetrop, beta[i, 5:6], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, blen = 200)
  out.batch.beta6[, ((i-1)*2+1):(i*2)] = out$batch
  out.accept.beta6[i, ] = out$accept
  beta[i+1, 5:6] = apply(out$batch[200:1000, ], 2, mean)[1:2]
  conf.interval[i, 9:10] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 11:12] = t.test(out$batch[200:1000, 2])$conf.int
  
  for (j in 1:2) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j+4] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # perbaharui nilai y0
  y0 = lg_y0(theta0_init = theta0_out[i, 1], y0_init = y0, data_klaim1 = n4, data_klaim2 = n5, data_klaim3 = n6, 
             x = x, beta1_init = beta[i+1, 1:2], beta2_init = beta[i+1, 3:4], beta3_init = beta[i+1, 5:6])
  
}
# jam 11:36 - 2:45
# jam 8:44 - 11:34



out.accept.beta4
out.accept.beta5
out.accept.beta6


par(mfrow = c(4, 5))
for(i in 1:20){
  plot(ts(out.batch.beta4[200:1000, i]), ylab = paste("Beta 4", ifelse(i %% 2 == 0, 1, (i-1) %% 2)))
}
for(i in 1:20){
  plot(ts(out.batch.beta5[200:1000, i]), ylab = paste("Beta 5", ifelse(i %% 2 == 0, 1, (i-1) %% 2)))
}
for(i in 1:20){
  plot(ts(out.batch.beta6[200:1000, i]), ylab = paste("Beta 6", ifelse(i %% 2 == 0, 1, (i-1) %% 2)))
}
par(mfrow = c(1, 1))

stasioner = factor(stasioner)
summary(stasioner)
theta0_out  # nilai theta 0 kecil karena nilai a = 0.1, tp b = 0.1 + n, jadi nilainya kecil bgt. Y0 = 0.
beta
conf.interval
plot(ts(theta0_out))
plot(ts(beta[2:11, 1]))

quantile(out.batch.beta4[200:1000, 19], c(0.025, 0.975)) # -2.415752 -2.158903
quantile(out.batch.beta4[200:1000, 20], c(0.025, 0.975)) # -0.06256067 -0.03211076
quantile(out.batch.beta5[200:1000, 19], c(0.025, 0.975)) # -0.7320507 -0.6240290
quantile(out.batch.beta5[200:1000, 20], c(0.025, 0.975)) # 0.003569347 0.014969787
quantile(out.batch.beta6[200:1000, 19], c(0.025, 0.975)) # -0.9473814 -0.8234244 
quantile(out.batch.beta6[200:1000, 20], c(0.025, 0.975)) # -0.004447839  0.008778988

hist(out.batch.beta4[200:1000, 19])
hist(out.batch.beta4[200:1000, 20])

hist(out.batch.beta5[200:1000, 19])
hist(out.batch.beta5[200:1000, 20])

hist(out.batch.beta6[200:1000, 19])
hist(out.batch.beta6[200:1000, 20])


tes.normal = matrix(data = NA, nrow = 20, ncol = 3) 
for (i in 1:3) {
  for (j in 1:20) {
    tes.normal[j, i] = ifelse(i == 1,
                              ifelse(shapiro.test(out.batch.beta4[200:1000, j])$p.value > 0.05, "normal", "bukan normal"),
                              ifelse(i == 2, 
                                     ifelse(shapiro.test(out.batch.beta5[200:1000, j])$p.value > 0.05, "normal", "bukan normal"),
                                     ifelse(shapiro.test(out.batch.beta6[200:1000, j])$p.value > 0.05, "normal", "bukan normal")))
                              
  }
}
shapiro.test(out.batch.beta5[200:1000, 58])$p.value
tes.normal


b11 = mean(beta[2:11, 1])  # -2.288137
b12 = mean(beta[2:11, 2])  # -0.04709771
b21 = mean(beta[2:11, 3])  # -0.6812201
b22 = mean(beta[2:11, 4])  # 0.009683969
b31 = mean(beta[2:11, 5])  # -0.8842648
b32 = mean(beta[2:11, 6])  # 0.002116342

b11 = beta[11, 1]  # -2.280723 
b12 = beta[11, 2]  # -0.04780552 
b21 = beta[11, 3]  # -0.6792705
b22 = beta[11, 4]  # 0.009485343 
b31 = beta[11, 5]  # -0.8855119 
b33 = beta[11, 6]  # 0.002250243 


t1 = exp(b11*x.test[, 1] + b12*x.test[, 2])
t2 = exp(b21*x.test[, 1] + b22*x.test[, 2])
t3 = exp(b31*x.test[, 1] + b32*x.test[, 2])


summary(t1)
t0 = mean(theta0_out[, 1])   # 0.0004159892
t0 = theta0_out[10, 1]     # 0.0002466751
theta0_out[10, 4:5]   # 1.363732e-05 8.487922e-04

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
RMSE(ekspektasi_N4, data.testing.Bayes$Theft.x)                   # 0.2826713
RMSE(test.N4, data.testing.Bayes$Theft.x)                         # 0.2824322
abs(mean(ekspektasi_N4) - mean(data.testing.Bayes$Theft.x))       # 0.0004545208
abs(mean(test.N4) - mean(data.testing.Bayes$Theft.x))             # 0.0008632623

N4.Bayes = ekspektasi_N4*test.Y4.new                              
RMSE(N4.Bayes, data.testing.Bayes$Theft.y)                        # 852.3919
RMSE(yhat.X4, data.testing.Bayes$Theft.y)                         # 852.4663
abs(mean(N4.Bayes) - mean(data.testing.Bayes$Theft.y))            # 12.02139
abs(mean(yhat.X4) - mean(data.testing.Bayes$Theft.y))             # 7.906904

N4.Bayes.Normal = ekspektasi_N4*test.Y4.Normal.new
RMSE(N4.Bayes.Normal, data.testing.Bayes$Theft.y)                 # 852.6389
RMSE(yhat.X4.PoiNorm, data.testing.Bayes$Theft.y)                 # 852.2913
abs(mean(N4.Bayes.Normal) - mean(data.testing.Bayes$Theft.y))     # 13.12197
abs(mean(yhat.X4.PoiNorm) - mean(data.testing.Bayes$Theft.y))     # 5.99961

N4.Bayes.InvGaussian = ekspektasi_N4*test.Y4.InvGaussian.new  
RMSE(N4.Bayes.InvGaussian, data.testing.Bayes$Theft.y)                  # 852.5766
RMSE(yhat.X4.PoiInvG, data.testing.Bayes$Theft.y)                       # 853.07
abs(mean(N4.Bayes.InvGaussian) - mean(data.testing.Bayes$Theft.y))      # 12.539
abs(mean(yhat.X4.PoiInvG) - mean(data.testing.Bayes$Theft.y))           # 10.09065

# TPL ----------
ekspektasi_N5 = t2+t0
RMSE(ekspektasi_N5, data.testing.Bayes$TPL.x)                      # 0.5913519
RMSE(test.N5, data.testing.Bayes$TPL.x)                            # 0.5873714
abs(mean(ekspektasi_N5) - mean(data.testing.Bayes$TPL.x))          # 0.009795108
abs(mean(test.N5) - mean(data.testing.Bayes$TPL.x))                # 0.009498639

N5.Bayes = ekspektasi_N5*test.Y5.new
RMSE(N5.Bayes, data.testing.Bayes$TPL.y)                           # 1625.107
RMSE(yhat.X5, data.testing.Bayes$TPL.y)                            # 1631.086
abs(mean(N5.Bayes) - mean(data.testing.Bayes$TPL.y))               # 223.2356
abs(mean(yhat.X5) - mean(data.testing.Bayes$TPL.y))                # 231.1662

N5.Bayes.Normal = ekspektasi_N5*test.Y5.Normal.new
RMSE(N5.Bayes.Normal, data.testing.Bayes$TPL.y)                    # 1616.855
RMSE(yhat.X5.PoiNorm, data.testing.Bayes$TPL.y)                    # 1615.651
abs(mean(N5.Bayes.Normal) - mean(data.testing.Bayes$TPL.y))        # 227.0569
abs(mean(yhat.X5.PoiNorm) - mean(data.testing.Bayes$TPL.y))        # 227.5213

N5.Bayes.InvGaussian = ekspektasi_N5*test.Y5.InvGaussian.new
RMSE(N5.Bayes.InvGaussian, data.testing.Bayes$TPL.y)               # 1619.749
RMSE(yhat.X5.PoiInvG, data.testing.Bayes$TPL.y)                    # 1618.321
abs(mean(N5.Bayes.InvGaussian) - mean(data.testing.Bayes$TPL.y))   # 227.2742
abs(mean(yhat.X5.PoiInvG) - mean(data.testing.Bayes$TPL.y))        # 227.4878


# Windscreen --------------
ekspektasi_N6 = t3+t0
RMSE(ekspektasi_N6, data.testing.Bayes$Windscreen.x)               # 0.5403322
RMSE(test.N6, data.testing.Bayes$Windscreen.x)                     # 0.5371733
abs(mean(ekspektasi_N6) - mean(data.testing.Bayes$Windscreen.x))   # 0.009005761
abs(mean(test.N6) - mean(data.testing.Bayes$Windscreen.x))         # 0.007522401

N6.Bayes = ekspektasi_N6*test.Y6.new
RMSE(N6.Bayes, data.testing.Bayes$Windscreen.y)                    # 169.5951
RMSE(yhat.X6, data.testing.Bayes$Windscreen.y)                     # 170.1049
abs(mean(N6.Bayes) - mean(data.testing.Bayes$Windscreen.y))        # 13.07449
abs(mean(yhat.X6) - mean(data.testing.Bayes$Windscreen.y))         # 14.46317

N6.Bayes.Normal = ekspektasi_N6*test.Y6.Normal.new
RMSE(N6.Bayes.Normal, data.testing.Bayes$Windscreen.y)                 # 169.7264
RMSE(yhat.X6.PoiNorm, data.testing.Bayes$Windscreen.y)                 # 170.4715
abs(mean(N6.Bayes.Normal) - mean(data.testing.Bayes$Windscreen.y))     # 12.54678
abs(mean(yhat.X6.PoiNorm) - mean(data.testing.Bayes$Windscreen.y))     # 14.2662

N6.Bayes.InvGaussian = ekspektasi_N6*test.Y6.InvGaussian.new
RMSE(N6.Bayes.InvGaussian, data.testing.Bayes$Windscreen.y)                 # 169.5941
RMSE(yhat.X6.PoiInvG, data.testing.Bayes$Windscreen.y)                      # 170.0413
abs(mean(N6.Bayes.InvGaussian) - mean(data.testing.Bayes$Windscreen.y))     # 13.0641
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

