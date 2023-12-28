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
sd_51 = abs(log(mean(n5))/mean(x[, 1]))
sd_52 = abs(log(mean(n5))/mean(x[, 2]))
sd_61 = abs(log(mean(n6))/mean(x[, 1]))
sd_62 = abs(log(mean(n6))/mean(x[, 2]))
set.seed(123)
beta_51 = rnorm(1, mean = 0, sd = sd_51)
beta_52 = rnorm(1, mean = 0, sd = sd_52)
beta_61 = rnorm(1, mean = 0, sd = sd_61)
beta_62 = rnorm(1, mean = 0, sd = sd_62)
beta_5 = c(beta_51, beta_52)
# -0.34374313 -0.02048922
beta_6 = c(beta_61, beta_62)
# 1.354015496 0.008889654
a = 0.1
b = 0.1
theta_0 = rgamma(1e3, shape = a, rate = b) 
theta_0 = mean(theta_0)  # 0.9642894

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

set.seed(123)
y_0 = rpois(n, theta_0)
summary(y_0)
y_0 = pmin(pmax(y_0, 0), pmin(n5, n6))  # pmin untuk menilai min setiap row dari data
# 0.00000 0.00000 0.00000 0.03388 0.00000 1.00000 

# likelihood y0
lg_y0 = function(theta0_init, y0_init, data_klaim1, data_klaim2, x, 
                 beta1_init, beta2_init){
  theta1 = exp(beta1_init[1]*x[, 1] + beta1_init[2]*x[, 2])
  theta2 = exp(beta2_init[1]*x[, 1] + beta2_init[2]*x[, 2])
  y0 = exp(
    y0_init*log(theta0_init) - log(factorial(y0_init)) - log(factorial(data_klaim1 - y0_init)) - 
      log(factorial(data_klaim2 - y0_init)) - 
      y0_init*log(theta1) - y0_init*log(theta2)
  )
  y0_final = pmin(pmax(y0, 0), pmin(data_klaim1, data_klaim2))
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


# Looping N kali -----
N = 10

theta.0 = matrix(NA, nrow = 1e3, ncol = N)
theta0_out = matrix(NA, nrow = N, ncol = 5)

y0 = lg_y0(theta0_init = theta_0, y0_init = y_0, data_klaim1 = n5, data_klaim2 = n6, x = x, 
           beta1_init = beta_5, beta2_init = beta_6)

beta = matrix(NA, nrow = N+1, ncol = 4)
beta[1, 1:2] = beta_5
beta[1, 3:4] = beta_6
colnames(beta) = c("beta_51", "beta_52", "beta_61", "beta_62")

conf.interval = matrix(NA, nrow = N, ncol = 8)
colnames(conf.interval) = c("beta_51.a", "beta_51.b", "beta_52.a", "beta_52.b", 
                            "beta_61.a", "beta_61.b", "beta_62.a", "beta_62.b")

#sigmasq = matrix(NA, nrow = N+1, ncol = 12)
#colnames(sigmasq) = c("beta_11", "beta_12", "beta_21", "beta_22", "beta_31", "beta_32", "beta_41", "beta_42", 
#                      "beta_51", "beta_52", "beta_61", "beta_62")
stasioner = matrix(NA, nrow = N, ncol = 4)
colnames(stasioner) = c("beta_51", "beta_52", "beta_61", "beta_62")


# manual -------
set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$blen
out$scale
out$accept # 0.001
out = metrop(out, scale = 0.1)
out$accept # 0.018

set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept # 0.001
out = metrop(out, scale = 0.02)
out$accept # 0.147


set.seed(123)
lgMetrop = lg_metrop(n = n5, x = x, y0 = y0, sigma = sigma.beta5)
out = metrop(lgMetrop, beta[1, 1:2], nbatch = 1e3)
out$accept # 0.001
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.01)
out$accept # 0.276
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(1, 2))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 150, main = paste("Beta 5", i-1))    # blen = 150
}
out = metrop(out, blen = 150)  # blen = 150
out$accept  # 0.2696333
out$time
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 5", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
acf(out$batch)


set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.006
out$scale
out$blen
out = metrop(out, scale = 0.1)
out$accept # 0.027

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.006
out = metrop(out, scale = 0.02)
out$accept # 0.188

set.seed(123)
lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
out = metrop(lgMetrop, beta[1, 3:4], nbatch = 1e3)
out$accept # 0.006
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 1")
}
out = metrop(out, scale = 0.01)
out$accept # 0.313
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
par(mfrow = c(1, 2))
for (i in 1:2) {
  acf(out$batch[, i], lag.max = 300, main = paste("Beta 6", i-1))    # blen = 300
}
out = metrop(out, blen = 300)
par(mfrow = c(1, 2))
for (i in 1:2) {
  plot(ts(out$batch[, i]), ylab = paste("Beta 6", i-1))
  title("Time series plot of MCMC output with scale 0.01")
}
out$accept   # 0.3079667
acf(out$batch)

# Looping -----

out.batch.beta5 = matrix(NA, nrow = 1e3, ncol = 20)
out.batch.beta6 = matrix(NA, nrow = 1e3, ncol = 20)
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
  out = metrop(lgMetrop, beta[i, 1:2], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, blen = 150)
  out.batch.beta5[, ((i-1)*2+1):(i*2)] = out$batch
  out.accept.beta5[i, ] = out$accept
  beta[i+1, 1:2] = apply(out$batch[200:1000, ], 2, mean)[1:2]
  #sigmasq[i+1, 1:2] = apply(out$batch, 2, mean)[3:4] - beta[i+1, 1:2]^2
  conf.interval[i, 1:2] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 3:4] = t.test(out$batch[200:1000, 2])$conf.int
  
  for (j in 1:2) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  # metropolis hasting beta 6
  set.seed(123)
  lgMetrop = lg_metrop(n = n6, x = x, y0 = y0, sigma = sigma.beta6)
  out = metrop(lgMetrop, beta[i, 3:4], nbatch = 1e3)
  out = metrop(out, scale = 0.01)
  out = metrop(out, blen = 300)
  out.batch.beta6[, ((i-1)*2+1):(i*2)] = out$batch
  out.accept.beta6[i, ] = out$accept
  beta[i+1, 3:4] = apply(out$batch[200:1000, ], 2, mean)[1:2]
  #sigmasq[i+1, 3:4] = apply(out$batch, 2, mean)[3:4] - beta[i+1, 3:4]^2
  conf.interval[i, 5:6] = t.test(out$batch[200:1000, 1])$conf.int
  conf.interval[i, 7:8] = t.test(out$batch[200:1000, 2])$conf.int
  
  for (j in 1:2) {
    result = adf.test(out$batch[200:1000, j]) 
    stasioner[i, j+2] = ifelse(result$p.value <= 0.05, "stasioner", "tidak stasioner")
  }
  
  
  # perbaharui nilai y0
  y0 = lg_y0(theta0_init = theta0_out[i, 1], y0_init = y0, data_klaim1 = n5, data_klaim2 = n6, 
             x = x, beta1_init = beta[i+1, 1:2], beta2_init = beta[i+1, 3:4])
  
}
# jam 12:19 - 2:25
# jam 11:41 - 1:56


out.batch.beta5
out.batch.beta6
out.accept.beta5
out.accept.beta6
par(mfrow = c(4, 5))
for(i in 1:20){
  plot(ts(out.batch.beta5[200:1000, i]), ylab = paste("Beta 5", ifelse(i %% 2 == 0, 1, i %% 1)))
}
for(i in 1:20){
  plot(ts(out.batch.beta6[200:1000, i]), ylab = paste("Beta 6", ifelse(i %% 2 == 0, 1, i %% 1)))
}
par(mfrow = c(1, 1))

hist(out.batch.beta5[, 1])
hist(out.batch.beta5[, 2])

shapiro.test(out.batch.beta5[200:1000, 9])  # p-value = 0.9888
# Hipotesis Nol (H0): Sampel data berasal dari distribusi normal.
# Hipotesis Alternatif (H1): Sampel data tidak berasal dari distribusi normal.
shapiro.test(out.batch.beta5[200:1000, 10]) # p-value = 0.9996
shapiro.test(out.batch.beta5[200:1000, 8]) # p-value = 0.108   
# sudah dicek 1-8, semua normal dengan taraf signifikansi 5%

shapiro.test(out.batch.beta6[200:1000, 9])  # p-value = 0.8509
# Hipotesis Nol (H0): Sampel data berasal dari distribusi normal.
# Hipotesis Alternatif (H1): Sampel data tidak berasal dari distribusi normal.
shapiro.test(out.batch.beta6[200:1000, 10]) # p-value = 0.9018
shapiro.test(out.batch.beta6[200:1000, 8]) # 0.2531   
# sudah dicek 1-8, semua normal dengan taraf signifikansi 5%


stasioner = factor(stasioner)
summary(stasioner)
theta0_out  # nilai theta 0 kecil karena nilai a = 0.1, tp b = 0.1 + n, jadi nilainya kecil bgt. Y0 = 0.
beta
conf.interval
t.test(out.batch.beta5[200:1000, 19])$conf.int
plot(ts(theta0_out))
plot(ts(beta[2:11, 1]))

quantile(out.batch.beta5[200:1000, 19], c(0.025, 0.975)) # -0.8053784 -0.6914864
quantile(out.batch.beta5[200:1000, 20], c(0.025, 0.975)) # 0.005920547 0.017908488 
quantile(out.batch.beta6[200:1000, 19], c(0.025, 0.975)) # -1.0309533 -0.9078179 
quantile(out.batch.beta6[200:1000, 20], c(0.025, 0.975)) # -0.00186801  0.01097443
quantile(out.batch.beta6[200:1000, 20], c(0.05, 0.95)) # -0.0009701141  0.0095703541
par(mfrow = c(1,1))
hist(out.batch.beta5[200:1000, 19])
hist(out.batch.beta5[200:1000, 20])
hist(out.batch.beta6[200:1000, 19])
hist(out.batch.beta6[200:1000, 20])

b11 = mean(beta[2:11, 1])  # -0.7485392
b12 = mean(beta[2:11, 2])  # 0.01183465
b21 = mean(beta[2:11, 3])  # -0.9682928
b22 = mean(beta[2:11, 4])  # 0.004451724

b11 = beta[11, 1]  # -0.7494615
b12 = beta[11, 2]  # 0.01200793 
b21 = beta[11, 3]  # -0.9678094
b22 = beta[11, 4]  # 0.004465921


t1 = exp(b11*x.test[, 1] + b12*x.test[, 2])
t2 = exp(b21*x.test[, 1] + b22*x.test[, 2])



summary(t1)
t0 = mean(theta0_out)  # 0.02784242
t0 = theta0_out[10, 1]  # 0.02775446
theta0_out[10, 4:5]  # 0.02353117 0.03244078


# RMSE Model GLM -------------------------------------------------------
# Model N1, N2, ..., NT Poisson
test.N1 = predict(mDamage.N1, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N2 = predict(mFire.N2, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N3 = predict(mOther.N3, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N4 = predict(mTheft.N4, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N5 = predict(mTPL.N5, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.N6 = predict(mWindscreen.N6, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.NT = predict(mTotal.NT, newdata = data.testing.Bayes, type = "response", interval = "confidence")

par(mfrow = c(2, 4))
plot(test.N1 - data.testing.Bayes$Damage.x, main = "Frequency Damage")
plot(test.N2 - data.testing.Bayes$Fire.x, main = "Frequency Fire")
plot(test.N3 - data.testing.Bayes$Other.x, main = "Frequency Other")
plot(test.N4 - data.testing.Bayes$Theft.x, main = "Frequency Theft")
plot(test.N5 - data.testing.Bayes$TPL.x, main = "Frequency TPL")
plot(test.N6 - data.testing.Bayes$Windscreen.x, main = "Frequency Windscreen")
plot(test.NT - data.testing.Bayes$Total.x, main = "Frequency Total")

set.seed(123)
yhat.N1 = simul_N(test.N1, 0.87)
table(data.testing.Bayes$Damage.x, yhat.N1)
yhat.N2 = simul_N(test.N2, 0.87)
table(data.testing.Bayes$Fire.x, yhat.N2)
yhat.N3 = simul_N(test.N3, 0.87)
table(data.testing.Bayes$Other.x, yhat.N3)
yhat.N4 = simul_N(test.N4, 0.87)
table(data.testing.Bayes$Theft.x, yhat.N4)
yhat.N5 = simul_N(test.N5, 0.87)
table(data.testing.Bayes$TPL.x, yhat.N5)
yhat.N6 = simul_N(test.N6, 0.87)
table(data.testing.Bayes$Windscreen.x, yhat.N6)
yhat.NT = simul_N(test.NT, 0.87)
table(data.testing.Bayes$Total.x, yhat.NT)

# Model X1, X2, ..., XT Poisson-Gamma ----------------------
test.Y1.new = predict(mDamage.Y1, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y2.new = predict(mFire.Y2, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y3.new = predict(mOther.Y3, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y4.new = predict(mTheft.Y4, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y5.new = predict(mTPL.Y5, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.Y6.new = predict(mWindscreen.Y6, newdata = data.testing.Bayes, type = "response", interval = "confidence")
test.YT.new = predict(mTotal.YT, newdata = data.testing.Bayes, type = "response", interval = "confidence")

par(mfrow = c(2, 4))
plot(test.Y1.new - data.testing.Bayes$Damage.Sev, main = "Severity Damage Gamma")
plot(test.Y2.new - data.testing.Bayes$Fire.Sev, main = "Severity Fire Gamma")
plot(test.Y3.new - data.testing.Bayes$Other.Sev, main = "Severity Other Gamma")
plot(test.Y4.new - data.testing.Bayes$Theft.Sev, main = "Severity Theft Gamma")
plot(test.Y5.new - data.testing.Bayes$TPL.Sev, main = "Severity TPL Gamma")
plot(test.Y6.new - data.testing.Bayes$Windscreen.Sev, main = "Severity Windscreen Gamma")
plot(test.YT.new - data.testing.Bayes$Total.Sev, main = "Severity Total Gamma")


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

par(mfrow = c(2, 4))
plot(test.Y1.Normal.new - data.testing.Bayes$Damage.Sev, main = "Severity Damage Normal")
plot(test.Y2.Normal.new - data.testing.Bayes$Fire.Sev, main = "Severity Fire Normal")
plot(test.Y3.Normal.new - data.testing.Bayes$Other.Sev, main = "Severity Other Normal")
plot(test.Y4.Normal.new - data.testing.Bayes$Theft.Sev, main = "Severity Theft Normal")
plot(test.Y5.Normal.new - data.testing.Bayes$TPL.Sev, main = "Severity TPL Normal")
plot(test.Y6.Normal.new - data.testing.Bayes$Windscreen.Sev, main = "Severity Windscreen Normal")
plot(test.YT.Normal.new - data.testing.Bayes$Total.Sev, main = "Severity Total Normal")

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

par(mfrow = c(2, 4))
plot(test.Y1.InvGaussian.new - data.testing.Bayes$Damage.Sev, main = "Severity Damage Inverse Gaussian")
plot(test.Y2.InvGaussian.new - data.testing.Bayes$Fire.Sev, main = "Severity Fire Inverse Gaussian")
plot(test.Y3.InvGaussian.new - data.testing.Bayes$Other.Sev, main = "Severity Other Inverse Gaussian")
plot(test.Y4.InvGaussian.new - data.testing.Bayes$Theft.Sev, main = "Severity Theft Inverse Gaussian")
plot(test.Y5.InvGaussian.new - data.testing.Bayes$TPL.Sev, main = "Severity TPL Inverse Gaussian")
plot(test.Y6.InvGaussian.new - data.testing.Bayes$Windscreen.Sev, main = "Severity Windscreen Inverse Gaussian")
plot(test.YT.InvGaussian.new - data.testing.Bayes$Total.Sev, main = "Severity Total Inverse Gaussian")
par(mfrow = c(1, 1))

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
RMSE(ekspektasi_N5, data.testing.Bayes$TPL.x)                  # 0.5914358
RMSE(test.N5, data.testing.Bayes$TPL.x)                        # 0.5873714
abs(mean(ekspektasi_N5) - mean(data.testing.Bayes$TPL.x))      # 0.01007723
abs(mean(test.N5) - mean(data.testing.Bayes$TPL.x))            # 0.009498639

N5.Bayes = ekspektasi_N5*test.Y5.new
RMSE(N5.Bayes, data.testing.Bayes$TPL.y)                       # 1624.961
RMSE(yhat.X5, data.testing.Bayes$TPL.y)                        # 1631.086
abs(mean(N5.Bayes) - mean(data.testing.Bayes$TPL.y))           # 222.2329
abs(mean(yhat.X5) - mean(data.testing.Bayes$TPL.y))            # 231.1662

N5.Bayes.Normal = ekspektasi_N5*test.Y5.Normal.new             
RMSE(N5.Bayes.Normal, data.testing.Bayes$TPL.y)                # 1617.083
RMSE(yhat.X5.PoiNorm, data.testing.Bayes$TPL.y)                # 1615.651
abs(mean(N5.Bayes.Normal) - mean(data.testing.Bayes$TPL.y))    # 226.615
abs(mean(yhat.X5.PoiNorm) - mean(data.testing.Bayes$TPL.y))    # 227.5213

N5.Bayes.InvGaussian = ekspektasi_N5*test.Y5.InvGaussian.new
RMSE(N5.Bayes.InvGaussian, data.testing.Bayes$TPL.y)               # 1619.734
RMSE(yhat.X5.PoiInvG, data.testing.Bayes$TPL.y)                    # 1618.321
abs(mean(N5.Bayes.InvGaussian) - mean(data.testing.Bayes$TPL.y))   # 226.4687
abs(mean(yhat.X5.PoiInvG) - mean(data.testing.Bayes$TPL.y))        # 227.4878

# Windscreen ----------
ekspektasi_N6 = t2+t0
RMSE(ekspektasi_N6, data.testing.Bayes$Windscreen.x)               # 0.5402781
RMSE(test.N6, data.testing.Bayes$Windscreen.x)                     # 0.5371733
abs(mean(ekspektasi_N6) - mean(data.testing.Bayes$Windscreen.x))   # 0.008354858
abs(mean(test.N6) - mean(data.testing.Bayes$Windscreen.x))         # 0.007522401

N6.Bayes = ekspektasi_N6*test.Y6.new
RMSE(N6.Bayes, data.testing.Bayes$Windscreen.y)                    # 169.6111
RMSE(yhat.X6, data.testing.Bayes$Windscreen.y)                     # 170.1049
abs(mean(N6.Bayes) - mean(data.testing.Bayes$Windscreen.y))        # 13.26945
abs(mean(yhat.X6) - mean(data.testing.Bayes$Windscreen.y))         # 14.46317

N6.Bayes.Normal = ekspektasi_N6*test.Y6.Normal.new
RMSE(N6.Bayes.Normal, data.testing.Bayes$Windscreen.y)               # 169.7646
RMSE(yhat.X6.PoiNorm, data.testing.Bayes$Windscreen.y)               # 170.4715
abs(mean(N6.Bayes.Normal) - mean(data.testing.Bayes$Windscreen.y))   # 12.76961
abs(mean(yhat.X6.PoiNorm) - mean(data.testing.Bayes$Windscreen.y))   # 14.2662

N6.Bayes.InvGaussian = ekspektasi_N6*test.Y6.InvGaussian.new
RMSE(N6.Bayes.InvGaussian, data.testing.Bayes$Windscreen.y)                  # 169.6051
RMSE(yhat.X6.PoiInvG, data.testing.Bayes$Windscreen.y)                       # 170.0413
abs(mean(N6.Bayes.InvGaussian) - mean(data.testing.Bayes$Windscreen.y))      # 13.25255
abs(mean(yhat.X6.PoiInvG) - mean(data.testing.Bayes$Windscreen.y))           # 14.36593


