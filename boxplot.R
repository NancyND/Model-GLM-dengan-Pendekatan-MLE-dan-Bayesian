ekspektasi_N5_2jenis.1var = ekspektasi_N5
ekspektasi_N6_2jenis.1var = ekspektasi_N6
ekspektasi_N5_2jenis.fvar = ekspektasi_N5
ekspektasi_N6_2jenis.fvar = ekspektasi_N6

ekspektasi_N4_3jenis.1var = ekspektasi_N4
ekspektasi_N5_3jenis.1var = ekspektasi_N5
ekspektasi_N6_3jenis.1var = ekspektasi_N6
ekspektasi_N4_3jenis.fvar = ekspektasi_N4
ekspektasi_N5_3jenis.fvar = ekspektasi_N5
ekspektasi_N6_3jenis.fvar = ekspektasi_N6


N5.Bayes_2jenis.1var = N5.Bayes
N5.Bayes.Normal_2jenis.1var = N5.Bayes.Normal
N5.Bayes.InvGaussian_2jenis.1var = N5.Bayes.InvGaussian
N6.Bayes_2jenis.1var = N6.Bayes
N6.Bayes.Normal_2jenis.1var = N6.Bayes.Normal
N6.Bayes.InvGaussian_2jenis.1var = N6.Bayes.InvGaussian

N5.Bayes_2jenis.fvar = N5.Bayes
N5.Bayes.Normal_2jenis.fvar = N5.Bayes.Normal
N5.Bayes.InvGaussian_2jenis.fvar = N5.Bayes.InvGaussian
N6.Bayes_2jenis.fvar = N6.Bayes
N6.Bayes.Normal_2jenis.fvar = N6.Bayes.Normal
N6.Bayes.InvGaussian_2jenis.fvar = N6.Bayes.InvGaussian

N4.Bayes_3jenis.1var = N4.Bayes
N4.Bayes.Normal_3jenis.1var = N4.Bayes.Normal
N4.Bayes.InvGaussian_3jenis.1var = N4.Bayes.InvGaussian
N5.Bayes_3jenis.1var = N5.Bayes
N5.Bayes.Normal_3jenis.1var = N5.Bayes.Normal
N5.Bayes.InvGaussian_3jenis.1var = N5.Bayes.InvGaussian
N6.Bayes_3jenis.1var = N6.Bayes
N6.Bayes.Normal_3jenis.1var = N6.Bayes.Normal
N6.Bayes.InvGaussian_3jenis.1var = N6.Bayes.InvGaussian

N4.Bayes_3jenis.fvar = N4.Bayes
N4.Bayes.Normal_3jenis.fvar = N4.Bayes.Normal
N4.Bayes.InvGaussian_3jenis.fvar = N4.Bayes.InvGaussian
N5.Bayes_3jenis.fvar = N5.Bayes
N5.Bayes.Normal_3jenis.fvar = N5.Bayes.Normal
N5.Bayes.InvGaussian_3jenis.fvar = N5.Bayes.InvGaussian
N6.Bayes_3jenis.fvar = N6.Bayes
N6.Bayes.Normal_3jenis.fvar = N6.Bayes.Normal
N6.Bayes.InvGaussian_3jenis.fvar = N6.Bayes.InvGaussian


# Data Total Freq dari Model Univariat ----
#Total.test.N = test.N1 + test.N2 + test.N3 + test.N4 + test.N5 + test.N6

# Data Total Sev dari Model Univariat ----
#Total.test.Y.Gamma = test.Y1.new + test.Y2.new + test.Y3.new + test.Y4.new + test.Y5.new + test.Y6.new
#Total.test.Y.Normal = test.Y1.Normal.new + test.Y2.Normal.new + test.Y3.Normal.new + 
#  test.Y4.Normal.new + test.Y5.Normal.new + test.Y6.Normal.new
#Total.test.Y.InvGaussian = test.Y1.InvGaussian.new + test.Y2.InvGaussian.new + test.Y3.InvGaussian.new + 
#  test.Y4.InvGaussian.new + test.Y5.InvGaussian.new + test.Y6.InvGaussian.new

# Data Total Premi Murni dari Model Univariat ----
#Total.test.X.Gamma = Total.test.N*Total.test.Y.Gamma
#Total.test.X.Normal = Total.test.N*Total.test.Y.Normal
#Total.test.X.InvGaussian = Total.test.N*Total.test.Y.InvGaussian

Total.test.X.Gamma = yhat.X1 + yhat.X2 + yhat.X3 + yhat.X4 + yhat.X5 + yhat.X6
Total.test.X.Normal = yhat.X1.PoiNorm + yhat.X2.PoiNorm + yhat.X3.PoiNorm + yhat.X4.PoiNorm + yhat.X5.PoiNorm + yhat.X6.PoiNorm
Total.test.X.InvGaussian = yhat.X1.PoiInvG + yhat.X2.PoiInvG + yhat.X3.PoiInvG + yhat.X4.PoiInvG + yhat.X5.PoiInvG + yhat.X6.PoiInvG

Total.test.X.new = yhat.X1.PoiNorm + yhat.X2.PoiInvG + yhat.X3 + yhat.X4.PoiNorm + N5.Bayes_2jenis.1var + N6.Bayes.Normal_3jenis.1var

# boxplot --------------
# Freq ------
par(mfrow = c(2, 2))
# par(mar = c(6, 2, 2, 1))  # Atur margin untuk memperkecil jarak antara baris
# par(las = 2)
boxplot(data.testing.Bayes$Damage.x, test.N1, names = c("Data Asli", "MLE"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Damage", ylim = c(0, 0.5), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Damage.x), mean(test.N1))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Fire.x, test.N2, names = c("Data Asli", "MLE"),
        main = "Boxplot Jumlah Klaim Jenis Jaminan Fire", ylim = c(0, 0.2), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Fire.x), mean(test.N2))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Other.x, test.N3, names = c("Data Asli", "MLE"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Other", ylim = c(0, 0.2), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Other.x), mean(test.N3))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Total.x, test.NT, 
        names = c("Data Asli", "MLE"), 
        main = "Boxplot Jumlah Klaim Total Jenis Jaminan", ylim = c(0, 2), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Total.x), mean(test.NT))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)



boxplot(data.testing.Bayes$Theft.x, test.N4, ekspektasi_N4_3jenis.1var, ekspektasi_N4_3jenis.fvar,
        names = c("Data Asli", "MLE", "Bayesian 3\njenis jaminan\n 1 var", "Bayesian 3\njenis jaminan\n 6 var"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Theft", ylim = c(0, 0.2) )
mean_value = c(mean(data.testing.Bayes$Theft.x), mean(test.N4), mean(ekspektasi_N4_3jenis.1var), 
               mean(ekspektasi_N4_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$TPL.x, test.N5, ekspektasi_N5_2jenis.1var, ekspektasi_N5_2jenis.fvar, 
        ekspektasi_N5_3jenis.1var, ekspektasi_N5_3jenis.fvar,
        names = c("Data Asli", "MLE", "Bayesian 2\njenis jaminan\n 1 var", "Bayesian 2\njenis jaminan\n 5 var",
                  "Bayesian 3\njenis jaminan\n 1 var", "Bayesian 3\njenis jaminan\n 6 var"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan TPL", ylim = c(0, 2), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$TPL.x), mean(test.N5), mean(ekspektasi_N5_2jenis.1var), 
               mean(ekspektasi_N5_2jenis.fvar), mean(ekspektasi_N5_3jenis.1var), mean(ekspektasi_N5_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Windscreen.x, test.N6, ekspektasi_N6_2jenis.1var, ekspektasi_N6_2jenis.fvar,
        ekspektasi_N6_3jenis.1var, ekspektasi_N6_3jenis.fvar,
        names = c("Data Asli", "MLE", "Bayesian 2\njenis jaminan\n 1 var", "Bayesian 2\njenis jaminan\n 5 var",
                  "Bayesian 3\njenis jaminan\n 1 var", "Bayesian 3\njenis jaminan\n 6 var"), 
        main = "Boxplot Jumlah Klaim Jenis Jaminan Windscreen", ylim = c(0, 2), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Windscreen.x), mean(test.N6), mean(ekspektasi_N6_2jenis.1var), 
               mean(ekspektasi_N6_2jenis.fvar), mean(ekspektasi_N6_3jenis.1var), mean(ekspektasi_N6_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)



# Premi Murni -------
par(mfrow = c(3, 1))
par(las = 1)
par(mar = c(5, 3, 4, 2))  # Atur margin untuk memperkecil jarak antara baris
boxplot(data.testing.Bayes$Damage.y, yhat.X1, yhat.X1.PoiNorm, yhat.X1.PoiInvG,
        ylim = c(0, 800), 
        names = c("Data Asli", "Poisson MLE\n-Gamma MLE", "Poisson MLE\n-Normal MLE", "Poisson MLE\n-Inverse\nGaussian MLE"), 
        main = "Boxplot Premi Murni Jenis Jaminan Damage")
mean_value = c(mean(data.testing.Bayes$Damage.y), mean(yhat.X1), mean(yhat.X1.PoiNorm), mean(yhat.X1.PoiInvG))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Fire.y, yhat.X2, yhat.X2.PoiNorm, yhat.X2.PoiInvG,
        ylim = c(0, 100), 
        names = c("Data Asli", "Poisson MLE\n-Gamma MLE", "Poisson MLE\n-Normal MLE", "Poisson MLE\n-Inverse\nGaussian MLE"), 
        main = "Boxplot Premi Murni Jenis Jaminan Fire")
mean_value = c(mean(data.testing.Bayes$Fire.y), mean(yhat.X2), mean(yhat.X2.PoiNorm), mean(yhat.X2.PoiInvG))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Other.y, yhat.X3, yhat.X3.PoiNorm, yhat.X3.PoiInvG,
        ylim = c(0, 100), 
        names = c("Data Asli", "Poisson MLE\n-Gamma MLE", "Poisson MLE\n-Normal MLE", "Poisson MLE\n-Inverse\nGaussian MLE"), 
        main = "Boxplot Premi Murni Jenis Jaminan Other")
mean_value = c(mean(data.testing.Bayes$Other.y), mean(yhat.X3), mean(yhat.X3.PoiNorm), mean(yhat.X3.PoiInvG))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

par(mfrow = c(1, 1))
boxplot(data.testing.Bayes$Theft.y, yhat.X4, N4.Bayes_3jenis.1var, N4.Bayes_3jenis.fvar,
        yhat.X4.PoiNorm, N4.Bayes.Normal_3jenis.1var, N4.Bayes.Normal_3jenis.fvar,
        yhat.X4.PoiInvG, N4.Bayes.InvGaussian_3jenis.1var,  N4.Bayes.InvGaussian_3jenis.fvar, 
        ylim = c(0, 300), 
        names = c("Data Asli", "Poisson MLE\n-Gamma MLE", "Bayesian 3\njenis jaminan\n1 var Poi-Gamma", "Bayesian 3\njenis jaminan\n6 var Poi-Gamma", 
                  "Poisson MLE\n-Normal MLE", "Bayesian 3\njenis jaminan\n1 var Poi-Normal", "Bayesian 3\njenis jaminan\n6 var Poi-Normal", 
                  "Poisson MLE\n-Inverse\nGaussian MLE", "Bayesian 3\njenis jaminan\n1 var Poi-Inverse\nGaussian", "Bayesian 3\njenis jaminan\n6 var Poi-Inverse\nGaussian"), 
        main = "Boxplot Premi Murni Jenis Jaminan Theft", cex.axis = 0.8)
mean_value = c(mean(data.testing.Bayes$Theft.y), mean(yhat.X4), mean(N4.Bayes_3jenis.1var), mean(N4.Bayes_3jenis.fvar),
               mean(yhat.X4.PoiNorm), mean(N4.Bayes.Normal_3jenis.1var), mean(N4.Bayes.Normal_3jenis.fvar), 
               mean(yhat.X4.PoiInvG), mean(N4.Bayes.InvGaussian_3jenis.1var), mean(N4.Bayes.InvGaussian_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$TPL.y, 
        yhat.X5, N5.Bayes_2jenis.1var, N5.Bayes_2jenis.fvar, N5.Bayes_3jenis.1var, N5.Bayes_3jenis.fvar,
        yhat.X5.PoiNorm, N5.Bayes.Normal_2jenis.1var, N5.Bayes.Normal_2jenis.fvar, N5.Bayes.Normal_3jenis.1var, N5.Bayes.Normal_3jenis.fvar,
        yhat.X5.PoiInvG, N5.Bayes.InvGaussian_2jenis.1var, N5.Bayes.InvGaussian_2jenis.fvar, N5.Bayes.InvGaussian_3jenis.1var, N5.Bayes.InvGaussian_3jenis.fvar,
        ylim = c(0, 2000), 
        names = c("Data Asli", 
                  "Poisson MLE\n-Gamma MLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-Gamma", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-Gamma",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-Gamma", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-Gamma", 
                  "Poisson MLE\n-Normal MLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-Normal", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-Normal",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-Normal", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-Normal",
                  "Poisson MLE\n-Inverse\nGaussian MLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-Inverse\nGaussian", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-Inverse\nGaussian",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-Inverse\nGaussian", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-Inverse\nGaussian"), 
        main = "Boxplot Premi Murni Jenis Jaminan TPL", cex.axis = 0.7)
mean_value = c(mean(data.testing.Bayes$TPL.y), 
               mean(yhat.X5), mean(N5.Bayes_2jenis.1var), mean(N5.Bayes_2jenis.fvar), mean(N5.Bayes_3jenis.1var), mean(N5.Bayes_3jenis.fvar), 
               mean(yhat.X5.PoiNorm), mean(N5.Bayes.Normal_2jenis.1var), mean(N5.Bayes.Normal_2jenis.fvar), 
               mean(N5.Bayes.Normal_3jenis.1var), mean(N5.Bayes.Normal_3jenis.fvar), 
               mean(yhat.X5.PoiInvG), mean(N5.Bayes.InvGaussian_2jenis.1var), mean(N5.Bayes.InvGaussian_2jenis.fvar), 
               mean(N5.Bayes.InvGaussian_3jenis.1var), mean(N5.Bayes.InvGaussian_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Windscreen.y, 
        yhat.X6, N6.Bayes_2jenis.1var, N6.Bayes_2jenis.fvar, N6.Bayes_3jenis.1var, N6.Bayes_3jenis.fvar, 
        yhat.X6.PoiNorm, N6.Bayes.Normal_2jenis.1var, N6.Bayes.Normal_2jenis.fvar, N6.Bayes.Normal_3jenis.1var, N6.Bayes.Normal_3jenis.fvar, 
        yhat.X6.PoiInvG, N6.Bayes.InvGaussian_2jenis.1var, N6.Bayes.InvGaussian_2jenis.fvar, N6.Bayes.InvGaussian_3jenis.1var, N6.Bayes.InvGaussian_3jenis.fvar,
        ylim = c(0, 500), 
        names = c("Data Asli", 
                  "Poisson MLE\n-Gamma MLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-Gamma", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-Gamma",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-Gamma", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-Gamma", 
                  "Poisson MLE\n-Normal MLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-Normal", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-Normal",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-Normal", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-Normal",
                  "Poisson MLE\n-Inverse\nGaussian\nMLE", "Bayesian\n2 jenis\njaminan\n1 var\nPoi-InvGauss", "Bayesian\n2 jenis\njaminan\n5 var\nPoi-InvGauss",
                  "Bayesian\n3 jenis\njaminan\n1 var\nPoi-InvGauss", "Bayesian\n3 jenis\njaminan\n6 var\nPoi-InvGauss"),
        main = "Boxplot Premi Murni Jenis Jaminan Windscreen", cex.axis = 0.7)
mean_value = c(mean(data.testing.Bayes$Windscreen.y), 
               mean(yhat.X6), mean(N6.Bayes_2jenis.1var), mean(N6.Bayes_2jenis.fvar), mean(N6.Bayes_3jenis.1var), mean(N6.Bayes_3jenis.fvar), 
               mean(yhat.X6.PoiNorm), mean(N6.Bayes.Normal_2jenis.1var), mean(N6.Bayes.Normal_2jenis.fvar), 
               mean(N6.Bayes.Normal_3jenis.1var), mean(N6.Bayes.Normal_3jenis.fvar), 
               mean(yhat.X6.PoiInvG), mean(N6.Bayes.InvGaussian_2jenis.1var), mean(N6.Bayes.InvGaussian_2jenis.fvar), 
               mean(N6.Bayes.InvGaussian_3jenis.1var), mean(N6.Bayes.InvGaussian_3jenis.fvar))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Total.y, yhat.XT, yhat.XT.PoiNorm, yhat.XT.PoiInvG,
        Total.test.X.Gamma, Total.test.X.Normal, Total.test.X.InvGaussian, Total.test.X.new, 
        ylim = c(0, 5000), 
        names = c("Data Asli", "Premi Total\nPoisson MLE\n-Gamma MLE", "Premi Total\nPoisson MLE\n-Normal MLE", 
                  "Premi Total\nPoisson MLE\n-Inverse\nGaussian MLE",
                  "Jumlah Model\nUnivariat\nPoisson MLE\n-Gamma MLE", "Jumlah Model\nUnivariat\nPoisson MLE\n-Normal MLE",
                  "Jumlah Model\nUnivariat\nPoisson MLE\n-Inverse\nGaussian MLE", "Jumlah Model\nUnivariat\nTerbaik\nBerdasarkan\nNilai Rataan"), 
        main = "Boxplot Premi Murni Total Jenis Jaminan")
mean_value = c(mean(data.testing.Bayes$Total.y), mean(yhat.XT), mean(yhat.XT.PoiNorm), mean(yhat.XT.PoiInvG), 
               mean(Total.test.X.Gamma), mean(Total.test.X.Normal), mean(Total.test.X.InvGaussian), mean(Total.test.X.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)




summary(data.testing.Bayes$Windscreen.y)
quantile(data.testing.Bayes$Windscreen.y, c(0.025, 0.975))
quantile(data.testing.Bayes$Windscreen.y, 0.5)



# Severity ---------------------
par(mfrow = c(2, 2))
boxplot(data.testing.Bayes$Damage.Sev, test.Y1.new, test.Y1.Normal.new, test.Y1.InvGaussian.new,
        names = c("Data Asli", "Severity\nDamage\nGamma", "Severity\nDamage\nNormal", "Severity\nDamage\nInverse\nGaussian"),
        main = "Severity Damage", ylim = c(0, 3000))
mean_value = c(mean(data.testing.Bayes$Damage.Sev), mean(test.Y1.new), 
               mean(test.Y1.Normal.new), mean(test.Y1.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Fire.Sev, test.Y2.new, test.Y2.Normal.new, test.Y2.InvGaussian.new,
        names = c("Data Asli", "Severity\nFire\nGamma", "Severity\nFire\nNormal", "Severity\nFire\nInverse\nGaussian"),
        main = "Severity Fire", ylim = c(0, 8000))
mean_value = c(mean(data.testing.Bayes$Fire.Sev), mean(test.Y2.new), 
               mean(test.Y2.Normal.new), mean(test.Y2.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Other.Sev, test.Y3.new, test.Y3.Normal.new, test.Y3.InvGaussian.new,
        names = c("Data Asli", "Severity\nOther\nGamma", "Severity\nOther\nNormal", "Severity\nOther\nInverse\nGaussian"),
        main = "Severity Other", ylim = c(0, 3000))
mean_value = c(mean(data.testing.Bayes$Other.Sev), mean(test.Y3.new), 
               mean(test.Y3.Normal.new), mean(test.Y3.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Theft.Sev, test.Y4.new, test.Y4.Normal.new, test.Y4.InvGaussian.new,
        names = c("Data Asli", "Severity\nTheft\nGamma", "Severity\nTheft\nNormal", "Severity\nTheft\nInverse\nGaussian"),
        main = "Severity Theft", ylim = c(0, 3000))
mean_value = c(mean(data.testing.Bayes$Theft.Sev), mean(test.Y4.new), 
               mean(test.Y4.Normal.new), mean(test.Y4.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$TPL.Sev, test.Y5.new, test.Y5.Normal.new, test.Y5.InvGaussian.new,
        names = c("Data Asli", "Severity\nTPL\nGamma", "Severity\nTPL\nNormal", "Severity\nTPL\nInverse\nGaussian"),
        main = "Severity TPL", ylim = c(0, 3000))
mean_value = c(mean(data.testing.Bayes$TPL.Sev), mean(test.Y5.new), 
               mean(test.Y5.Normal.new), mean(test.Y5.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Windscreen.Sev, test.Y6.new, test.Y6.Normal.new, test.Y6.InvGaussian.new,
        names = c("Data Asli", "Severity\nWindscreen\nGamma", "Severity\nWindscreen\nNormal", "Severity\nWindscreen\nInverse\nGaussian"),
        main = "Severity Windscreen", ylim = c(0, 3000), cex.axis = 0.9)
mean_value = c(mean(data.testing.Bayes$Windscreen.Sev), mean(test.Y6.new), 
               mean(test.Y6.Normal.new), mean(test.Y6.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)

boxplot(data.testing.Bayes$Total.Sev, test.YT.new, test.YT.Normal.new, test.YT.InvGaussian.new,
        names = c("Data Asli", "Severity\nTotal\nGamma", "Severity\nTotal\nNormal", "Severity\nTotal\nInverse\nGaussian"),
        main = "Severity Total", ylim = c(0, 3000))
mean_value = c(mean(data.testing.Bayes$Total.Sev), mean(test.YT.new), 
               mean(test.YT.Normal.new), mean(test.YT.InvGaussian.new))
points(mean_value, col = "red", pch = 19)
text(mean_value, labels = round(mean_value, 2), pos = 3)
legend("topright", legend = "Rataan", col = "red", pch = 19)
