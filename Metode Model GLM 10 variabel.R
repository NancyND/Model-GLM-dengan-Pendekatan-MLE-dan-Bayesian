# OLAH DATA ------------------------
load("fremotor1freq0304a.rda")
load("fremotor1prem0304a.rda")
load("fremotor1sev0304a.rda")

datafreq = fremotor1freq0304a
datasev = fremotor1sev0304a
datasev$Year = format(as.Date(datasev$OccurDate), "%Y")
datasev$Year = as.numeric(datasev$Year)
dataprem = fremotor1prem0304a


summary(datafreq)
summary(datasev)
summary(dataprem)


datafreq2003 = datafreq[datafreq$Year == 2003, ]
datasev2003 = datasev[datasev$Year == 2003, ]
dataprem2003 = dataprem[dataprem$Year == 2003, ]

datafreq2004 = datafreq[datafreq$Year == 2004, ]
datasev2004 = datasev[datasev$Year == 2004, ]
dataprem2004 = dataprem[dataprem$Year == 2004, ]

# buat kolom "Damage, Fire, dll" di datasev
for (i in 1:nrow(datasev2003)) {
  datasev2003$Damage[i] = ifelse(datasev2003$Guarantee[i] == "Damage", datasev2003$Payment[i], 0)
  datasev2003$Fire[i] = ifelse(datasev2003$Guarantee[i] == "Fire", datasev2003$Payment[i], 0)
  datasev2003$Other[i] = ifelse(datasev2003$Guarantee[i] == "Other", datasev2003$Payment[i], 0)
  datasev2003$Theft[i] = ifelse(datasev2003$Guarantee[i] == "Theft", datasev2003$Payment[i], 0)
  datasev2003$TPL[i] = ifelse(datasev2003$Guarantee[i] == "TPL", datasev2003$Payment[i], 0)
  datasev2003$Windscreen[i] = ifelse(datasev2003$Guarantee[i] == "Windscreen", datasev2003$Payment[i], 0)
}

for (i in 1:nrow(datasev2004)) {
  datasev2004$Damage[i] = ifelse(datasev2004$Guarantee[i] == "Damage", datasev2004$Payment[i], 0)
  datasev2004$Fire[i] = ifelse(datasev2004$Guarantee[i] == "Fire", datasev2004$Payment[i], 0)
  datasev2004$Other[i] = ifelse(datasev2004$Guarantee[i] == "Other", datasev2004$Payment[i], 0)
  datasev2004$Theft[i] = ifelse(datasev2004$Guarantee[i] == "Theft", datasev2004$Payment[i], 0)
  datasev2004$TPL[i] = ifelse(datasev2004$Guarantee[i] == "TPL", datasev2004$Payment[i], 0)
  datasev2004$Windscreen[i] = ifelse(datasev2004$Guarantee[i] == "Windscreen", datasev2004$Payment[i], 0)
}

data01 = merge(datafreq2003, datasev2003, by = "IDpol")
summary(data01)
data01 = merge(data01, dataprem2003, by = "IDpol")
data01$PremTPL = data01$PremTPLM + data01$PremTPLV
data01$PremOther = data01$PremAcc1 + data01$PremAcc2 + data01$PremLegal + data01$PremServ
data01$BonusMalus = ifelse(data01$BonusMalus < 100, "Bonus", "Malus") # < 100 = bonus = 1 
data01$BonusMalus = factor(data01$BonusMalus)

data02 = merge(datafreq2004, datasev2004, by = "IDpol")
summary(data02)
data02 = merge(data02, dataprem2004, by = "IDpol")
data02$PremTPL = data02$PremTPLM + data02$PremTPLV
data02$PremOther = data02$PremAcc1 + data02$PremAcc2 + data02$PremLegal + data02$PremServ
data02$BonusMalus = ifelse(data02$BonusMalus < 100, "Bonus", "Malus") # < 100 = bonus = 1
data02$BonusMalus = factor(data02$BonusMalus)

sum(duplicated(data01))
sum(duplicated(data02))

summary(data01)
summary(data02)

nrow(data01[data01$Damage.x == 0 & data01$Fire.x == 0 & data01$Other.x == 0 & data01$Theft.x == 0 & data01$TPL.x == 0 & data01$Windscreen.x == 0, ])
nrow(data01[data01$Damage.y == 0 & data01$Fire.y == 0 & data01$Other.y == 0 & data01$Theft.y == 0 & data01$TPL.y == 0 & data01$Windscreen.y == 0, ])
nrow(data01[data01$Damage.y != 0, ])
nrow(data01[data01$Fire.y != 0, ])
nrow(data01[data01$Other.y != 0, ])
nrow(data01[data01$Theft.y != 0, ])
nrow(data01[data01$TPL.y != 0, ])
nrow(data01[data01$Windscreen.y != 0, ])

411+10+63+241+2074+1566
411+10+63+241+2074+1566+169

# 169 data ada klaim tapi 0 sev, jadi datanya dihapus
datafix = subset(data01, !(Damage.y == 0 & Fire.y == 0 & Other.y == 0 & Theft.y == 0 & TPL.y == 0 & Windscreen.y == 0))
summary(datafix)

nrow(data02[data02$Damage.x == 0 & data02$Fire.x == 0 & data02$Other.x == 0 & data02$Theft.x == 0 & data02$TPL.x == 0 & data02$Windscreen.x == 0, ])
nrow(data02[data02$Damage.y == 0 & data02$Fire.y == 0 & data02$Other.y == 0 & data02$Theft.y == 0 & data02$TPL.y == 0 & data02$Windscreen.y == 0, ])
nrow(data02[data02$Damage.y != 0, ])
nrow(data02[data02$Fire.y != 0, ])
nrow(data02[data02$Other.y != 0, ])
nrow(data02[data02$Theft.y != 0, ])
nrow(data02[data02$TPL.y != 0, ])
nrow(data02[data02$Windscreen.y != 0, ])

114+318+65+86+233+1409+1173
318+65+86+233+1409+1173

# 114 data ada klaim tapi 0 sev, jadi datanya dihapus
datafix2 = subset(data02, !(Damage.y == 0 & Fire.y == 0 & Other.y == 0 & Theft.y == 0 & TPL.y == 0 & Windscreen.y == 0))
summary(datafix2)

# buat data severity per klaim setiap jenis jaminan
datafix$Damage.Sev = ifelse(datafix$Damage.x == 0, 0, datafix$Damage.y/datafix$Damage.x)
datafix$Fire.Sev = ifelse(datafix$Fire.x == 0, 0, datafix$Fire.y/datafix$Fire.x)
datafix$Other.Sev = ifelse(datafix$Other.x == 0, 0, datafix$Other.y/datafix$Other.x)
datafix$Theft.Sev = ifelse(datafix$Theft.x == 0, 0, datafix$Theft.y/datafix$Theft.x)
datafix$TPL.Sev = ifelse(datafix$TPL.x == 0, 0, datafix$TPL.y/datafix$TPL.x)
datafix$Windscreen.Sev = ifelse(datafix$Windscreen.x == 0, 0, datafix$Windscreen.y/datafix$Windscreen.x)
summary(datafix)

datafix2$Damage.Sev = ifelse(datafix2$Damage.x == 0, 0, datafix2$Damage.y/datafix2$Damage.x)
datafix2$Fire.Sev = ifelse(datafix2$Fire.x == 0, 0, datafix2$Fire.y/datafix2$Fire.x)
datafix2$Other.Sev = ifelse(datafix2$Other.x == 0, 0, datafix2$Other.y/datafix2$Other.x)
datafix2$Theft.Sev = ifelse(datafix2$Theft.x == 0, 0, datafix2$Theft.y/datafix2$Theft.x)
datafix2$TPL.Sev = ifelse(datafix2$TPL.x == 0, 0, datafix2$TPL.y/datafix2$TPL.x)
datafix2$Windscreen.Sev = ifelse(datafix2$Windscreen.x == 0, 0, datafix2$Windscreen.y/datafix2$Windscreen.x)
summary(datafix2)

# gabungkan data tahun 2003 dan 2004
mydata = rbind(datafix, datafix2)
#set.seed(123)
library(caret)
#trainIndex = createDataPartition(mydata$Fire.Sev, p = 0.7, list = FALSE)
#trainData = mydata[trainIndex, ]
#testData = mydata[-trainIndex, ]

mydata$Total.x = mydata$Damage.x + mydata$Fire.x + mydata$Other.x + mydata$Theft.x + mydata$TPL.x + mydata$Windscreen.x
mydata$Total.y = mydata$Damage.y + mydata$Fire.y + mydata$Other.y + mydata$Theft.y + mydata$TPL.y + mydata$Windscreen.y
mydata$Total.Sev = mydata$Total.y/mydata$Total.x

summary(mydata)
#summary(trainData)
#summary(testData)
summary(mydata$Area)
summary(mydata$VehClass)

##################################################################
# cek korelasi variabel independent

cekdata = mydata
summary(cekdata)

#17 variabel
cekdata$DrivGender = ifelse(cekdata$DrivGender == "F", 1, 2)
cekdata$BonusMalus = ifelse(cekdata$BonusMalus == "Bonus", 1, 2)
cekdata$PayFreq = ifelse(cekdata$PayFreq == "Annual", 1, 
                         ifelse(cekdata$PayFreq == "Half-yearly", 2, 
                                ifelse(cekdata$PayFreq == "Monthly", 3, 4)))
cekdata$VehClass = ifelse(cekdata$VehClass == "Cheap", 1, 
                          ifelse(cekdata$VehClass == "Cheaper", 2, 
                                 ifelse(cekdata$VehClass == "Cheapest", 3, 
                                        ifelse(cekdata$VehClass == "Expensive", 4, 
                                               ifelse(cekdata$VehClass == "Medium", 5, 
                                                      ifelse(cekdata$VehClass == "Medium high", 6, 
                                                             ifelse(cekdata$VehClass == "Medium low", 7, 
                                                                    ifelse(cekdata$VehClass == "More expensive", 8, 9))))))))
summary(cekdata$VehClass)

cekdata$VehPower = ifelse(cekdata$VehPower == "P2", 1, 
                          ifelse(cekdata$VehPower == "P4", 2, 
                                 ifelse(cekdata$VehPower == "P5", 3, 
                                        ifelse(cekdata$VehPower == "P6", 4, 
                                               ifelse(cekdata$VehPower == "P7", 5, 
                                                      ifelse(cekdata$VehPower == "P8", 6, 
                                                             ifelse(cekdata$VehPower == "P9", 7, 
                                                                    ifelse(cekdata$VehPower == "P10", 8, 
                                                                           ifelse(cekdata$VehPower == "P11", 9, 
                                                                                  ifelse(cekdata$VehPower == "P12", 10, 
                                                                                         ifelse(cekdata$VehPower == "P13", 11, 
                                                                                                ifelse(cekdata$VehPower == "P14", 12, 
                                                                                                       ifelse(cekdata$VehPower == "P15", 13, 
                                                                                                              ifelse(cekdata$VehPower == "P16", 14, 15))))))))))))))
summary(cekdata$VehPower)

cekdata$VehGas = ifelse(cekdata$VehGas == "Diesel", 1, 2)
cekdata$VehUsage = ifelse(cekdata$VehUsage == "Private+trip to office", 1, 
                          ifelse(cekdata$VehUsage == "Professional", 2, 3))
cekdata$Garage = ifelse(cekdata$Garage == "Closed collective parking", 1, 
                        ifelse(cekdata$Garage == " Closed zbox", 2, 
                               ifelse(cekdata$Garage == "Opened collective parking", 3, 4)))
cekdata$Area = ifelse(cekdata$Area == "A2", 1, 
                      ifelse(cekdata$Area == "A3", 2,
                             ifelse(cekdata$Area == "A4", 3,
                                    ifelse(cekdata$Area == "A5", 4, 
                                           ifelse(cekdata$Area == "A6", 5, 
                                                  ifelse(cekdata$Area == "A7", 6, 
                                                         ifelse(cekdata$Area == "A8", 7, 
                                                                ifelse(cekdata$Area == "A9", 8, 
                                                                       ifelse(cekdata$Area == "A10", 9, 10)))))))))
summary(cekdata$Area)

cekdata$Region = ifelse(cekdata$Region == "Center", 1, 
                        ifelse(cekdata$Region == "Headquarters", 2, 
                               ifelse(cekdata$Region == "Paris area", 3, 4)))
cekdata$Channel = ifelse(cekdata$Channel == "A", 1, 
                         ifelse(cekdata$Channel == "B", 2, 3))
cekdata$Marketing = ifelse(cekdata$Marketing == "M1", 1, 
                           ifelse(cekdata$Marketing == "M2", 2, 
                                  ifelse(cekdata$Marketing == "M3", 3, 4)))
summary(cekdata)

kordata = cekdata[, c(3, 4, 5, 6, 7, 8, 14, 15, 16, 17, 18, 19, 51, 52, 53, 54, 55, 56, 57, 58, 59, 21, 22, 24, 25, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)]
kordata = matrix(kordata)
str(kordata)
kordata$LicenceNb = as.numeric(kordata$LicenceNb)
length(kordata$LicenceNb)
length(kordata$Damage.x)

# Menghitung matriks korelasi
correlation_matrix <- cor(kordata)

summary(mydata$MaritalStatus)

correlation_matrix = data.frame(correlation_matrix)

highlight_values <- function(x, threshold) {
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      x[i, j] = ifelse(x[i, j] >= threshold, NA, x[i, j])
    }
  }
  return(x)
}

correlation_matrix1 = highlight_values(correlation_matrix, 0.1)
cor(correlation_matrix)

##################################################################


# ubah VehClass menjadi 3 kategori atau 2 kategori
#mydata$VehClass = ifelse(mydata$VehClass == "Cheap", "Cheap", 
#                         ifelse(mydata$VehClass == "Cheaper", "Cheaper", "Other"))
mydata$VehClass = ifelse(mydata$VehClass == "Cheap", "Cheap or Cheaper", 
                         ifelse(mydata$VehClass == "Cheaper", "Cheap or Cheaper", "Other"))
mydata$VehClass = factor(mydata$VehClass)

# ubah Area menjadi 3 kategori
mydata$Area = ifelse(mydata$Area == "A2", "A2-A3", 
                     ifelse(mydata$Area == "A3", "A2-A3",
                            ifelse(mydata$Area == "A4", "A4-A6",
                                   ifelse(mydata$Area == "A5", "A4-A6", 
                                          ifelse(mydata$Area == "A6", "A4-A6", "A7-A10,A12")))))
mydata$Area = factor(mydata$Area)
summary(mydata$Area)

# ubah PayFreq menjadi 3 kategori
mydata$PayFreq = ifelse(mydata$PayFreq == "Annual", "Annual", 
                        ifelse(mydata$PayFreq == "Half-yearly", "Half-yearly", "Other"))
mydata$PayFreq = factor(mydata$PayFreq)

# ubah Garage menjadi 2 kategori
mydata$Garage = ifelse(mydata$Garage == "Closed collective parking", "Closed parking", 
                       ifelse(mydata$Garage == " Closed zbox", "Closed parking", "Opened parking"))
mydata$Garage = factor(mydata$Garage)

# ubah Region menjadi 2 kategori
mydata$Region = ifelse(mydata$Region == "Center", "Center or Headquarters", 
                       ifelse(mydata$Region == "Headquarters", "Center or Headquarters", "Paris area or South West"))
mydata$Region = factor(mydata$Region)

# ubah VehPower menjadi 3 kategori
mydata$VehPower = ifelse(mydata$VehPower == "P2", "P2-P9", 
                         ifelse(mydata$VehPower == "P4", "P2-P9", 
                                ifelse(mydata$VehPower == "P5", "P2-P9", 
                                       ifelse(mydata$VehPower == "P6", "P2-P9", 
                                              ifelse(mydata$VehPower == "P7", "P2-P9", 
                                                     ifelse(mydata$VehPower == "P8", "P2-P9", 
                                                            ifelse(mydata$VehPower == "P9", "P2-P9", 
                                                                   ifelse(mydata$VehPower == "P10", "P10-P13", 
                                                                          ifelse(mydata$VehPower == "P11", "P10-P13", 
                                                                                 ifelse(mydata$VehPower == "P12", "P10-P13", 
                                                                                        ifelse(mydata$VehPower == "P13", "P10-P13", "P14-P17")))))))))))
mydata$VehPower = factor(mydata$VehPower)

mydata$LicenceNb = factor(mydata$LicenceNb)
summary(mydata$LicenceNb)

summary(mydata)
summary(mydata$VehPower)


mydata1 = mydata[c(3, 4, 5, 6, 7, 8, 14, 15, 16, 17, 18, 19, 
                   21, 22, 24, 26, 28, 29, 30, 31, 33, 35,
                   51, 52, 53, 54, 55, 56, 57, 58, 59)]
summary(mydata1)

#mydata1$Damage.x = factor(mydata1$Damage.x)
#mydata1$Fire.x = factor(mydata1$Fire.x)
#mydata1$Other.x = factor(mydata1$Other.x)
#mydata1$Theft.x = factor(mydata1$Theft.x)
#mydata1$TPL.x = factor(mydata1$TPL.x)
#mydata1$Windscreen.x = factor(mydata1$Windscreen.x)
#mydata1$Total.x = factor(mydata1$Total.x)

sum(duplicated(mydata1))
duplicates <- mydata1[duplicated(mydata1) | duplicated(mydata1, fromLast = TRUE), ]
mydata1 <- unique(mydata1)
###################################################################################################



# FUNGSI LOOPING SIMULASI POISSON
simul_N = function(mu, threshold){
  N = NULL
  for(i in 1:length(mu)){
    y_hat = rpois(10001, mu[i])
    N[i] = quantile(y_hat, threshold)
  }
  simul_N = N
}


# pahami dengan manual!

#test.N1[8]
#y_hat = rpois(10001, test.N1[8])
#coba1 = quantile(y_hat, 0.87)
#10000*0.87
#y_hat[8700]

###################################################################################################


# MODEL GLM N1, N2, ..., N6, NT

# data training dan testing
set.seed(123)
trainIndex.DX = createDataPartition(mydata1$Damage.x, p = 0.7, list = FALSE)
trainData.DX = mydata1[trainIndex.DX, ]
testData.DX = mydata1[-trainIndex.DX, ]
summary(testData.DX)

trainIndex.FX = createDataPartition(mydata1$Fire.x, p = 0.7, list = FALSE)
trainData.FX = mydata1[trainIndex.FX, ]
testData.FX = mydata1[-trainIndex.FX, ]
summary(testData.FX)

trainIndex.OX = createDataPartition(mydata1$Other.x, p = 0.7, list = FALSE)
trainData.OX = mydata1[trainIndex.OX, ]
testData.OX = mydata1[-trainIndex.OX, ]
summary(testData.OX)

trainIndex.TX = createDataPartition(mydata1$Theft.x, p = 0.7, list = FALSE)
trainData.TX = mydata1[trainIndex.TX, ]
testData.TX = mydata1[-trainIndex.TX, ]
summary(testData.TX)

trainIndex.TPLX = createDataPartition(mydata1$TPL.x, p = 0.7, list = FALSE)
trainData.TPLX = mydata1[trainIndex.TPLX, ]
testData.TPLX = mydata1[-trainIndex.TPLX, ]
summary(testData.TPLX)

trainIndex.WX = createDataPartition(mydata1$Windscreen.x, p = 0.7, list = FALSE)
trainData.WX = mydata1[trainIndex.WX, ]
testData.WX = mydata1[-trainIndex.WX, ]
summary(testData.WX)

trainIndex.TotalX = createDataPartition(mydata1$Total.x, p = 0.7, list = FALSE)
trainData.TotalX = mydata1[trainIndex.TotalX, ]
testData.TotalX = mydata1[-trainIndex.TotalX, ]
summary(testData.TotalX)

# Model Frekuensi -------------
# Model jaminan Damage
library(recipes)
mDamage.N1 = glm(Damage.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                   VehPower + VehGas + Garage + Region, data = trainData.DX, 
                 family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mDamage.N1)

test.N1 = predict(mDamage.N1, newdata = testData.DX, type = "response", interval = "confidence")
summary(test.N1)
#threshold = 0.87
yhat.N1 = simul_N(test.N1, 0.87)
table(yhat.N1)
table(testData.DX$Damage.x, yhat.N1)

plot(testData.DX$VehAge, yhat.N1)

# Model jaminan Fire
mFire.N2 = glm(Fire.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                 VehPower + VehGas + Garage + Region, data = trainData.FX, 
               family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mFire.N2)

test.N2 = predict(mFire.N2, newdata = testData.FX, type = "response", interval = "confidence")
summary(test.N2)
yhat.N2 = simul_N(test.N2, 0.97)
table(testData.FX$Fire.x, yhat.N2)


# Model jaminan Other
mOther.N3 = glm(Other.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.OX, 
                family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mOther.N3)

test.N3 = predict(mOther.N3, newdata = testData.OX, type = "response", interval = "confidence")
summary(test.N3)
yhat.N3 = simul_N(test.N3, 0.95)
table(testData.OX$Other.x, yhat.N3)

plot(testData.OX$Region, yhat.N3)

# Model jaminan Theft
mTheft.N4 = glm(Theft.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.TX, 
                family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTheft.N4)

test.N4 = predict(mTheft.N4, newdata = testData.TX, type = "response", interval = "confidence")
summary(test.N4)
yhat.N4 = simul_N(test.N4, 0.88)
table(testData.TX$Theft.x, yhat.N4)

plot(testData.TX$VehAge, yhat.N4)
plot(testData.TX$Region, yhat.N4)

# Model jaminan TPL
mTPL.N5 = glm(TPL.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                VehPower + VehGas + Garage + Region, data = trainData.TPLX, 
              family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTPL.N5)

test.N5 = predict(mTPL.N5, newdata = testData.TPLX, type = "response", interval = "confidence")
summary(test.N5)
yhat.N5 = simul_N(test.N5, 0.87)
table(testData.TPLX$TPL.x, yhat.N5)

plot(testData.TPLX$BonusMalus, yhat.N5)

# Model jaminan Windscreen
mWindscreen.N6 = glm(Windscreen.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                       VehPower + VehGas + Garage + Region, data = trainData.WX, 
                     family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mWindscreen.N6)

#library(car)
#vif(mWindscreen.N6)

test.N6 = predict(mWindscreen.N6, newdata = testData.WX, type = "response", interval = "confidence")
summary(test.N6)
yhat.N6 = simul_N(test.N6, 0.91)
table(testData.WX$Windscreen.x, yhat.N6)

plot(testData.WX$BonusMalus, yhat.N6)
plot(testData.WX$VehGas, yhat.N6)

# Model semua jaminan
mTotal.NT = glm(Total.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.TotalX, 
                family = poisson(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTotal.NT)

test.NT = predict(mTotal.NT, newdata = testData.TotalX, type = "response", interval = "confidence")
summary(test.NT)
yhat.NT = simul_N(test.NT, 0.87)
table(testData.TotalX$Total.x, yhat.NT)

plot(testData.TotalX$VehAge, yhat.NT)

###################################################################################################


# MODEL GLM Y1, Y2, ..., Y6, YT     (ingat dalam data .Sev)

# data training dan testing besar klaim ----------
set.seed(123)
mydata1.Damage = mydata1[mydata1$Damage.Sev != 0, ]
trainIndex.DSev = createDataPartition(mydata1.Damage$Damage.Sev, p = 0.7, list = FALSE)
trainData.DSev = mydata1.Damage[trainIndex.DSev, ]
testData.DSev = mydata1.Damage[-trainIndex.DSev, ]
summary(trainData.DSev)
summary(testData.DSev)

mydata1.Fire = mydata1[mydata1$Fire.Sev != 0, ]
trainIndex.FSev = createDataPartition(mydata1.Fire$Fire.Sev, p = 0.7, list = FALSE)
trainData.FSev = mydata1.Fire[trainIndex.FSev, ]
testData.FSev = mydata1.Fire[-trainIndex.FSev, ]
summary(trainData.FSev)
summary(testData.FSev)

mydata1.Other = mydata1[mydata1$Other.Sev != 0, ]
trainIndex.OSev = createDataPartition(mydata1.Other$Other.Sev, p = 0.7, list = FALSE)
trainData.OSev = mydata1.Other[trainIndex.OSev, ]
testData.OSev = mydata1.Other[-trainIndex.OSev, ]
summary(testData.OSev)

mydata1.Theft = mydata1[mydata1$Theft.Sev != 0, ]
trainIndex.TSev = createDataPartition(mydata1.Theft$Theft.Sev, p = 0.7, list = FALSE)
trainData.TSev = mydata1.Theft[trainIndex.TSev, ]
testData.TSev = mydata1.Theft[-trainIndex.TSev, ]
summary(testData.TSev)

mydata1.TPL = mydata1[mydata1$TPL.Sev != 0, ]
trainIndex.TPLSev = createDataPartition(mydata1.TPL$TPL.Sev, p = 0.7, list = FALSE)
trainData.TPLSev = mydata1.TPL[trainIndex.TPLSev, ]
testData.TPLSev = mydata1.TPL[-trainIndex.TPLSev, ]
summary(testData.TPLSev)

mydata1.Windscreen = mydata1[mydata1$Windscreen.Sev != 0, ]
trainIndex.WSev = createDataPartition(mydata1.Windscreen$Windscreen.Sev, p = 0.7, list = FALSE)
trainData.WSev = mydata1.Windscreen[trainIndex.WSev, ]
testData.WSev = mydata1.Windscreen[-trainIndex.WSev, ]
summary(testData.WSev)

mydata1.Total = mydata1[mydata1$Total.Sev != 0, ]
trainIndex.TotalSev = createDataPartition(mydata1.Total$Total.Sev, p = 0.7, list = FALSE)
trainData.TotalSev = mydata1.Total[trainIndex.TotalSev, ]
testData.TotalSev = mydata1.Total[-trainIndex.TotalSev, ]
summary(testData.TotalSev)

summary(trainData.DSev)
sum(trainData.DSev$Damage.Sev == 0)
summary(testData.DSev)
sum(testData.DSev$Damage.Sev == 0)

# Model Gamma ------------------------------------------------------

# Model jaminan Damage
mDamage.Y1 = glm(Damage.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                   VehPower + VehGas + Garage + Region, data = trainData.DSev, 
                 family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mDamage.Y1)

summary(trainData.DSev$Damage.Sev)
summary(trainData.OSev$Other.Sev)

train.Y1 = predict(mDamage.Y1, newdata = trainData.DSev, type = "response", interval = "confidence")
plot(mDamage.Y1$residuals)
test.Y1 = predict(mDamage.Y1, newdata = testData.DSev, type = "response", interval = "confidence")
summary(test.Y1)
RMSE(testData.DSev$Damage.Sev, test.Y1)

abs(mean(testData.DSev$Damage.Sev)-mean(test.Y1))
abs(median(testData.DSev$Damage.Sev)-median(test.Y1))
quantile(testData.DSev$Damage.Sev, c(0.025, 0.975))
quantile(test.Y1, c(0.025, 0.975))

boxplot(testData.DSev$Damage.Sev, test.Y1)
plot(testData.DSev$VehClass, test.Y1)

# Model jaminan Fire
#mFire.Y2 = glm(Fire.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehGas + 
#                  Garage, data = trainData.FSev, family = Gamma(link = "log"))

mFire.Y2 = glm(Fire.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                 VehPower + VehGas + Garage + Region, data = trainData.FSev, 
               family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
mFire.Y2 = glm(Fire.Sev ~ DrivAge + VehGas, data = trainData.FSev, family = Gamma(link = "log"))

summary(mFire.Y2)

summary(trainData.FSev)
summary(mydata1.Fire$VehClass)
summary(mydata1.Fire$Garage)

summary(trainData.FSev)
str(trainData.FSev)

test.Y2 = predict(mFire.Y2, newdata = testData.FSev, type = "response", interval = "confidence")
summary(test.Y2)
RMSE(testData.FSev$Fire.Sev, test.Y2)

abs(mean(testData.FSev$Fire.Sev)-mean(test.Y2))
abs(median(testData.FSev$Fire.Sev)-median(test.Y2))
quantile(testData.FSev$Fire.Sev, c(0.025, 0.975))
quantile(test.Y2, c(0.025, 0.975))

boxplot(testData.FSev$Fire.Sev, test.Y2)
plot(testData.FSev$DrivAge, test.Y2)
plot(testData.FSev$VehGas, test.Y2)

# error di bagian test.Y2 karena terdapat new levels professional run
# Fixing this with recipes ------------------------------------------------

# create a recipe that handles novel levels in VehUsage
#rec = recipe(Fire.Sev ~ ., data = trainData.FSev) %>%
#  step_other(VehUsage) %>% # This is where we handle new categories
#  prep()

#new_trainData.FSev = bake(rec, new_data = trainData.FSev)
#new_testData.FSev = bake(rec, new_data = testData.FSev)

#summary(trainData.FSev$VehUsage)
#summary(testData.FSev$VehUsage)
#summary(new_trainData.FSev$VehUsage)
#summary(new_testData.FSev$VehUsage)

# We build the model again with the prepared data
#mFire.Y2 = glm(Fire.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass + VehGas + 
#                 VehUsage + Garage + Area + Region, data = new_trainData.FSev, family = Gamma(link = "log"), maxit = 1000) %>%
#  stats::step(direction = "backward")
#summary(mFire.Y2)

#test.Y2 = predict(mFire.Y2, newdata = new_testData.FSev, type = "response", interval = "confidence")
#summary(test.Y2)
#RMSE(new_testData.FSev$Fire.Sev, test.Y2)

#abs(mean(new_testData.FSev$Fire.Sev)-mean(test.Y2))
#abs(median(new_testData.FSev$Fire.Sev)-median(test.Y2))
#quantile(new_testData.FSev$Fire.Sev, c(0.025, 0.975))
#quantile(test.Y2, c(0.025, 0.975))

#table(new_trainData.FSev$Fire.Sev)

# Model jaminan Other
mOther.Y3 = glm(Other.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.OSev, 
                family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mOther.Y3)

test.Y3 = predict(mOther.Y3, newdata = testData.OSev, type = "response", interval = "confidence")
summary(test.Y3)
RMSE(testData.OSev$Other.Sev, test.Y3)

abs(mean(testData.OSev$Other.Sev)-mean(test.Y3))
abs(median(testData.OSev$Other.Sev)-median(test.Y3))
quantile(testData.OSev$Other.Sev, c(0.025, 0.975))
quantile(test.Y3, c(0.025, 0.975))

boxplot(testData.OSev$Other.Sev, test.Y3)
plot(testData.OSev$VehAge, test.Y3)

# create a recipe that handles novel levels in PayFreq
#rec = recipe(Other.Sev ~ ., data = trainData.OSev) %>%
#  step_other(PayFreq, threshold = 15) %>% # This is where we handle new categories
#  prep()

#new_trainData.OSev = bake(rec, new_data = trainData.OSev)
#new_testData.OSev = bake(rec, new_data = testData.OSev)

#summary(trainData.OSev$PayFreq)
#summary(testData.OSev$PayFreq)
#summary(new_trainData.OSev$PayFreq)
#summary(new_testData.OSev$PayFreq)

# We build the model again with the prepared data
#mOther.Y3 = glm(Other.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass + VehGas + 
#                 VehUsage + Garage + Area + Region, data = new_trainData.OSev, family = Gamma(link = "log")) %>%
#  stats::step(direction = "backward")
#summary(mOther.Y3)

#test.Y3 = predict(mOther.Y3, newdata = new_testData.OSev, type = "response", interval = "confidence")
#summary(test.Y3)
#RMSE(new_testData.OSev$Other.Sev, test.Y3)

#abs(mean(new_testData.OSev$Other.Sev)-mean(test.Y3))
#abs(median(new_testData.OSev$Other.Sev)-median(test.Y3))
#quantile(new_testData.OSev$Other.Sev, c(0.025, 0.975))
#quantile(test.Y3, c(0.025, 0.975))


# Model jaminan Theft

mTheft.Y4 = glm(Theft.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.TSev, 
                family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTheft.Y4)

test.Y4 = predict(mTheft.Y4, newdata = testData.TSev, type = "response", interval = "confidence")
summary(test.Y4)
RMSE(testData.TSev$Theft.Sev, test.Y4)

abs(mean(testData.TSev$Theft.Sev)-mean(test.Y4))
abs(median(testData.TSev$Theft.Sev)-median(test.Y4))
quantile(testData.TSev$Theft.Sev, c(0.025, 0.975))
quantile(test.Y4, c(0.025, 0.975))

boxplot(testData.TSev$Theft.Sev, test.Y4)
boxplot(testData.TSev$Theft.Sev, test.Y4, ylim = c(0, 6000))
plot(testData.TSev$VehPower, test.Y4)

# Model jaminan TPL
mTPL.Y5 = glm(TPL.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                VehPower + VehGas + Garage + Region, data = trainData.TPLSev, 
              family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTPL.Y5)

test.Y5 = predict(mTPL.Y5, newdata = testData.TPLSev, type = "response", interval = "confidence")
summary(test.Y5)
RMSE(testData.TPLSev$TPL.Sev, test.Y5)

abs(mean(testData.TPLSev$TPL.Sev)-mean(test.Y5))
abs(median(testData.TPLSev$TPL.Sev)-median(test.Y5))
quantile(testData.TPLSev$TPL.Sev, c(0.025, 0.975))
quantile(test.Y5, c(0.025, 0.975))

boxplot(testData.TPLSev$TPL.Sev, test.Y5)
boxplot(testData.TPLSev$TPL.Sev, test.Y5, ylim = c(0, 10000))
plot(testData.TPLSev$BonusMalus, test.Y5)

# Model jaminan Windscreen
mWindscreen.Y6 = glm(Windscreen.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + 
                       VehAge + VehClass + VehPower + VehGas + Garage + Region,
                     data = trainData.WSev, family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mWindscreen.Y6)

test.Y6 = predict(mWindscreen.Y6, newdata = testData.WSev, type = "response", interval = "confidence")
summary(test.Y6)
RMSE(testData.WSev$Windscreen.Sev, test.Y6)

abs(mean(testData.WSev$Windscreen.Sev)-mean(test.Y6))
abs(median(testData.WSev$Windscreen.Sev)-median(test.Y6))
quantile(testData.WSev$Windscreen.Sev, c(0.025, 0.975))
quantile(test.Y6, c(0.025, 0.975))

boxplot(testData.WSev$Windscreen.Sev, test.Y6)
plot(testData.WSev$VehPower, test.Y6)

# Model semua jaminan YT
mTotal.YT = glm(Total.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
                  VehPower + VehGas + Garage + Region, data = trainData.TotalSev, 
                family = Gamma(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTotal.YT)

test.YT = predict(mTotal.YT, newdata = testData.TotalSev, type = "response", interval = "confidence")
train.YT = predict(mTotal.YT, newdata = trainData.TotalSev, type = "response", interval = "confidence")
RMSE(test.YT, testData.TotalSev$Total.Sev)
RMSE(train.YT, trainData.TotalSev$Total.Sev)

summary(test.YT)
RMSE(testData.TotalSev$Total.Sev, test.YT)

abs(mean(testData.TotalSev$Total.Sev)-mean(test.YT))
abs(median(testData.TotalSev$Total.Sev)-median(test.YT))
quantile(testData.TotalSev$Total.Sev, c(0.025, 0.975))
quantile(test.YT, c(0.025, 0.975))

boxplot(testData.TotalSev$Total.Sev, test.YT)
boxplot(testData.TotalSev$Total.Sev, test.YT, ylim = c(0, 5000))
plot(testData.TotalSev$VehAge, test.YT)

############################################################################

# Model X1, X2, ..., XT
test.Y1.new = predict(mDamage.Y1, newdata = testData.DX, type = "response", interval = "confidence")
test.Y2.new = predict(mFire.Y2, newdata = testData.FX, type = "response", interval = "confidence")
test.Y3.new = predict(mOther.Y3, newdata = testData.OX, type = "response", interval = "confidence")
test.Y4.new = predict(mTheft.Y4, newdata = testData.TX, type = "response", interval = "confidence")
test.Y5.new = predict(mTPL.Y5, newdata = testData.TPLX, type = "response", interval = "confidence")
test.Y6.new = predict(mWindscreen.Y6, newdata = testData.WX, type = "response", interval = "confidence")
test.YT.new = predict(mTotal.YT, newdata = testData.TotalX, type = "response", interval = "confidence")

yhat.X1 = test.N1*test.Y1.new
yhat.X2 = test.N2*test.Y2.new
yhat.X3 = test.N3*test.Y3.new
yhat.X4 = test.N4*test.Y4.new
yhat.X5 = test.N5*test.Y5.new
yhat.X6 = test.N6*test.Y6.new
yhat.XT = test.NT*test.YT.new

RMSE(testData.DX$Damage.y, yhat.X1)
RMSE(testData.FX$Fire.y, yhat.X2)
RMSE(testData.OX$Other.y, yhat.X3)
RMSE(testData.TX$Theft.y, yhat.X4)
RMSE(testData.TPLX$TPL.y, yhat.X5)
RMSE(testData.WX$Windscreen.y, yhat.X6)
RMSE(testData.TotalX$Total.y, yhat.XT)

plot(testData.DX$Damage.y, yhat.X1)
plot(testData.FX$Fire.y, yhat.X2)
plot(testData.OX$Other.y, yhat.X3)
plot(testData.TX$Theft.y, yhat.X4)
plot(testData.TPLX$TPL.y, yhat.X5)
plot(testData.WX$Windscreen.y, yhat.X6)
plot(testData.TotalX$Total.y, yhat.XT)


# Model Normal ------------------------------------------------------

# Model jaminan Damage
mDamage.Y1.Normal = glm(Damage.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq +
                          VehAge + VehClass + VehPower + VehGas + Garage + Region, 
                        data = trainData.DSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mDamage.Y1.Normal)

test.Y1.Normal = predict(mDamage.Y1.Normal, newdata = testData.DSev, type = "response", interval = "confidence")
summary(test.Y1.Normal)
RMSE(testData.DSev$Damage.Sev, test.Y1.Normal)    #RMSE menggunakan library(caret)

abs(mean(testData.DSev$Damage.Sev)-mean(test.Y1.Normal))
abs(median(testData.DSev$Damage.Sev)-median(test.Y1.Normal))
quantile(testData.DSev$Damage.Sev, c(0.025, 0.975))
quantile(test.Y1.Normal, c(0.025, 0.975))

boxplot(testData.DSev$Damage.Sev, test.Y1.Normal)
boxplot(testData.DSev$Damage.Sev, test.Y1.Normal, ylim = c(0, 5000))

# Model jaminan Fire
mFire.Y2.Normal = glm(Fire.Sev ~ DrivAge + DrivGender + BonusMalus +
                        PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                        Region, 
                      data = trainData.FSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

mFire.Y2.Normal = glm(Fire.Sev ~ DrivAge + VehAge + VehClass + VehGas, 
                      data = trainData.FSev, family = gaussian(link = "log"))
summary(mFire.Y2.Normal)


summary(mydata1)
summary(trainData.FX)
summary(testData.FX)

test.Y2.Normal = predict(mFire.Y2.Normal, newdata = testData.FSev, type = "response", interval = "confidence")
summary(test.Y2.Normal)
RMSE(testData.FSev$Fire.Sev, test.Y2.Normal)

abs(mean(testData.FSev$Fire.Sev)-mean(test.Y2.Normal))
abs(median(testData.FSev$Fire.Sev)-median(test.Y2.Normal))
quantile(testData.FSev$Fire.Sev, c(0.025, 0.975))
quantile(test.Y2.Normal, c(0.025, 0.975))

boxplot(testData.FSev$Fire.Sev, test.Y2.Normal)
plot(testData.FSev$DrivAge, test.Y2.Normal)


# Model jaminan Other
mOther.Y3.Normal = glm(Other.Sev ~ DrivAge + DrivGender + BonusMalus + 
                         PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                         Region, 
                       data = trainData.OSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")
mOther.Y3.Normal = glm(Other.Sev ~ DrivAge + BonusMalus + PayFreq + VehClass + VehPower + 
                         VehGas + Garage, 
                       data = trainData.OSev, family = gaussian(link = "log"))

mOther.Y3.Normal = glm(Other.Sev ~ DrivAge + DrivGender + BonusMalus + 
                              VehAge + VehClass + VehPower + VehGas + Garage + 
                              Region, 
                            data = trainData.OSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

summary(mOther.Y3.Normal)


summary(trainData.OSev)

test.Y3.Normal = predict(mOther.Y3.Normal, newdata = testData.OSev, type = "response", interval = "confidence")
summary(test.Y3.Normal)
RMSE(testData.OSev$Other.Sev, test.Y3.Normal)

abs(mean(testData.OSev$Other.Sev)-mean(test.Y3.Normal))
abs(median(testData.OSev$Other.Sev)-median(test.Y3.Normal))
quantile(testData.OSev$Other.Sev, c(0.025, 0.975))
quantile(test.Y3.Normal, c(0.025, 0.975))

boxplot(testData.OSev$Other.Sev, test.Y3.Normal)

# Model jaminan Theft
mTheft.Y4.Normal = glm(Theft.Sev ~ DrivAge + DrivGender + BonusMalus + 
                         PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                         Region, 
                       data = trainData.TSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTheft.Y4.Normal)


summary(trainData.TSev)
summary(testData.TSev)

test.Y4.Normal = predict(mTheft.Y4.Normal, newdata = testData.TSev, type = "response", interval = "confidence")
summary(test.Y4.Normal)
RMSE(testData.TSev$Theft.Sev, test.Y4.Normal)

abs(mean(testData.TSev$Theft.Sev)-mean(test.Y4.Normal))
abs(median(testData.TSev$Theft.Sev)-median(test.Y4.Normal))
quantile(testData.TSev$Theft.Sev, c(0.025, 0.975))
quantile(test.Y4.Normal, c(0.025, 0.975))

boxplot(testData.TSev$Theft.Sev, test.Y4.Normal)

# Model jaminan TPL
mTPL.Y5.Normal = glm(TPL.Sev ~ DrivAge + DrivGender + BonusMalus + 
                       PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                       Region, 
                     data = trainData.TPLSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

mTPL.Y5.Normal = glm(TPL.Sev ~ DrivAge + PayFreq + VehAge + VehPower + VehGas + Garage + 
                       Region, 
                     data = trainData.TPLSev, family = gaussian(link = "log"))

mTPL.Y5.Normal = glm(TPL.Sev ~ DrivAge + DrivGender + BonusMalus + 
                            PayFreq + VehAge + VehClass + VehGas + Garage +
                            Region, 
                          data = trainData.TPLSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

summary(mTPL.Y5.Normal)

summary(mydata1)
summary(trainData.TPLSev)
summary(testData.TPLSev)

test.Y5.Normal = predict(mTPL.Y5.Normal, newdata = testData.TPLSev, type = "response", interval = "confidence")
summary(test.Y5.Normal)
RMSE(testData.TPLSev$TPL.Sev, test.Y5.Normal)

abs(mean(testData.TPLSev$TPL.Sev)-mean(test.Y5.Normal))
abs(median(testData.TPLSev$TPL.Sev)-median(test.Y5.Normal))
quantile(testData.TPLSev$TPL.Sev, c(0.025, 0.975))
quantile(test.Y5.Normal, c(0.025, 0.975))

boxplot(testData.TPLSev$TPL.Sev, test.Y5.Normal)

# Model jaminan Windscreen
mWindscreen.Y6.Normal = glm(Windscreen.Sev ~ DrivAge + DrivGender + BonusMalus +
                              PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                              Region, 
                            data = trainData.WSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

mWindscreen.Y6.Normal = glm(Windscreen.Sev ~ DrivGender + BonusMalus + PayFreq + VehAge + 
                              VehClass + VehPower + VehGas + Garage, 
                            data = trainData.WSev, family = gaussian(link = "log"))

mWindscreen.Y6.Normal = glm(Windscreen.Sev ~ DrivAge + DrivGender + BonusMalus +
                                   VehAge + VehClass + VehPower + VehGas + Garage + 
                                   Region, 
                                 data = trainData.WSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")

summary(mWindscreen.Y6.Normal)

summary(mydata1)
summary(trainData.WSev)

test.Y6.Normal = predict(mWindscreen.Y6.Normal, newdata = testData.WSev, type = "response", interval = "confidence")
summary(test.Y6.Normal)
RMSE(testData.WSev$Windscreen.Sev, test.Y6.Normal)

abs(mean(testData.WSev$Windscreen.Sev)-mean(test.Y6.Normal))
abs(median(testData.WSev$Windscreen.Sev)-median(test.Y6.Normal))
quantile(testData.WSev$Windscreen.Sev, c(0.025, 0.975))
quantile(test.Y6.Normal, c(0.025, 0.975))

boxplot(testData.WSev$Windscreen.Sev, test.Y6.Normal)

# Model semua jaminan YT
mTotal.YT.Normal = glm(Total.Sev ~ DrivAge + DrivGender + BonusMalus +
                         PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
                         Region, 
                       data = trainData.TotalSev, family = gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTotal.YT.Normal)

test.YT.Normal = predict(mTotal.YT.Normal, newdata = testData.TotalSev, type = "response", interval = "confidence")
summary(test.YT.Normal)
RMSE(testData.TotalSev$Total.Sev, test.YT.Normal)

abs(mean(testData.TotalSev$Total.Sev)-mean(test.YT.Normal))
abs(median(testData.TotalSev$Total.Sev)-median(test.YT.Normal))
quantile(testData.TotalSev$Total.Sev, c(0.025, 0.975))
quantile(test.YT.Normal, c(0.025, 0.975))

boxplot(testData.TotalSev$Total.Sev, test.YT.Normal)
boxplot(testData.TotalSev$Total.Sev, test.YT.Normal, ylim = c(0, 3000))

############################################################################

# Model X1, X2, ..., XT
test.Y1.Normal.new = predict(mDamage.Y1.Normal, newdata = testData.DX, type = "response", interval = "confidence")
test.Y2.Normal.new = predict(mFire.Y2.Normal, newdata = testData.FX, type = "response", interval = "confidence")
test.Y3.Normal.new = predict(mOther.Y3.Normal, newdata = testData.OX, type = "response", interval = "confidence")
test.Y4.Normal.new = predict(mTheft.Y4.Normal, newdata = testData.TX, type = "response", interval = "confidence")
test.Y5.Normal.new = predict(mTPL.Y5.Normal, newdata = testData.TPLX, type = "response", interval = "confidence")
test.Y6.Normal.new = predict(mWindscreen.Y6.Normal, newdata = testData.WX, type = "response", interval = "confidence")
test.YT.Normal.new = predict(mTotal.YT.Normal, newdata = testData.TotalX, type = "response", interval = "confidence")

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

RMSE(testData.DX$Damage.y, yhat.X1.PoiNorm)
RMSE(testData.FX$Fire.y, yhat.X2.PoiNorm)
RMSE(testData.OX$Other.y, yhat.X3.PoiNorm)
RMSE(testData.TX$Theft.y, yhat.X4.PoiNorm)
RMSE(testData.TPLX$TPL.y, yhat.X5.PoiNorm)
RMSE(testData.WX$Windscreen.y, yhat.X6.PoiNorm)
RMSE(testData.TotalX$Total.y, yhat.XT.PoiNorm)

plot(testData.DX$Damage.y, yhat.X1.PoiNorm)
plot(testData.FX$Fire.y, yhat.X2.PoiNorm)
plot(testData.OX$Other.y, yhat.X3.PoiNorm)
plot(testData.TX$Theft.y, yhat.X4.PoiNorm)
plot(testData.TPLX$TPL.y, yhat.X5.PoiNorm)
plot(testData.WX$Windscreen.y, yhat.X6.PoiNorm)
plot(testData.TotalX$Total.y, yhat.XT.PoiNorm)


# Model Inverse Gaussian ------------------------------------------------------

# Model jaminan Damage

#mDamage.Y1.InvGaussian = glm(Damage.Sev ~ DrivAge + DrivGender + BonusMalus + LicenceNb + 
#                               PayFreq + VehAge + VehClass + VehPower + VehGas + Garage + 
#                               Area + Region + Channel + Marketing, 
#                             data = trainData.DSev, family = inverse.gaussian(link = "log")) 

#mDamage.Y1.InvGaussian = glm(Damage.Sev ~ DrivAge + Area + Region + BonusMalus + Garage
#                             , data = trainData.DSev, family = inverse.gaussian(link = "log")) 

#install.packages("writexl")
#library(writexl)
#write_xlsx(trainData.DSev, path = "data_trainData.DSev.xlsx")

mDamage.Y1.InvGaussian = glm(Damage.Sev ~ DrivAge + BonusMalus + PayFreq + 
                               VehPower + VehGas + Garage, 
                             data = trainData.DSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")

mDamage.Y1.InvGaussian = glm(Damage.Sev ~ 1
                             , data = trainData.DSev, family = inverse.gaussian(link = "log")) 
summary(mDamage.Y1.InvGaussian)

#summary(trainData.DSev$DrivGender)
#summary(trainData.DSev)

test.Y1.InvGaussian = predict(mDamage.Y1.InvGaussian, newdata = testData.DSev, type = "response", interval = "confidence")
summary(test.Y1.InvGaussian)
RMSE(testData.DSev$Damage.Sev, test.Y1.InvGaussian)

abs(mean(testData.DSev$Damage.Sev)-mean(test.Y1.InvGaussian))
abs(median(testData.DSev$Damage.Sev)-median(test.Y1.InvGaussian))
quantile(testData.DSev$Damage.Sev, c(0.025, 0.975))
quantile(test.Y1.InvGaussian, c(0.025, 0.975))

boxplot(testData.DSev$Damage.Sev, test.Y1.InvGaussian)

# Model jaminan Fire
#mFire.Y2 = glm(Fire.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehGas + 
#                  Garage, data = trainData.FSev, family = Gamma(link = "log"))

mFire.Y2.InvGaussian = glm(Fire.Sev ~ VehPower + DrivGender + BonusMalus + VehClass, 
                           data = trainData.FSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
mFire.Y2.InvGaussian = glm(Fire.Sev ~ 1, 
                           data = trainData.FSev, family = inverse.gaussian(link = "log"))
summary(mFire.Y2.InvGaussian)

#summary(trainData.FSev)
#summary(mydata1.Fire$VehClass)
#summary(mydata1.Fire$Garage)

#summary(trainData.FSev)
#str(trainData.FSev)

test.Y2.InvGaussian = predict(mFire.Y2.InvGaussian, newdata = testData.FSev, type = "response", interval = "confidence")
summary(test.Y2.InvGaussian)
RMSE(testData.FSev$Fire.Sev, test.Y2.InvGaussian)

abs(mean(testData.FSev$Fire.Sev)-mean(test.Y2.InvGaussian))
abs(median(testData.FSev$Fire.Sev)-median(test.Y2.InvGaussian))
quantile(testData.FSev$Fire.Sev, c(0.025, 0.975))
quantile(test.Y2.InvGaussian, c(0.025, 0.975))

boxplot(testData.FSev$Fire.Sev, test.Y2.InvGaussian)

# Model jaminan Other
mOther.Y3.InvGaussian = glm(Other.Sev ~ VehClass + VehAge + Region, 
                            data = trainData.OSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
mOther.Y3.InvGaussian = glm(Other.Sev ~ VehClass + VehAge, data = trainData.OSev, 
                            family = inverse.gaussian(link = "log"))
summary(mOther.Y3.InvGaussian)

test.Y3.InvGaussian = predict(mOther.Y3.InvGaussian, newdata = testData.OSev, type = "response", interval = "confidence")
summary(test.Y3.InvGaussian)
RMSE(testData.OSev$Other.Sev, test.Y3.InvGaussian)

abs(mean(testData.OSev$Other.Sev)-mean(test.Y3.InvGaussian))
abs(median(testData.OSev$Other.Sev)-median(test.Y3.InvGaussian))
quantile(testData.OSev$Other.Sev, c(0.025, 0.975))
quantile(test.Y3.InvGaussian, c(0.025, 0.975))

boxplot(testData.OSev$Other.Sev, test.Y3.InvGaussian)

# Model jaminan Theft
mTheft.Y4.InvGaussian = glm(Theft.Sev ~ VehClass, data = trainData.TSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTheft.Y4.InvGaussian)

test.Y4.InvGaussian = predict(mTheft.Y4.InvGaussian, newdata = testData.TSev, type = "response", interval = "confidence")
summary(test.Y4.InvGaussian)
RMSE(testData.TSev$Theft.Sev, test.Y4.InvGaussian)

abs(mean(testData.TSev$Theft.Sev)-mean(test.Y4.InvGaussian))
abs(median(testData.TSev$Theft.Sev)-median(test.Y4.InvGaussian))
quantile(testData.TSev$Theft.Sev, c(0.025, 0.975))
quantile(test.Y4.InvGaussian, c(0.025, 0.975))

boxplot(testData.TSev$Theft.Sev, test.Y4.InvGaussian)
boxplot(testData.TSev$Theft.Sev, test.Y4.InvGaussian, ylim = c(0, 3000))

# Model jaminan TPL
mTPL.Y5.InvGaussian = glm(TPL.Sev ~ Garage + DrivAge + DrivGender + PayFreq + VehAge + VehGas, 
                          data = trainData.TPLSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
mTPL.Y5.InvGaussian = glm(TPL.Sev ~ DrivGender + PayFreq + VehAge + VehGas, 
                          data = trainData.TPLSev, family = inverse.gaussian(link = "log"))

summary(mTPL.Y5.InvGaussian)

test.Y5.InvGaussian = predict(mTPL.Y5.InvGaussian, newdata = testData.TPLSev, type = "response", interval = "confidence")
summary(test.Y5.InvGaussian)
RMSE(testData.TPLSev$TPL.Sev, test.Y5.InvGaussian)

abs(mean(testData.TPLSev$TPL.Sev)-mean(test.Y5.InvGaussian))
abs(median(testData.TPLSev$TPL.Sev)-median(test.Y5.InvGaussian))
quantile(testData.TPLSev$TPL.Sev, c(0.025, 0.975))
quantile(test.Y5.InvGaussian, c(0.025, 0.975))

boxplot(testData.TPLSev$TPL.Sev, test.Y5.InvGaussian)
boxplot(testData.TPLSev$TPL.Sev, test.Y5.InvGaussian, ylim = c(0, 3000))

# Model jaminan Windscreen
mWindscreen.Y6.InvGaussian = glm(Windscreen.Sev ~ DrivAge + DrivGender + BonusMalus + PayFreq + 
                                   VehAge + VehClass + VehPower + VehGas + Garage + Region, 
                                 data = trainData.WSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mWindscreen.Y6.InvGaussian)

test.Y6.InvGaussian = predict(mWindscreen.Y6.InvGaussian, newdata = testData.WSev, type = "response", interval = "confidence")
summary(test.Y6.InvGaussian)
RMSE(testData.WSev$Windscreen.Sev, test.Y6.InvGaussian)

abs(mean(testData.WSev$Windscreen.Sev)-mean(test.Y6.InvGaussian))
abs(median(testData.WSev$Windscreen.Sev)-median(test.Y6.InvGaussian))
quantile(testData.WSev$Windscreen.Sev, c(0.025, 0.975))
quantile(test.Y6.InvGaussian, c(0.025, 0.975))

boxplot(testData.WSev$Windscreen.Sev, test.Y6.InvGaussian)

# Model semua jaminan YT
mTotal.YT.InvGaussian = glm(Total.Sev ~ VehClass + DrivAge + DrivGender + BonusMalus + PayFreq +
                              VehPower + VehGas + Garage + Region, 
                            data = trainData.TotalSev, family = inverse.gaussian(link = "log")) %>%
  stats::step(direction = "backward")
summary(mTotal.YT.InvGaussian)

test.YT.InvGaussian = predict(mTotal.YT.InvGaussian, newdata = testData.TotalSev, type = "response", interval = "confidence")
summary(test.YT.InvGaussian)
RMSE(testData.TotalSev$Total.Sev, test.YT.InvGaussian)

abs(mean(testData.TotalSev$Total.Sev)-mean(test.YT.InvGaussian))
abs(median(testData.TotalSev$Total.Sev)-median(test.YT.InvGaussian))
quantile(testData.TotalSev$Total.Sev, c(0.025, 0.975))
quantile(test.YT.InvGaussian, c(0.025, 0.975))

boxplot(testData.TotalSev$Total.Sev, test.YT.InvGaussian)
boxplot(testData.TotalSev$Total.Sev, test.YT.InvGaussian, ylim = c(0, 3000))

############################################################################

# Model X1, X2, ..., XT
test.Y1.InvGaussian.new = predict(mDamage.Y1.InvGaussian, newdata = testData.DX, type = "response", interval = "confidence")
test.Y2.InvGaussian.new = predict(mFire.Y2.InvGaussian, newdata = testData.FX, type = "response", interval = "confidence")
test.Y3.InvGaussian.new = predict(mOther.Y3.InvGaussian, newdata = testData.OX, type = "response", interval = "confidence")
test.Y4.InvGaussian.new = predict(mTheft.Y4.InvGaussian, newdata = testData.TX, type = "response", interval = "confidence")
test.Y5.InvGaussian.new = predict(mTPL.Y5.InvGaussian, newdata = testData.TPLX, type = "response", interval = "confidence")
test.Y6.InvGaussian.new = predict(mWindscreen.Y6.InvGaussian, newdata = testData.WX, type = "response", interval = "confidence")
test.YT.InvGaussian.new = predict(mTotal.YT.InvGaussian, newdata = testData.TotalX, type = "response", interval = "confidence")

yhat.X1.PoiInvG = test.N1*test.Y1.InvGaussian.new
yhat.X2.PoiInvG = test.N2*test.Y2.InvGaussian.new
yhat.X3.PoiInvG = test.N3*test.Y3.InvGaussian.new
yhat.X4.PoiInvG = test.N4*test.Y4.InvGaussian.new
yhat.X5.PoiInvG = test.N5*test.Y5.InvGaussian.new
yhat.X6.PoiInvG = test.N6*test.Y6.InvGaussian.new
yhat.XT.PoiInvG = test.NT*test.YT.InvGaussian.new

RMSE(testData.DX$Damage.y, yhat.X1.PoiInvG)
RMSE(testData.FX$Fire.y, yhat.X2.PoiInvG)
RMSE(testData.OX$Other.y, yhat.X3.PoiInvG)
RMSE(testData.TX$Theft.y, yhat.X4.PoiInvG)
RMSE(testData.TPLX$TPL.y, yhat.X5.PoiInvG)
RMSE(testData.WX$Windscreen.y, yhat.X6.PoiInvG)
RMSE(testData.TotalX$Total.y, yhat.XT.PoiInvG)



#install.packages("MASS")
#library(MASS)

#model_damage = glm(Damage.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
#                        VehPower + VehGas + Garage + Region, data = trainData.DX, family=negative.binomial(theta=1))
#model_damage = glm.nb(Damage.x ~ DrivAge + DrivGender + BonusMalus + PayFreq + VehAge + VehClass +
#                     VehPower + VehGas + Garage + Region, data = trainData.DX)
#summary(model_damage)
