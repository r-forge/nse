require(nse)
seed = 123
set.seed(seed)
#Random series
n = 1000
ar = c(0.5,0.6)
mean = c(1,5)
sd = c(10,2)

Ts1 = as.vector(arima.sim(n = n, list(ar = ar[1]), sd = sd[1]) + mean[1])
Ts2 = as.vector(arima.sim(n = n, list(ar = ar[2]), sd = sd[2]) + mean[2])

Ts = cbind(Ts1,Ts2)

#Geyer test
nbatch = 30
nse::nse.geyer(x = Ts1, nbatch = nbatch, type =  "bm")
nse::nse.geyer(x = Ts, nbatch = nbatch , type =  "bm")
nse::nse.geyer(x = Ts1, nbatch = nbatch , type =  "obm")

nse::nse.geyer(x = Ts1 , type = "iseq",iseq.type = "pos")
nse::nse.geyer(x = Ts1 , type = "iseq",iseq.type = "dec")
nse::nse.geyer(x = Ts1 , type = "iseq",iseq.type = "con")
nse::nse.geyer(x = Ts1, nbatch = nbatch, type = "iseq.bm",iseq.type = "pos")
nse::nse.geyer(x = Ts1, nbatch = nbatch, type = "iseq.bm",iseq.type = "dec")
nse::nse.geyer(x = Ts1, nbatch = nbatch, type = "iseq.bm",iseq.type = "con")

#Spc0
nse::nse.spec0(x = Ts1,method = "wosa")
nse::nse.spec0(x = Ts1,method = "AR")
nse::nse.spec0(x = Ts1,method = "bartlett")
nse::nse.spec0(x = Ts1,method = "tukey")


nse::nse.spec0(x = Ts1,method = "wosa", max.length = 200)
nse::nse.spec0(x = Ts1,method = "AR", max.length = 200)
nse::nse.spec0(x = Ts1,method = "bartlett", max.length = 1000)
nse::nse.spec0(x = Ts1,method = "tukey", max.length = 1000)


nse::nse.spec0(x = Ts1,method = "wosa",prewhite = TRUE)
nse::nse.spec0(x = Ts1,method = "AR",prewhite = TRUE)
nse::nse.spec0(x = Ts1,method = "bartlett",prewhite = TRUE)
nse::nse.spec0(x = Ts1,method = "tukey",prewhite = TRUE)


nse::nse.spec0(x = Ts1,method = "wosa",prewhite = TRUE, max.length = 1000)
nse::nse.spec0(x = Ts1,method = "AR",prewhite = TRUE, max.length = 1000)
nse::nse.spec0(x = Ts1,method = "bartlett",prewhite = TRUE, max.length = 1000)
nse::nse.spec0(x = Ts1,method = "tukey",prewhite = TRUE, max.length = 1000)


#NEWEY-WEST TEST
nse::nse.nw(x = Ts1)
nse::nse.nw(x = Ts)
nse::nse.nw(x = Ts1, prewhite = TRUE)
nse::nse.nw(x = Ts, prewhite = TRUE)

#ANDREWS BANDWIDTH TEST
nse::nse.andrews(x = Ts1, type = "Bartlett")
nse::nse.andrews(x = Ts, type = "Bartlett")
nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Bartlett")
nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Bartlett")

nse::nse.andrews(x = Ts1, type = "Parzen")
nse::nse.andrews(x = Ts, type = "Parzen")
nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Parzen")
nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Parzen")

nse::nse.andrews(x = Ts1, type = "Quadratic Spectral")
nse::nse.andrews(x = Ts, type = "Quadratic Spectral")
nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Quadratic Spectral")
nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Quadratic Spectral")

nse::nse.andrews(x = Ts1, type = "Truncated")
nse::nse.andrews(x = Ts, type = "Truncated")
nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Truncated")
nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Truncated")

nse::nse.andrews(x = Ts1, type = "Tukey-Hanning")
nse::nse.andrews(x = Ts, type = "Tukey-Hanning")
nse::nse.andrews(x = Ts1, prewhite = TRUE, type = "Tukey-Hanning")
nse::nse.andrews(x = Ts, prewhite = TRUE, type = "Tukey-Hanning")

#Hirukawa BANDWIDTH TEST

nse::nse.hiruk(x = Ts1, type = "Bartlett")
nse::nse.hiruk(x = Ts1, prewhite = TRUE, type = "Bartlett")

nse::nse.hiruk(x = Ts1, type = "Parzen")
nse::nse.hiruk(x = Ts1, prewhite = TRUE, type = "Parzen")

#Bootstrap TEST
nb = 100
nse::nse.boot(x = Ts1, nb =  nb, type = "stationary")
nse::nse.boot(x = Ts, nb =  nb, type = "stationary")

nse::nse.boot(x = Ts1, nb =  nb, type = "circular")
nse::nse.boot(x = Ts, nb =  nb, type = "circular")

