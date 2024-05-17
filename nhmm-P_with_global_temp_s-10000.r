library(NHMM)
library(readxl)
library(ggplot2)
library(patchwork)
library(readr)

# getwd()
setwd("~/WorkSpace/yanl/nhmm-world/P~temp_s")

DATA <- read.table("SSTA_SCALE_Pacific2_2023.1.csv",header = FALSE, sep = "," , stringsAsFactors = FALSE)
EXO <- read.table("EXO.csv",header = FALSE, sep = "," , stringsAsFactors = FALSE)

#smoothed monthly global temp  from 1880-1 to 2022-12
temp_s <- read_csv("temp_smoothed.csv")

EXO_3 <- cbind(EXO[301:2004,1:2], temp_s[1:1704,2])

# modify the time period of SSTA data, we just use the 1881.1-2022.12 SSTA data
SSTA = DATA[301:2004, ]

xsel1 <- data.matrix(EXO_3)
ysel1 <- data.matrix(SSTA)

# a sample of the NHMM model, run several times to find the best model
my.nhmm=NHMM(y=ysel1, X=t(xsel1), K=5, iters=10000, burnin=2000, emdist="gamma", 
             nmix=1, delta=TRUE)



zz=Oz(my.nhmm)
Trans_prob=OQQ(my.nhmm,FALSE)     #transition probability
pp=OWcoef(my.nhmm,FALSE)
tt=Oemparams(my.nhmm,FALSE)
EXO_coe = OXcoef(my.nhmm, plots = FALSE, outfile = NULL)
# [1] "The Markov property of the model is significant with 95% probability."
# [1] "The Markov property of the model is significant with 90% probability."
# [1] "Input1: X is significant with 95% probability"
# [1] "Input2: X is significant with 95% probability"
# [1] "Input3: X is significant with 95% probability"
# [1] "Input1: X is significant with 90% probability"
# [1] "Input2: X is significant with 90% probability"
# [1] "Input3: X is significant with 90% probability"

ind = c(2, 1, 3, 5, 4)
EXO_coe2 = EXO_coe
EXO_coe2[1,,]=EXO_coe[2,,]
EXO_coe2[2,,]=EXO_coe[1,,]
EXO_coe2[3,,]=EXO_coe[3,,]
EXO_coe2[4,,]=EXO_coe[4,,]  # Strong El Nino
EXO_coe2[,1,]=EXO_coe[,2,]
EXO_coe2[,2,]=EXO_coe[,1,]
EXO_coe2[,3,]=EXO_coe[,3,]
EXO_coe2[,4,]=EXO_coe[,5,]
EXO_coe2[,5,]=EXO_coe[,4,]
write.csv(EXO_coe2, file = "Oxcoef_P_temp_s_k5-10000.csv")

# AD test
OBIC(my.nhmm)
logl = my.nhmm$loglik
density_estimate <- density(logl[2001:10000])
plot(density_estimate, main="Probability Density Function", xlab="Value", ylab="Density")
qqnorm(logl[2001:10000])
qqline(logl[2001:10000])
ks.test(logl[2001:10000], "pnorm", mean(logl[2001:10000]), sd(logl[2001:10000]))
library(nortest)
ad.test(logl[2001:10000])



write.csv(zz, file = "zz_PAC_temp_s_k5-10000.csv")
write.csv(logl, file = "ll_PAC_temp_s_k5-10000.csv")
#write.csv(pp, file = "Owcoef_PAC_temp_k5-10000.csv")
#save.image(file = "PAC_temp_s_k5-10000-2.RData")

#######transition probabilities###########
#mean transition probabilities
ind = c(2,1,3,5,4)
VIT<-Trans_prob[,,1]
for (i in 1:5) {
  for (j in 1:5){
    x=Trans_prob[ind[i],ind[j],] 
    VIT[i,j]<-mean(x)
  }    
}
write.csv(VIT, file = "Transprob_PAC_k5.csv")


