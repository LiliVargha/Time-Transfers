
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#######
####### Time transfers by age and gender in 28 countries
####### by Vargha, Lili; Binder-Hammer, Bernhard & Donehower, Gretchen
#######
####### Contact: Lili Vargha (lili.vargha@hu-berlin.de or vargha@demografia.hu)
#######

####### Original data source:
####### 1. European AGENTA Project (Vargha et al. 2016): http://dataexplorer.wittgensteincentre.org/nta/ for countries BE, BG, DE, DK, EE, ES, FI, FR, IT, LT, LV, NL, PL, SE, SI, UK
####### 2. Counting Women's Work (2022): https://www.countingwomenswork.org/data for countries US, ZA, SN, GH, UY, CO, VN, IN, MX, MU
####### 3. Hammer 2014 for country AT
####### 4. Gál, Szabó and Vargha 2017 for country HU

####### Data downloaded from the AGENTA and CWW website: October 2022

####### Last update: 2 March 2023

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")

####### Loading packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(plyr)
library(ggplot2)
library(cowplot)
library(data.table)
library(reshape)
library(patchwork)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####
##### reading the AGENTA data (Vargha et al. 2016)
#####

##### long database based on HETUS downloaded from http://dataexplorer.wittgensteincentre.org/nta/

agentahetus <- read.csv(file = 'HETUS NTTA_long_Time.csv')

##### selecting unpaid work production (PT) and consumption variables (CT)

agentahetus <- select (agentahetus,-c(DN, PIH, CIH, CHW, PHW, CCH, PCH))

##### long database based on MTUS downloaded from http://dataexplorer.wittgensteincentre.org/nta/

agentamtus <- read.csv(file = 'MTUS NTTA_long_Time.csv')

##### selecting unpaid work production (PT) and consumption variables (CT)

agentamtus <- select (agentamtus,-c(DN, CHW, PHW, CCH, PCH))

##### drop countries from MTUS agenta data that are found in HETUS data

agentamtus<-subset(agentamtus, country!="Germany" & year!="2001" | country!="Italy" & year!="2002" | country!="Spain" & year!="2003" | country!="United Kingdom" & year!="2001" )

##merge hetus and mtus data

agenta <- rbind(agentahetus, agentamtus)

str(agenta)

##### PT for age 0-10 is NA, we change that to 0

agenta[is.na(agenta)] = 0

#### calculating time transfers in hours per day

agenta$TT <- (agenta$CT - agenta$PT) / 60

#### selecting the most recent year for every country

agenta2 <- select (agenta,-c(X, PT, CT))

agenta3 <- agenta2 %>% filter (
  
  country == "Belgium" & year == '2005' | country == "Bulgaria" & year == '2002' | country == "Germany" & year == '2002' | country == "Estonia" & year == '2000'
  | country == "Finland" & year == '2000' | country == "France" & year == '1999' | country == "Italy" & year == '2003' | country == "Lithuania" & year == '2003'
  | country == "Latvia" & year == '2003' | country == "Poland" & year == '2004' | country == "Sweden" & year == '2001' | country == "Slovenia" & year == '2001'
  | country =="United Kingdom" & year =="2005" | country =="Denmark" & year =="2001" | country =="Netherlands" & year =="2005" | country =="Spain" & year =="2010"
)

#### reshaping database separately for men (TT1) and women (TT2)

agentamen <- agenta3 %>% filter( sex == "male")
agentawomen <- agenta3 %>% filter( sex == "female")


#### creating wide database of age profiles for men (TT1)

agentamen <- select (agentamen,-c(year, sex))

agentamen <- reshape(agentamen, idvar = "country", timevar = "age", direction = "wide")



agentamen$country <- revalue(agentamen$country, c("Belgium"="TT1_BE", "Bulgaria"="TT1_BG", "Estonia"="TT1_EE", "Finland"="TT1_FI", "France"="TT1_FR", "Germany"="TT1_DE", "Italy"="TT1_IT",
                                                  "Latvia"="TT1_LV", "Lithuania"="TT1_LT", "Poland"="TT1_PL", "Slovenia"="TT1_SI", "Sweden"="TT1_SE", "United Kingdom"="TT1_UK", "Denmark"="TT1_DK",
                                                  "Netherlands"="TT1_NL", "Spain"="TT1_ES"))

#### transpose age profiles for men (TT1) and make sure they stay numeric values

rownames(agentamen) <- agentamen[,1 ]  
agentamen <- select (agentamen,-c(country))

agentamen <- as.data.frame(t(agentamen), stringsAsFactors = FALSE)

#### creating wide database of age profiles for women (TT2)

agentawomen <- select (agentawomen,-c(year, sex))

agentawomen <- reshape(agentawomen, idvar = "country", timevar = "age", direction = "wide")

agentawomen$country <- revalue(agentawomen$country, c("Belgium"="TT2_BE", "Bulgaria"="TT2_BG", "Estonia"="TT2_EE", "Finland"="TT2_FI", "France"="TT2_FR", "Germany"="TT2_DE", "Italy"="TT2_IT",
                                                      "Latvia"="TT2_LV", "Lithuania"="TT2_LT", "Poland"="TT2_PL", "Slovenia"="TT2_SI", "Sweden"="TT2_SE", "United Kingdom"="TT2_UK", "Denmark"="TT2_DK",
                                                      "Netherlands"="TT2_NL", "Spain"="TT2_ES"))

#### transpose age profiles for women (TT2) and make sure they stay numeric values

rownames(agentawomen) <- agentawomen[,1 ]  
agentawomen <- select (agentawomen,-c(country))

agentawomen <- as.data.frame(t(agentawomen), stringsAsFactors = FALSE)

str(agentawomen)

#### merge data for men and women age 0-80

agentafinal <- cbind(agentamen, agentawomen)

#### use data point 80+ for ages 81-85+

agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])

#### renaming row names
vec <- 0:85
row.names(agentafinal) <- vec

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####
##### reading data from Austria (Hammer 2014) and Hungary (Gal, Szabo & Vargha, 2017)
#####

ATHU <- as.data.frame(read_excel("Unpaidwork_HU_AT.xlsx"))

str(ATHU)


####### calculate time transfers by gender

ATHU$TT1_HU <- (ATHU$CT1_HU - ATHU$PT1_HU)/60
ATHU$TT2_HU <- (ATHU$CT2_HU - ATHU$PT2_HU)/60
ATHU$TT1_AT <- (ATHU$CT1_AT - ATHU$PT1_AT)/60
ATHU$TT2_AT <- (ATHU$CT2_AT - ATHU$PT2_AT)/60

####### merge with agentafinal dataframe

agentafinal <- cbind(agentafinal, ATHU)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####
##### reading the Counting Women's Work Data (Counting Women's Work 2022)
#####

cww <- as.data.frame(read_excel("cww_database_PUBLIC_RELEASE_V3.0.xlsx", sheet = "data", range = "B8:JY99"))

str(cww)
attach(cww)

####### creating total production and total consumption age profiles (hours per day) for each country
####### calculate value for age 85 if it is necessary
####### calculate time transfers by gender
####### plotting time transfers for the different countries for check-up

#US 2009
cww$PT1_US <- (hyc1tUS + hyh1tUS)/7
cww$PT2_US <- (hyc2tUS + hyh2tUS)/7
cww$CT1_US <- (hcc0tUS + hch0tUS)/7
cww$CT2_US <- (hcc0tUS + hch0tUS)/7

cww$TT1_US <- cww$CT1_US - cww$PT1_US
cww$TT2_US <- cww$CT2_US - cww$PT2_US

age2 <- age[1:86]
TT1_US <- cww$TT1_US[1:86]
TT2_US <- cww$TT2_US[1:86]

plot(age2,TT1_US, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10))
lines(age2, TT2_US, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in the US")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#South Africa 2010
cww$PT1_ZA <- (hyc1tZA + hyh1tZA)/7
cww$PT2_ZA <- (hyc2tZA + hyh2tZA)/7
cww$CT1_ZA <- (hcc0tZA + hch0tZA)/7
cww$CT2_ZA <- (hcc0tZA + hch0tZA)/7

plot(age,cww$PT1_ZA, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_ZA, col="red", lwd=3)
title("Unpaid work in ZA")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_ZA[86] <- (mean(cww$PT1_ZA[86:91]))
cww$PT2_ZA[86] <- (mean(cww$PT2_ZA[86:91]))
cww$CT1_ZA[86] <- (mean(cww$CT1_ZA[86:91]))
cww$CT2_ZA[86] <- (mean(cww$CT2_ZA[86:91]))

cww$TT1_ZA <- cww$CT1_ZA - cww$PT1_ZA
cww$TT2_ZA <- cww$CT2_ZA - cww$PT2_ZA

TT1_ZA <- cww$TT1_ZA[1:86]
TT2_ZA <- cww$TT2_ZA[1:86]

plot(age2,TT1_ZA, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10))
lines(age2, TT2_ZA, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in ZA")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Senegal 2011
cww$PT1_SN <- (hyc1tSN + hyh1tSN)/7
cww$PT2_SN <- (hyc2tSN + hyh2tSN)/7
cww$CT1_SN <- (hcc0tSN + hch0tSN)/7
cww$CT2_SN <- (hcc0tSN + hch0tSN)/7

plot(age,cww$PT1_SN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_SN, col="red", lwd=3)
title("Unpaid work in SN")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_SN[86] <- (mean(cww$PT1_SN[86:91]))
cww$PT2_SN[86] <- (mean(cww$PT2_SN[86:91]))
cww$CT1_SN[86] <- (mean(cww$CT1_SN[86:91]))
cww$CT2_SN[86] <- (mean(cww$CT2_SN[86:91]))

cww$TT1_SN <- cww$CT1_SN - cww$PT1_SN
cww$TT2_SN <- cww$CT2_SN - cww$PT2_SN

TT1_SN <- cww$TT1_SN[1:86]
TT2_SN <- cww$TT2_SN[1:86]

plot(age2,TT1_SN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_SN, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in SN")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Ghana 2009
cww$PT1_GH <- (hyc1tGH + hyh1tGH)/7
cww$PT2_GH <- (hyc2tGH + hyh2tGH)/7
cww$CT1_GH <- (hcc0tGH + hch0tGH)/7
cww$CT2_GH <- (hcc0tGH + hch0tGH)/7

plot(age,cww$PT1_GH, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_GH, col="red", lwd=3)
title("Unpaid work in GH")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_GH[86] <- (mean(cww$PT1_GH[86:91]))
cww$PT2_GH[86] <- (mean(cww$PT2_GH[86:91]))
cww$CT1_GH[86] <- (mean(cww$CT1_GH[86:91]))
cww$CT2_GH[86] <- (mean(cww$CT2_GH[86:91]))

cww$TT1_GH <- cww$CT1_GH - cww$PT1_GH
cww$TT2_GH <- cww$CT2_GH - cww$PT2_GH

TT1_GH <- cww$TT1_GH[1:86]
TT2_GH <- cww$TT2_GH[1:86]

plot(age2,TT1_GH, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_GH, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in GH")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Uruguay 2013
cww$PT1_UY <- (hyc1tUY + hyh1tUY)/7
cww$PT2_UY <- (hyc2tUY + hyh2tUY)/7
cww$CT1_UY <- (hcc0tUY + hch0tUY)/7
cww$CT2_UY <- (hcc0tUY + hch0tUY)/7

plot(age,cww$PT1_UY, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_UY, col="red", lwd=3)
title("Unpaid work in UY")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_UY[86] <- (mean(cww$PT1_UY[86:91]))
cww$PT2_UY[86] <- (mean(cww$PT2_UY[86:91]))
cww$CT1_UY[86] <- (mean(cww$CT1_UY[86:91]))
cww$CT2_UY[86] <- (mean(cww$CT2_UY[86:91]))

cww$TT1_UY <- cww$CT1_UY - cww$PT1_UY
cww$TT2_UY <- cww$CT2_UY - cww$PT2_UY

TT1_UY <- cww$TT1_UY[1:86]
TT2_UY <- cww$TT2_UY[1:86]

plot(age2,TT1_UY, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_UY, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in UY")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Colombia 2012
cww$PT1_CO <- (hyc1tCO + hyh1tCO)/7
cww$PT2_CO <- (hyc2tCO + hyh2tCO)/7
cww$CT1_CO <- (hcc0tCO + hch0tCO)/7
cww$CT2_CO <- (hcc0tCO + hch0tCO)/7

plot(age,cww$PT1_CO, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_CO, col="red", lwd=3)
title("Unpaid work in CO")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_CO[86] <- (mean(cww$PT1_CO[86:91]))
cww$PT2_CO[86] <- (mean(cww$PT2_CO[86:91]))
cww$CT1_CO[86] <- (mean(cww$CT1_CO[86:91]))
cww$CT2_CO[86] <- (mean(cww$CT2_CO[86:91]))

cww$TT1_CO <- cww$CT1_CO - cww$PT1_CO
cww$TT2_CO <- cww$CT2_CO - cww$PT2_CO

TT1_CO <- cww$TT1_CO[1:86]
TT2_CO <- cww$TT2_CO[1:86]

plot(age2,TT1_CO, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_CO, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in CO")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Vietnam 2015
cww$PT1_VN <- (hyc1tVN + hyh1tVN)/7
cww$PT2_VN <- (hyc2tVN + hyh2tVN)/7
cww$CT1_VN <- (hcc0tVN + hch0tVN)/7
cww$CT2_VN <- (hcc0tVN + hch0tVN)/7

plot(age,cww$PT1_VN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_VN, col="red", lwd=3)
title("Unpaid work in VN")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_VN[86] <- (mean(cww$PT1_VN[86:91]))
cww$PT2_VN[86] <- (mean(cww$PT2_VN[86:91]))
cww$CT1_VN[86] <- (mean(cww$CT1_VN[86:91]))
cww$CT2_VN[86] <- (mean(cww$CT2_VN[86:91]))

cww$TT1_VN <- cww$CT1_VN - cww$PT1_VN
cww$TT2_VN <- cww$CT2_VN - cww$PT2_VN

TT1_VN <- cww$TT1_VN[1:86]
TT2_VN <- cww$TT2_VN[1:86]

plot(age2,TT1_VN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_VN, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in VN")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#India 1999
cww$PT1_IN <- (hyc1tIN + hyh1tIN)/7
cww$PT2_IN <- (hyc2tIN + hyh2tIN)/7
cww$CT1_IN <- (hcc0tIN + hch0tIN)/7
cww$CT2_IN <- (hcc0tIN + hch0tIN)/7

plot(age,cww$PT1_IN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_IN, col="red", lwd=3)
title("Unpaid work in IN")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_IN[86] <- (mean(cww$PT1_IN[86:91]))
cww$PT2_IN[86] <- (mean(cww$PT2_IN[86:91]))
cww$CT1_IN[86] <- (mean(cww$CT1_IN[86:91]))
cww$CT2_IN[86] <- (mean(cww$CT2_IN[86:91]))

cww$TT1_IN <- cww$CT1_IN - cww$PT1_IN
cww$TT2_IN <- cww$CT2_IN - cww$PT2_IN

TT1_IN <- cww$TT1_IN[1:86]
TT2_IN <- cww$TT2_IN[1:86]

plot(age2,TT1_IN, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_IN, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in IN")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Mexico 2014
cww$PT1_MX <- (hyc1tMX + hyh1tMX)/7
cww$PT2_MX <- (hyc2tMX + hyh2tMX)/7
cww$CT1_MX <- (hcc0tMX + hch0tMX)/7
cww$CT2_MX <- (hcc0tMX + hch0tMX)/7

plot(age,cww$PT1_MX, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_MX, col="red", lwd=3)
title("Unpaid work in MX")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_MX[86] <- (mean(cww$PT1_MX[86:91]))
cww$PT2_MX[86] <- (mean(cww$PT2_MX[86:91]))
cww$CT1_MX[86] <- (mean(cww$CT1_MX[86:91]))
cww$CT2_MX[86] <- (mean(cww$CT2_MX[86:91]))

cww$TT1_MX <- cww$CT1_MX - cww$PT1_MX
cww$TT2_MX <- cww$CT2_MX - cww$PT2_MX

TT1_MX <- cww$TT1_MX[1:86]
TT2_MX <- cww$TT2_MX[1:86]

plot(age2,TT1_MX, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_MX, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in MX")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))

#Mauritius 2003
cww$PT1_MU <- (hyc1tMU + hyh1tMU)/7
cww$PT2_MU <- (hyc2tMU + hyh2tMU)/7
cww$CT1_MU <- (hcc0tMU + hch0tMU)/7
cww$CT2_MU <- (hcc0tMU + hch0tMU)/7

plot(age,cww$PT1_MU, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(0,10))
lines(age, cww$PT2_MU, col="red", lwd=3)
title("Unpaid work in MU")
legend(2,1,c("men","women"), lwd=c(3,3), col=c("blue","red"))

cww$PT1_MU[86] <- (mean(cww$PT1_MU[86:91]))
cww$PT2_MU[86] <- (mean(cww$PT2_MU[86:91]))
cww$CT1_MU[86] <- (mean(cww$CT1_MU[86:91]))
cww$CT2_MU[86] <- (mean(cww$CT2_MU[86:91]))

cww$TT1_MU <- cww$CT1_MU - cww$PT1_MU
cww$TT2_MU <- cww$CT2_MU - cww$PT2_MU

TT1_MU <- cww$TT1_MU[1:86]
TT2_MU <- cww$TT2_MU[1:86]

plot(age2,TT1_MU, type="l", col="blue", lwd=3, xlab="age", ylab="hours per day", ylim=c(-6,10), xlim=c(0,85))
lines(age2, TT2_MU, col="red", lwd=3)
abline(h = 0, col = "black")
title("Time transfers in MU")
legend(0,-4,c("men","women"), lwd=c(3,3), col=c("blue","red"))


detach(cww)

####### cut dataframe to age 0-85+ (please note the 80-85+ values were calculated above)

cww <- cww[-c(87:91), ]

####### merge with agentafinal dataframe

ntta <- cbind(agentafinal, cww)
str(ntta)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### creating seperate database for time transfers by women and men

TT1var <- c("TT1_US", "TT1_ZA", "TT1_SN", "TT1_GH", "TT1_UY", "TT1_CO", "TT1_VN",
            "TT1_IN", "TT1_MX", "TT1_MU", "TT1_HU", "TT1_AT", "TT1_BE", "TT1_BG",
            "TT1_DE", "TT1_DK", "TT1_EE", "TT1_ES", "TT1_FI", "TT1_FR", "TT1_IT",
            "TT1_LT", "TT1_LV", "TT1_NL", "TT1_PL", "TT1_SE", "TT1_SI", "TT1_UK", "age")

TT2var <- c("TT2_US", "TT2_ZA", "TT2_SN", "TT2_GH", "TT2_UY", "TT2_CO", "TT2_VN",
            "TT2_IN", "TT2_MX", "TT2_MU", "TT2_HU", "TT2_AT", "TT2_BE", "TT2_BG",
            "TT2_DE", "TT2_DK", "TT2_EE", "TT2_ES", "TT2_FI", "TT2_FR", "TT2_IT",
            "TT2_LT", "TT2_LV", "TT2_NL", "TT2_PL", "TT2_SE", "TT2_SI", "TT2_UK", "age")

nttaTT1 <- ntta[TT1var]

nttaTT2 <- ntta[TT2var]

colnames(nttaTT1)

names(nttaTT1)

####### using the name of the country and year for the visualization

nttaTT1c <- rename(nttaTT1, c("TT1_US"="United States 2009", "TT1_ZA"="South Africa 2010", "TT1_SN"="Senegal 2011", "TT1_GH"="Ghana 2009",
                              "TT1_UY"="Uruguay 2013", "TT1_CO"="Colombia 2012", "TT1_VN"="Vietnam 2015",
                              "TT1_IN"="India 1999", "TT1_MX"="Mexico 2014", "TT1_MU"="Mauritius 2003", "TT1_HU"="Hungary 2010", "TT1_AT"="Austria 2002",
                              "TT1_BE"="Belgium 2005", "TT1_BG"="Bulgaria 2002",
                              "TT1_DE"="Germany 2002", "TT1_DK"="Denmark 2001", "TT1_EE"="Estonia 2000", "TT1_ES"="Spain 2010",
                              "TT1_FI"="Finland 2000", "TT1_FR"="France 1999", "TT1_IT"="Italy 2003",
                              "TT1_LT"="Lithuania 2003", "TT1_LV"="Latvia 2003", "TT1_NL"="Netherlands 2005", "TT1_PL"="Poland 2004",
                              "TT1_SE"="Sweden 2001", "TT1_SI"="Slovenia 2001", "TT1_UK"="United Kingdom 2005" ))

nttaTT2c <- rename(nttaTT2, c("TT2_US"="United States 2009", "TT2_ZA"="South Africa 2010", "TT2_SN"="Senegal 2011", "TT2_GH"="Ghana 2009",
                              "TT2_UY"="Uruguay 2013", "TT2_CO"="Colombia 2012", "TT2_VN"="Vietnam 2015",
                              "TT2_IN"="India 1999", "TT2_MX"="Mexico 2014", "TT2_MU"="Mauritius 2003", "TT2_HU"="Hungary 2010", "TT2_AT"="Austria 2002",
                              "TT2_BE"="Belgium 2005", "TT2_BG"="Bulgaria 2002",
                              "TT2_DE"="Germany 2002", "TT2_DK"="Denmark 2001", "TT2_EE"="Estonia 2000", "TT2_ES"="Spain 2010",
                              "TT2_FI"="Finland 2000", "TT2_FR"="France 1999", "TT2_IT"="Italy 2003",
                              "TT2_LT"="Lithuania 2003", "TT2_LV"="Latvia 2003", "TT2_NL"="Netherlands 2005", "TT2_PL"="Poland 2004",
                              "TT2_SE"="Sweden 2001", "TT2_SI"="Slovenia 2001", "TT2_UK"="United Kingdom 2005" ))


rownames(nttaTT1c)
rownames(nttaTT1c) <- nttaTT1c$age
rownames(nttaTT2c) <- nttaTT2c$age


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############
############ Preparing time transfers of men for the final GGPlot
############

nttaTT1h <- t(as.matrix(nttaTT1c))

####### deleting row age 
nttaTT1h <- nttaTT1h[-c(29),]

###### general heatmap


colnames(nttaTT1h)

TT1heatmap <- heatmap(nttaTT1h, Rowv=NA, Colv=NA)

####### maximum and minimum values in the matrix
maxTT1 <- max(nttaTT1h)
minTT1 <- min(nttaTT1h)

maxTT1
minTT1

########
########
######## In what order the countries appear on the figure: sorting the countries
########

#1. Sort the data by the maximum place
#max <- as.numeric(max.col(nttaTT1h))
#nttaTT1hs <- cbind(nttaTT1h, max)
#nttaTT1hso <- nttaTT1hs[order(-nttaTT1hs[,87]),]

#2. sort the data according to when values turn negative (this method is used in the paper)
neg <- as.numeric(max.col(nttaTT1h < 0,ties.method = "first"))
neg


#replace value of no negative values according to Women's plot
#Ghana
neg[4] <- -8
#Senegal
neg[3] <- -7
#India
neg[8] <- -6
#South Africa
neg[2] <- -5
#Mauritius
neg[10] <- -4


#3. sort the data according to how many ages have negative values
#neg <- as.numeric(rowMeans(nttaTT1h < 0))
#neg


nttaTT1hs <- cbind(nttaTT1h, neg)

nttaTT1hso <- nttaTT1hs[order(-nttaTT1hs[,87]),]

#4. other sorting is also possible, like alphabetical, etc.

####### making a list of countries in order

nttaTT1hsor <- as.data.frame(nttaTT1hso)
Tborder <- setDT(nttaTT1hsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder


####### deleting the sorting coloumn
nttaTT1hso <- nttaTT1hso[,-c(87)]

TT1heatmap <- heatmap(nttaTT1hso, Rowv=NA, Colv=NA)

########
######## GGPLOT is used for the final visualization, so the data has to be restructured
########

TT1df <- as.data.frame(nttaTT1hso)

TT1d <- setDT(TT1df, keep.rownames = TRUE)[]
names(TT1d)[1] <- "country"

######## Reorder Data for ggplot2 plot
TT1melt <- melt(TT1d)

TT1melt
str(TT1melt)

######## Save the restructured data (men) for the plot

save(TT1melt, file="TT1melt.Rdata")

setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")
load("TT1melt.Rdata")

######## Looking at basic heatmap in ggplot

#reorder the country names

TT1melt$country <- factor(x = TT1melt$country,
                          levels = corder, 
                          ordered = TRUE)

TT1tiles <- ggplot(TT1melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  labs(title="Time transfers by age (men)",
       caption="")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
TT1tiles


############
############ Preparing time transfers of women for the final GGPlot
############


nttaTT2h <- t(as.matrix(nttaTT2c))

colnames(nttaTT2h)

TT2heatmap <- heatmap(nttaTT2h, Rowv=NA, Colv=NA)

####### deleting row age 
nttaTT2h <- nttaTT2h[-c(29),]

####### maximum and minimum values in the matrix
maxTT2 <- max(nttaTT2h)
minTT2 <- min(nttaTT2h)

maxTT2
minTT2

########
########
######## In what order the countries appear on the figure: sorting the countries
########


#1. Sort the data by the maximum place
#max <- as.numeric(max.col(nttaTT2h))
#nttaTT2hs <- cbind(nttaTT2h, max)
#nttaTT2hso <- nttaTT2hs[order(-nttaTT2hs[,87]),]


#2. sort the data according to when values turn negative
#neg <- as.numeric(max.col(nttaTT2h < 0,ties.method = "first"))
#neg

#replace value of no negative values
#neg <- replace(neg, neg==1, 80)
#neg

#3. sort the data according to how many ages have negative values
#neg <- as.numeric(rowMeans(nttaTT2h < 0))
#neg

#4. sort data according to men's plot

nttaTT2hs <- cbind(nttaTT2h, neg)

nttaTT2hso <- nttaTT2hs[order(-nttaTT2hs[,87]),]

#4. Other sorting is also possible, for example clusters as in Vargha & Istenic 2023

######## Making a list of countries in order

nttaTT2hsor <- as.data.frame(nttaTT2hso)
Tborder <- setDT(nttaTT2hsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder

######## deleting the sorting coloumn
nttaTT2hso <- nttaTT2hso[,-c(87)]

TT2heatmap <- heatmap(nttaTT2hso, Rowv=NA, Colv=NA)

########
######## GGPLOT is used for the final visualization, so the data has to be restructured
########

TT2df <- as.data.frame(nttaTT2hso)


TT2d <- setDT(TT2df, keep.rownames = TRUE)[]
names(TT2d)[1] <- "country"

######## Reorder Data for ggplot2 plot
TT2melt <- melt(TT2d)

TT2melt
str(TT2melt)

######## Save the restructured data (women) for the plot

save(TT2melt, file="TT2melt.Rdata")

setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")
load("TT2melt.Rdata")

#reorder the country names 

TT2melt$country <- factor(x = TT2melt$country,
                          levels = corder, 
                          ordered = TRUE)

TT2tiles <- ggplot(TT2melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  labs(title="Time transfers by age (women)",
       caption="")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
TT2tiles

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############
############ Final figure for men and women using shades of blue and red
############
############

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########## Defining the colours of blue and red

bl <- colorRampPalette(c("darkblue","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

########## Age appearing on the plot

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

############ Final figure for men 

TT1tiles <- ggplot(TT1melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", , linewidth=0.45)+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-12, 12)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  labs(title="Men") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "H/d") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

TT1tiles 

############ Final figure for women 

TT2tiles <- ggplot(TT2melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", , linewidth=0.45)+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-12, 12)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "H/d") +
  labs(title="Women") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

TT2tiles

############ Patchwork: putting the two plots together


fig <- TT2tiles + TT1tiles

figTT <- fig +
  plot_layout(guides = 'collect')
  
figTT

fig <- TT2tiles + TT1tiles

figTT <- fig +
  plot_layout(guides = 'collect')

figTT

figTT <- figTT + plot_annotation(
  title = "",
  caption = "",
  theme = theme(plot.title = element_text(size = 18)))

tiff("Outputs/TTViz.tiff", units="in", width=20, height=6, res=500)
plot(figTT, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/GitHub/TTViz.jpg", units="in", width=20, height=6, res=500)
plot(figTT, align="h", rel_widths=c(1,0.2))
dev.off()

figTT <- figTT + plot_annotation(
  title = "Time transfers by gender and age in 28 countries",
  caption = "Data: Counting Women's Work 2022, Gal et al. 2017, Hammer 2014, Vargha et al. 2016
             Replication files & details:https://github.com/LiliVargha/Time-Transfers",
  theme = theme(plot.title = element_text(size = 18)))


jpeg("Outputs/GitHub/TTVizcaption.jpg", units="in", width=20, height=6, res=500)
plot(figTT, align="h", rel_widths=c(1,0.2))
dev.off()

