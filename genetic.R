library(dplyr)
library(tidyr)
library(ggplot2)
library(umx)
#original data
sclerosis=read.csv("C:/Users/Administrator/Desktop/thesis/S1_Data.csv")
nosmoke = sclerosis  %>% select(twinid,zyg,age,sex,IMT,scoreMP,totaltooth,avePD)


#缺失值删�?
nosmoke.NT = nosmoke %>% select(zyg,age,sex,IMT,totaltooth)
#nosmoke.MP = nosmoke %>% filter(twinid != "66" & twinid != "97" & twinid != "96" & twinid != "107") %>% select(zyg,age,sex,IMT,scoreMP)
#nosmoke.PD = nosmoke %>% filter(twinid != "28" & twinid != "90" & twinid != "128") %>% select(zyg,age,sex,IMT,avePD)
nosmoke.MP = nosmoke %>% filter(twinid != "66" & twinid != "73" & twinid != "96" & twinid != "97" & twinid != "106" & twinid != "107") %>%
             select(zyg,age,sex,IMT,scoreMP)
nosmoke.PD = nosmoke %>% filter(twinid != "28" & twinid != "32" & twinid != "36" & twinid != "41" & twinid != "53" & twinid != "54" 
                                & twinid != "64" & twinid != "69" & twinid != "90" & twinid != "124" & twinid != "128" & twinid != "132" & twinid != "134") %>% 
            select(zyg,age,sex,IMT,avePD)

nosmoke.PD1 = nosmoke %>% filter(twinid != "28" & twinid != "32" & twinid != "36" & twinid != "41" & twinid != "53" & twinid != "54" 
                                 & twinid != "64" & twinid != "69" & twinid != "90" & twinid != "124" & twinid != "128" & twinid != "132" & twinid != "134") %>% 
            select(twinid,zyg,age,sex,IMT,totaltooth,scoreMP,avePD)
nosmoke.ALL = nosmoke.PD1 %>% filter(twinid != "66" & twinid != "73" & twinid != "96" & twinid != "97" & twinid != "106" & twinid != "107") %>% select(zyg,age,sex,IMT,totaltooth,scoreMP,avePD)


#整理成twin数据集样�?

nosmokeNT1 = nosmoke.NT[seq(from=1,to=(nrow(nosmoke.NT)-1),by=2),]
nosmokeNT2 = nosmoke.NT[seq(from=2,to=nrow(nosmoke.NT),by=2),]
nosmokeNT = cbind.data.frame(nosmokeNT1,nosmokeNT2)
nosmokeNT = nosmokeNT[,c(1,2,3,4,9,5,10)]
names(nosmokeNT) = c("zyg","age","sex","IMT1","IMT2","totaltooth1","totaltooth2")

nosmokeMP1 = nosmoke.MP[seq(from=1,to=(nrow(nosmoke.MP)-1),by=2),]
nosmokeMP2 = nosmoke.MP[seq(from=2,to=nrow(nosmoke.MP),by=2),]
nosmokeMP = cbind.data.frame(nosmokeMP1,nosmokeMP2)
nosmokeMP = nosmokeMP[,c(1,2,3,4,9,5,10)]
names(nosmokeMP) = c("zyg","age","sex","IMT1","IMT2","scoreMP1","scoreMP2")

nosmokePD1 = nosmoke.PD[seq(from=1,to=(nrow(nosmoke.PD)-1),by=2),]
nosmokePD2 = nosmoke.PD[seq(from=2,to=nrow(nosmoke.PD),by=2),]
nosmokePD = cbind.data.frame(nosmokePD1,nosmokePD2)
nosmokePD = nosmokePD[,c(1,2,3,4,9,5,10)]
names(nosmokePD) = c("zyg","age","sex","IMT1","IMT2","avePD1","avePD2")

nosmokeALL1 = nosmoke.ALL[seq(from=1,to=(nrow(nosmoke.ALL)-1),by=2),]
nosmokeALL2 = nosmoke.ALL[seq(from=2,to=nrow(nosmoke.ALL),by=2),]
nosmokeALL = cbind.data.frame(nosmokeALL1,nosmokeALL2)
nosmokeALL = nosmokeALL[,c(1,2,3,4,11,5,12,6,13,7,14)]
names(nosmokeALL) = c("zyg","age","sex","IMT1","IMT2","totaltooth1","totaltooth2","scoreMP1","scoreMP2","avePD1","avePD2")

#男女数据
nosmokeNT.M = nosmokeNT %>% filter(sex == 1)
nosmokeNT.F = nosmokeNT %>% filter(sex == 2)
nosmokeMP.M = nosmokeMP %>% filter(sex == 1)
nosmokeMP.F = nosmokeMP %>% filter(sex == 2)
nosmokePD.M = nosmokePD %>% filter(sex == 1)
nosmokePD.F = nosmokePD %>% filter(sex == 2)
nosmokeALL.M= nosmokeALL %>% filter(sex == 1)
nosmokeALL.F= nosmokeALL %>% filter(sex == 2)

#ACE模型--单一变量
#totaltooth
selDVs = c("totaltooth")
dz = nosmokeNT.M[nosmokeNT.M$zyg == 2,]
mz = nosmokeNT.M[nosmokeNT.M$zyg == 1,]
ACE.NT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果相似


selDVs = c("totaltooth")
dz = nosmokeNT.F[nosmokeNT.F$zyg == 2,]
mz = nosmokeNT.F[nosmokeNT.F$zyg == 1,]
ACE.NT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果相似


#IMT
selDVs = c("IMT")
dz = nosmokeNT.M[nosmokeNT.M$zyg == 2,]
mz = nosmokeNT.M[nosmokeNT.M$zyg == 1,]
ACE.IMT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果不同

selDVs = c("IMT")
dz = nosmokeNT.F[nosmokeNT.F$zyg == 2,]
mz = nosmokeNT.F[nosmokeNT.F$zyg == 1,]
ACE.IMT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果相似





#scoreMP
selDVs = c("scoreMP")
dz = nosmokeMP.M[nosmokeMP.M$zyg == 2,]
mz = nosmokeMP.M[nosmokeMP.M$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果不同

selDVs = c("scoreMP")
dz = nosmokeMP.F[nosmokeMP.F$zyg == 2,]
mz = nosmokeMP.F[nosmokeMP.F$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果不同



#avePD
selDVs = c("avePD")
dz = nosmokePD.M[nosmokePD.M$zyg == 2,]
mz = nosmokePD.M[nosmokePD.M$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果不同但相�?

selDVs = c("avePD")
dz = nosmokePD.F[nosmokePD.F$zyg == 2,]
mz = nosmokePD.F[nosmokePD.F$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果不同但相�?


#ACE模型--双变�?
#IMT&totaltooth
selDVs = c("IMT","totaltooth")
selCovs = c("age")
dz = nosmokeNT.M[nosmokeNT.M$zyg == 2,]
mz = nosmokeNT.M[nosmokeNT.M$zyg == 1,]
ACE.IMT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果相似
ACE.IMT = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dz, mzData = mz, sep = "") 

selDVs = c("IMT","totaltooth")
dz = nosmokeNT.F[nosmokeNT.F$zyg == 2,]
mz = nosmokeNT.F[nosmokeNT.F$zyg == 1,]
ACE.IMT = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与openmx结果相似

#IMT&scoreMP
selDVs = c("IMT","scoreMP")
dz = nosmokeMP.M[nosmokeMP.M$zyg == 2,]
mz = nosmokeMP.M[nosmokeMP.M$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果相似

selDVs = c("IMT","scoreMP")
dz = nosmokeMP.F[nosmokeMP.F$zyg == 2,]
mz = nosmokeMP.F[nosmokeMP.F$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果相似

#IMT&avePD
selDVs = c("IMT","avePD")
dz = nosmokePD.M[nosmokePD.M$zyg == 2,]
mz = nosmokePD.M[nosmokePD.M$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果不同

selDVs = c("IMT","avePD")
dz = nosmokePD.F[nosmokePD.F$zyg == 2,]
mz = nosmokePD.F[nosmokePD.F$zyg == 1,]
ACE.MP = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "") #与Openmx结果相似


#IMT&totaltooth,scoreMP,avePD
