#import packages
#install.packages("gee")
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(geepack)
library(MESS)
setwd("C:/Users/Administrator/Desktop/thesis")
#original data
sclerosis=read.csv("C:/Users/Administrator/Desktop/thesis/S1_Data.csv")
#missing values

#table with missing values
missing.values <- sclerosis %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 
#visualize missing data
missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='变量', y="缺失值数???") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

missing.values <- sclerosis %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

missing.percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("已知数据", "缺失数据")) +
  coord_flip() +
  labs(x = '变量???', y = "缺失值占???%")

missing.percentage.plot
#黑白色设???
missing.percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('#363636', '#9C9C9C'), labels = c("已知数据", "缺失数据")) +
  coord_flip() +
  labs(x = '变量???', y = "缺失值占???%")

missing.percentage.plot





str(sclerosis)
#=============================================================
#MZ data
exam.sclero <- sclerosis %>% select(-c(id,treat,cigaret,smoking01))
exam.sclero <- within(exam.sclero, {
  zyg <- factor(zyg, levels = 1:2, labels = c("MZ", "DZ"))
  sex <- factor(sex, levels = 1:2, labels = c("M", "F"))
  smoking <- factor(smoking, levels = 0:2, labels = c("Never", "Former", "Current"))
  tr <- as.factor(tr)
  })
exam.sclero <- exam.sclero %>% mutate(scoreMP_level = factor(ifelse(scoreMP %in% c(0:3),"[0,3]",
                                                                    ifelse(scoreMP %in% c(4:6),"[4,6]","[7,9]"))))
summary(exam.sclero)

exam.sclero.avg <- exam.sclero  %>% group_by(zyg) %>% summarize_if(is.numeric, mean, na.rm=T) 
exam.sclero.sd <- exam.sclero %>% group_by(zyg) %>% summarize_if(is.numeric, sd, na.rm=T)

exam.MZ <- exam.sclero  %>% filter(zyg == "MZ")
exam.DZ <- exam.sclero  %>% filter(zyg == "DZ")
summary(exam.MZ)


attach(exam.sclero)

#hypothesis test on numeric variables
boxplot(age ~ zyg)
t.test(age ~ zyg)
wilcox.test(age ~ zyg)
var.test(age ~ zyg)

boxplot(scoreMP ~ zyg)
t.test(scoreMP ~ zyg)
wilcox.test(scoreMP ~ zyg)
var.test(scoreMP ~ zyg)

boxplot(totaltooth ~ zyg)
t.test(totaltooth ~ zyg)
wilcox.test(totaltooth ~ zyg)
var.test(totaltooth ~ zyg)

boxplot(avePD ~ zyg)
t.test(avePD ~ zyg)
wilcox.test(avePD ~ zyg)
var.test(avePD ~ zyg)

boxplot(IMT ~ zyg)
t.test(IMT ~ zyg)
wilcox.test(IMT ~ zyg)
var.test(IMT ~ zyg)

boxplot(BH ~ zyg)
t.test(BH ~ zyg)
wilcox.test(BH ~ zyg)
var.test(BH ~ zyg)

boxplot(BW ~ zyg)
t.test(BW ~ zyg)
wilcox.test(BW ~ zyg)
var.test(BW ~ zyg)

boxplot(bmi ~ zyg)
t.test(bmi ~ zyg)
wilcox.test(bmi ~ zyg)
var.test(bmi ~ zyg)

#the comparison between >50 and <50
library(ggpubr)
#IMT水平
exam.sclero = exam.sclero %>% mutate(age50 = factor(ifelse(age >= 50, ">=50", "<50")))
#剔除IMT中的outlier
exam.sclero.ed = exam.sclero %>% filter(IMT != 2.61)
exam.sclero.ed %>% ggplot(aes(x = IMT, fill = age50)) + geom_density(alpha=0.5) +
  scale_fill_manual(name = "年龄",values = c("#7EC0EE","#FA8072")) + labs(x = "IMT水平", y = "核密度估???")
#牙齿健康状况
nt50 = exam.sclero %>% ggplot(aes(x = age50, y = totaltooth)) + geom_boxplot(size=0.8) + 
       stat_compare_means(size=3.7) + labs(x = "年龄",y = "牙齿数量") + ylim(0,35) + 
       theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5))

mp50 = exam.sclero %>% ggplot(aes(x = age50, y = scoreMP)) + geom_boxplot(size=0.8) + 
       stat_compare_means() + labs(x = "年龄",y = "牙齿咀嚼能力得分") + ylim(0,9.5) + 
       theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5))

pd50 = exam.sclero %>% ggplot(aes(x = age50, y = avePD)) + geom_boxplot(size=0.8) + 
       stat_compare_means() + labs(x = "年龄",y = "牙周袋平均深度") + ylim(1.6,5.1) +
       theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5))
oralhealth50.plot = grid.arrange(nt50,mp50,pd50,nrow=1)
ggsave(plot=oralhealth50.plot,width=18,height=7,units="cm",dpi=600,filename = "oralhealth50.tiff")

#==============================================================
#>50岁同卵双胞胎数据的可视化(探索性分???) 

exam.sclero = exam.sclero %>% filter(age >= 50)
summary(exam.sclero)
exam.sclero.avg <- exam.sclero  %>% summarize_if(is.numeric, mean, na.rm=T) 
exam.sclero.sd <- exam.sclero  %>% summarize_if(is.numeric, sd, na.rm=T)
a = rbind.data.frame(exam.sclero.avg,exam.sclero.sd)
exam.MZ <- exam.sclero  %>% filter(zyg == "MZ")
exam.DZ <- exam.sclero  %>% filter(zyg == "DZ")
summary(exam.DZ)

attach(exam.sclero)

#hypothesis test on numeric variables
boxplot(age ~ zyg)
t.test(age ~ zyg)
wilcox.test(age ~ zyg)
var.test(age ~ zyg)

boxplot(scoreMP ~ zyg)
t.test(scoreMP ~ zyg)
wilcox.test(scoreMP ~ zyg)
var.test(scoreMP ~ zyg)

boxplot(totaltooth ~ zyg)
t.test(totaltooth ~ zyg)
wilcox.test(totaltooth ~ zyg)
var.test(totaltooth ~ zyg)

boxplot(avePD ~ zyg)
t.test(avePD ~ zyg)
wilcox.test(avePD ~ zyg)
var.test(avePD ~ zyg)

boxplot(IMT ~ zyg)
t.test(IMT ~ zyg)
wilcox.test(IMT ~ zyg)
var.test(IMT ~ zyg)

boxplot(BH ~ zyg)
t.test(BH ~ zyg)
wilcox.test(BH ~ zyg)
var.test(BH ~ zyg)

boxplot(BW ~ zyg)
t.test(BW ~ zyg)
wilcox.test(BW ~ zyg)
var.test(BW ~ zyg)

boxplot(bmi ~ zyg)
t.test(bmi ~ zyg)
wilcox.test(bmi ~ zyg)
var.test(bmi ~ zyg)

exam.MZ.M = filter(exam.MZ,sex == "M")
exam.MZ.F = filter(exam.MZ,sex == "F")

#性别分化
#吸烟状态比???
#smoking status的性别差异
exam.MZ %>% group_by(sex,smoking) %>% summarise(total = n())
#IMT的水平比???
exam.MZ.ed = filter(exam.MZ, IMT != 2.61)
exam.MZ.ed %>% ggplot(aes(x = sex, y = IMT, fill = sex)) + geom_boxplot() + 
  scale_fill_manual(name = "性别", values = c("#7EC0EE","#FA8072"), labels = c("???","???")) +
  scale_x_discrete(labels=c("M"="男???","F"="女???")) + stat_compare_means() +
  labs(x = "性别", y = "IMT水平") + guides(fill=F)
#IMT在两个性别中分布大致相同，男性偏???
#牙齿健康状况比较
exam.MZ %>% ggplot(aes(x = sex, y = totaltooth, fill = sex)) + geom_boxplot() + 
  scale_fill_manual(name = "性别", values = c("#7EC0EE","#FA8072"), labels = c("???","???")) +
  scale_x_discrete(labels=c("M"="男???","F"="女???")) + stat_compare_means() +
  labs(x = "性别", y = "牙齿数量") + guides(fill=F) + ylim(0,35) 
#totaltooth和sex关系密切
exam.MZ %>% ggplot(aes(x = sex, y = scoreMP, fill = sex)) + geom_boxplot() + 
  scale_fill_manual(name = "性别", values = c("#7EC0EE","#FA8072"), labels = c("???","???")) +
  scale_x_discrete(labels=c("M"="男???","F"="女???")) + stat_compare_means() +
  labs(x = "性别", y = "咀嚼能力得???") + guides(fill=F) + ylim(0,9.5)
#男性的scoreMP平均低于女性，方差基本一???
exam.MZ %>% ggplot(aes(x = sex, y = avePD, fill = sex)) + geom_boxplot() + 
  scale_fill_manual(name = "性别", values = c("#7EC0EE","#FA8072"), labels = c("???","???")) +
  scale_x_discrete(labels=c("M"="男???","F"="女???")) + stat_compare_means() +
  labs(x = "性别", y = "牙周袋平均深???") + guides(fill=F) + ylim(1.6,5) 
#女性avePD显著低于男性，男性方差更???

#年龄分化
exam.MZ %>% ggplot(aes(x = age, y = totaltooth)) + geom_point() + stat_smooth(method = lm, se = T, colour = "#FA8072") + labs(x = "年龄", y = "牙齿数量")
#totaltooth和年龄关系密???
exam.MZ %>% ggplot(aes(x = age, y = avePD)) + geom_point() + stat_smooth(method = lm, se = T, colour = "#FA8072") + labs(x = "年龄", y = "牙周袋平均深???")
#avePD和年龄呈正比
exam.MZ %>% ggplot(aes(x = age, y = scoreMP)) + geom_point()  + labs(x = "年龄", y = "咀嚼能力得???")
exam.MZ %>% ggplot(aes(x = scoreMP_level, y = age , fill = scoreMP_level)) + geom_boxplot() +
  scale_fill_manual(values=c("#FF8247","#FFA54F","#FFD700")) + stat_compare_means() +
  labs(x="咀嚼能力得???",y="年龄") + guides(fill=F) + ylim(50,90)

#吸烟影响
#吸烟对IMT的影???
exam.MZ.ed %>% ggplot(aes(x=smoking, y=IMT, fill=smoking)) + geom_boxplot() + 
  scale_fill_manual(name="吸烟状???",values=c("#B3EE3A","#FF7F24","#FA8072")) + stat_compare_means() +
  labs(x = "吸烟状???",y="IMT水平") + scale_x_discrete(labels=c("Never"="从不吸烟","Former"="过去吸烟","Current"="现在吸烟")) + 
  guides(fill=F)
#吸烟对牙齿健康状况的影响
exam.MZ %>% ggplot(aes(x=smoking, y=totaltooth, fill=smoking)) + geom_boxplot() + 
  scale_fill_manual(name="吸烟状???",values=c("#B3EE3A","#FF7F24","#FA8072")) + stat_compare_means() +
  labs(x = "吸烟状???",y="牙齿数量") + ylim(0,35) + scale_x_discrete(labels=c("Never"="从不吸烟","Former"="过去吸烟","Current"="现在吸烟")) + 
  guides(fill=F)
#totaltooth和smoking关系密切,主要是在方差???
my_comparisons=list(c("Never","Former"),c("Former","Current"),c("Never","Current"))
exam.MZ %>% ggplot(aes(x=smoking, y=scoreMP, fill=smoking)) + geom_boxplot() +  
  scale_fill_manual(name="吸烟状???",values=c("#B3EE3A","#FF7F24","#FA8072")) + stat_compare_means(comparisons = my_comparisons) +
  labs(x = "吸烟状???",y="咀嚼能力得???") + ylim(0,12) + scale_x_discrete(labels=c("Never"="从不吸烟","Former"="过去吸烟","Current"="现在吸烟")) + 
  guides(fill=F)
#吸烟影响scoreMP的方???
exam.MZ %>% ggplot(aes(x=smoking, y=avePD, fill=smoking)) + geom_boxplot()  + 
  scale_fill_manual(name="吸烟状???",values=c("#B3EE3A","#FF7F24","#FA8072")) + stat_compare_means() +
  labs(x = "吸烟状???",y="牙周袋平均深???") + ylim(1.6,5.1) + scale_x_discrete(labels=c("Never"="从不吸烟","Former"="过去吸烟","Current"="现在吸烟")) + 
  guides(fill=F)

#correlation
exam.sclero.conti <-exam.sclero %>% select(c(IMT,age,bmi,totaltooth,scoreMP,avePD))
corrplot(cor(exam.sclero.conti, use = "complete.obs"), method = "color", addCoef.col="black")
chart.Correlation(exam.sclero.conti, method = "pearson")





boxplot(totaltooth ~ as.factor(tr), data = exam.MZ) #totaltooth在twin data中的分布大致一???
t.test(totaltooth ~ as.factor(tr), data = exam.MZ)
wilcox.test(totaltooth ~ as.factor(tr), data = exam.MZ)
var.test(totaltooth ~ as.factor(tr), data = exam.MZ)

boxplot(avePD ~ as.factor(tr), data = exam.MZ) #avePD在twin data中的分布方差不同，在一组中存在较多离群???
t.test(avePD ~ as.factor(tr), data = exam.MZ)
wilcox.test(avePD ~ as.factor(tr), data = exam.MZ)
var.test(avePD ~ as.factor(tr), data = exam.MZ)

boxplot(scoreMP ~ as.factor(tr), data = exam.MZ) #scoreMP在twin data中的分布基本一???
t.test(scoreMP ~ as.factor(tr), data = exam.MZ) #但其中一组的平均水平低于另一???
wilcox.test(scoreMP ~ as.factor(tr), data = exam.MZ)
var.test(scoreMP ~ as.factor(tr), data = exam.MZ)

boxplot(IMT ~ as.factor(tr), data = exam.MZ.ed) #IMT在twin data中的分布方差不同,有一个outlier太大
t.test(IMT ~ as.factor(tr), data = exam.MZ)
wilcox.test(IMT ~ as.factor(tr), data = exam.MZ)
var.test(IMT ~ as.factor(tr), data = exam.MZ.ed)


#regression
model1 = lm(IMT ~ sex + age + smoking + scoreMP + NT + bmi + avePD, data = exam.MZ)
summary(model1)

y.tr1 = exam.MZ %>% filter(tr == 1) %>% select(IMT) %>% t %>% as.vector
y.tr0 = exam.MZ %>% filter(tr == 0) %>% select(IMT) %>% t %>% as.vector
cor(y.tr0, y.tr1)

#IMT~totaltooth
model2 = geeglm(IMT ~ sex + age + smoking + bmi + NT , data = exam.MZ,
                id = twinid, family = gaussian, corstr = "exchangeable")
summary(model2)
tidy(model2, conf.int = TRUE) #置信区间

#置信区间的另一种求???
model2 = gee(IMT ~ sex + age + smoking + bmi + NT , data = exam.MZ,
            id = twinid, family = gaussian, corstr = "exchangeable")
summary(model2)
cc <- as.data.frame(coef(summary(model2)))
citab <- cbind.data.frame(lwr=cc$Estimate-1.96*cc$`Robust S.E.`,
                          upr=cc$Estimate+1.96*cc$`Robust S.E.`)
rownames(citab) <- rownames(cc) #citab为系数的95%置信区间



model3 = geeglm(IMT01 ~ sex + age + smoking + bmi + NT , data = exam.MZ,
                id = twinid, family = binomial, corstr = "exchangeable")
summary(model3)
tidy(model3, conf.int = TRUE, exponentiate = T) #置信区间

#IMT~scoreMP
#contrasts(exam.MZ$sex) = cbind(f_vs_m=c(0,1))
model4 = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP , data = exam.MZ[!is.na(exam.MZ$scoreMP),],
                id = twinid, family = gaussian, corstr = "exchangeable")
summary(model4)
tidy(model4, conf.int = TRUE, exponentiate = F) #置信区间

model5 <- geeglm(IMT01 ~ sex + age + smoking + bmi + scoreMP , data = exam.MZ[!is.na(exam.MZ$scoreMP),],
                 id = twinid, family = binomial, corstr = "exchangeable")
summary(model5)
tidy(model5, conf.int = TRUE, exponentiate = T) #置信区间


#IMT~avePD
model6 <- geeglm(IMT ~ sex + age + smoking + bmi + avePD , data = exam.MZ[!is.na(exam.MZ$avePD),],
                 id = twinid, family = gaussian, corstr = "exchangeable")
summary(model6)
tidy(model6, conf.int = TRUE, exponentiate = F) #置信区间

model7 <- geeglm(IMT01 ~ sex + age + smoking + bmi + avePD , data = exam.MZ[!is.na(exam.MZ$avePD),],
                 id = twinid, family = binomial, corstr = "exchangeable")
summary(model7)
tidy(model7, conf.int = TRUE, exponentiate = T) #置信区间




countsmoke = 0
exam.MZ$smokflag = 0
for(i in seq(1,(nrow(exam.MZ)-1),by=2)){
  if(exam.MZ$smoking[i] == "Never" & exam.MZ$smoking[i] == exam.MZ$smoking[i+1]){
    countsmoke = countsmoke + 1
    exam.MZ$smokflag[i] = 1
    exam.MZ$smokflag[i+1] = 1
  }
}
countsmoke
exam.MZ.nosmoke = exam.MZ %>% filter(smokflag == 1) #不吸烟的双胞胎数???



#between-within model in IMT ~ Number of Tooth

for(i in seq(1,(nrow(exam.MZ)-1), by=2)){
  exam.MZ$tt.ave[i] = (exam.MZ$totaltooth[i] + exam.MZ$totaltooth[i+1])/2
  exam.MZ$tt.ave[i+1] = exam.MZ$tt.ave[i]
}
exam.MZ$tt.indiv = exam.MZ$totaltooth - exam.MZ$tt.ave
exam.MZ.nosmoke$NT.ave = exam.MZ.nosmoke$tt.ave / 5
exam.MZ.nosmoke$NT.indiv = exam.MZ.nosmoke$tt.indiv / 5
btwi.mz = lm(IMT ~ NT.indiv + NT.ave, data = exam.MZ.nosmoke)
summary(btwi.mz)
btwi.mz.gee = geeglm(IMT ~ NT.indiv + NT.ave, data = exam.MZ.nosmoke,id = twinid, family = gaussian, corstr = "exchangeable")
summary(btwi.mz.gee)

