library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(geepack)
library(MESS)
library(mice)
library(lme4)
library(lmerTest)
library(gee)
library(pROC)
library(caret)
library(ggpubr)
sclerosis=read.csv("C:/Users/Administrator/Desktop/thesis/S1_Data.csv")
exam.sclero <- sclerosis  %>% filter(age >= 50) %>% select(-c(id,treat,cigaret,smoking01))
summary(exam.sclero)
exam.sclero <- within(exam.sclero, {
  zyg <- factor(zyg, levels = 1:2, labels = c("MZ", "DZ"))
  sex <- factor(sex, levels = 1:2, labels = c("M", "F"))
  smoking <- factor(smoking, levels = 0:2, labels = c("Never", "Former", "Current"))
  tr <- as.factor(tr)
})
exam.sclero <- exam.sclero %>% mutate(scoreMP_level = factor(ifelse(scoreMP %in% c(0:3),"0-3",
                                                                    ifelse(scoreMP %in% c(4:6),"4-6","7-9"))))
summary(exam.sclero)
exam.sclero.avg <- exam.sclero %>% group_by(zyg) %>% summarize_if(is.numeric, mean, na.rm=T) 
exam.sclero.sd <- exam.sclero %>% group_by(zyg) %>% summarize_if(is.numeric, sd, na.rm=T) 
exam.MZ <- exam.sclero %>% filter(zyg == "MZ")
exam.DZ <- exam.sclero %>% filter(zyg == "DZ")





#scoreMP模型
exam.MZ.cleanMP = exam.MZ %>% filter(twinid != "66" & twinid != "73" & twinid != "96" & twinid != "107")
model.scoreMP.lm = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP,
                       id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.scoreMP.lm) 
tidy(model.scoreMP.lm, conf.int = T, exponentiate = F) #odds ratio 

QIC(model.scoreMP.lm) #35.23

Rsuqare.scoreMP = 1 - sum((model.scoreMP.lm$y - model.scoreMP.lm$fitted.values)^2) / sum((model.scoreMP.lm$y - mean(model.scoreMP.lm$y))^2)
Rsuqare.scoreMP #0.241

#交互效应解读
#age*scoreMP--interaction plot
#exam.MZ.cleanMP = exam.MZ.cleanMP  %>% mutate(age_group = factor(ifelse(age < median(age), "younger", "older")))
exam.MZ.cleanMP = exam.MZ.cleanMP  %>% mutate(age_group = factor(ifelse(age < 60, "[50,60)", ifelse(age < 70, "[60,70)", ifelse(age < 80, "[70,80)",">80")))))
exam.MZ.cleanMP = exam.MZ.cleanMP %>% mutate(IMT_pred = model.scoreMP.lm$fitted.values)
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄�?",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="咀嚼能力得�?",y="IMT水平拟合�?")
#socreMP和IMT间的关系随年龄逐渐减弱

#sex*scoreMP
#exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = sex)) + geom_point()+ stat_smooth(method = lm)
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP_level, y = IMT_pred, colour = sex)) + geom_boxplot() +
  scale_colour_manual(name="性别",values=c("#7EC0EE","#FA8072"),labels=c("�?","�?")) + labs(x="咀嚼能力得�?",y="IMT拟合�?") +
  stat_compare_means(aes(group=sex),label="p.format") + ylim(0.35,1)
#在低咀嚼能力组中，男性和女性的IMT拟合值有显著差异 p-value: 2e-09, 0.008,0.8
exam.MZ.cleanMP %>% ggplot(aes(x = sex, y = IMT_pred, colour = scoreMP_level)) + geom_boxplot() +
  scale_colour_manual(name="咀嚼能力得�?",values=c("#7EC0EE","#FA8072","#458B00")) + labs(x="性别",y="IMT拟合�?") +
  stat_compare_means(aes(group=scoreMP_level),label="p.format") + ylim(0.35,1) + scale_x_discrete(labels=c("M"="�?","F"="�?"))



#===========================================================

#NT模型
model.NT.lm = geeglm(IMT ~ sex + age + smoking + bmi + NT + NT*sex + smoking*NT, data = exam.MZ,
                     id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.NT.lm)
tidy(model.NT.lm, conf.int = TRUE, exponentiate = F) 

QIC(model.NT.lm) #54.11
Rsuqare.NT = 1 - sum((model.NT.lm$y - model.NT.lm$fitted.values)^2) / sum((model.NT.lm$y - mean(model.NT.lm$y))^2) #0.162
Rsuqare.NT

#sex*NT-- interaction effect
exam.MZ.NT = exam.MZ %>% mutate(IMT_pred = model.NT.lm$fitted.values)
exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_pred, colour = sex)) + geom_point() + stat_smooth(method = lm) + 
  labs(x="牙齿数量",y="IMT拟合�?") + scale_colour_manual(name="性别",values=c("#7EC0EE","#FA8072"),labels=c("�?","�?"))
#在男性中牙齿数量和IMT拟合值的负相关性更明显


#模型检验与诊断
#在scoreMP模型中对比普通线性回�?
model.scoreMP = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP,
                       id = twinid, family = gaussian, corstr = "independence")
summary(model.scoreMP) 
#model.scoreMP = lm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP)

#剔除IMT离群�?
exam.MZ.cleanMP.ed = exam.MZ.cleanMP %>% filter(twinid != "50") %>% select(-c(IMT_pred))
model.scoreMP.ed = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP.ed,
                       id = twinid, family = gaussian, corstr = "independence")
summary(model.scoreMP.ed) 
#model.scoreMP.ed = lm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP.ed)


model.scoreMP.lm.ed = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP.ed,
                       id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.scoreMP.lm.ed) 
tidy(model.scoreMP.lm.ed, conf.int = TRUE, exponentiate = F) 
QIC(model.scoreMP.lm.ed) #22.90
Rsquare.scoreMP.ed = 1 - sum((model.scoreMP.lm.ed$y - model.scoreMP.lm.ed$fitted.values)^2) / sum((model.scoreMP.lm.ed$y - mean(model.scoreMP.lm.ed$y))^2)
Rsquare.scoreMP.ed #0.261


#残差分析
cbind.data.frame(x = model.scoreMP.ed$fitted.values, y = model.scoreMP.ed$residuals) %>% ggplot(aes(x = x, y = y )) + 
  geom_point() + stat_smooth(method = lm, se = F) +
  labs(x = "IMT���ֵ", y = "Pearson�в�")
qqnorm(model.scoreMP.ed$residuals)
qqline(model.scoreMP.ed$residuals) 

cbind.data.frame(x = model.scoreMP.lm.ed$fitted.values, y = model.scoreMP.lm.ed$residuals) %>% ggplot(aes(x = x, y = y )) + 
  geom_point() + stat_smooth(method = lm, se = F) +
  labs(x = "IMT拟合�?", y = "Pearson残差")

qqnorm(residuals(model.scoreMP.lm.ed))
qqline(residuals(model.scoreMP.lm.ed)) 


#剔除bmi后模型没有什么大变化
model.scoreMP.lm.im = geeglm(IMT ~ sex +age + smoking + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP.ed,
                             id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.scoreMP.lm.im)
QIC(model.scoreMP.lm.ed) #22.90
QIC(model.scoreMP.lm.im) #21.33
Rsquare.scoreMP.ed.im = 1 - sum((model.scoreMP.lm.im$y - model.scoreMP.lm.im$fitted.values)^2) / sum((model.scoreMP.lm.im$y - mean(model.scoreMP.lm.im$y))^2)
Rsquare.scoreMP.ed #0.261
Rsquare.scoreMP.ed.im #0.248

#最终模型model.scoreMP.lm.ed

#-----------------------------------------------------------------------------
#logistic模型
model.scoreMP.glm = geeglm(IMT01 ~ sex + age + smoking + bmi + scoreMP + sex*scoreMP, data = exam.MZ.cleanMP,
                           id = twinid, family = binomial, corstr = "exchangeable")
summary(model.scoreMP.glm) 
tidy(model.scoreMP.glm, conf.int = T, exponentiate = T) 
#交互效应解读
#scoreMP和age
exam.MZ.cleanMP = exam.MZ.cleanMP %>% mutate(IMT_prob = model.scoreMP.glm$fitted.values)
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_prob, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  labs(x="���ݾ׽������÷�",y="������Ӳ����Ԥ�����") + scale_colour_manual(name="�����",values=c("#7EC0EE","#FA8072","#458B00","#8B4726"))
#大于80岁群体中的负相关明显

#scoreMP和sex
#exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_prob, colour = sex)) + geom_point()
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP_level, y = IMT_prob, colour = sex)) + geom_boxplot() +
  scale_colour_manual(name="性别",values=c("#7EC0EE","#FA8072"),labels=c("�?","�?")) + labs(x="咀嚼能力得�?",y="患动脉硬化的预测概率") + 
  stat_compare_means(aes(group=sex),label="p.format") + ylim(0,0.48)
#在低咀嚼能力得分组中的性别差异明显
exam.MZ.cleanMP %>% ggplot(aes(x = sex, y = IMT_prob, colour = scoreMP_level)) + geom_boxplot() +
  scale_colour_manual(name="咀嚼能力得�?",values=c("#7EC0EE","#FA8072","#458B00")) + labs(x="性别",y="患动脉硬化的预测概率") + 
  stat_compare_means(aes(group=scoreMP_level),label="p.format") + ylim(0,0.48) + scale_x_discrete(labels=c("M"="�?","F"="�?"))



#====================================================================
model.NT.glm = geeglm(IMT01 ~ sex + age + smoking + bmi + NT + NT*sex + NT*smoking, data = exam.MZ,
                      id = twinid, family = binomial, corstr = "exchangeable")
summary(model.NT.glm)
tidy(model.NT.glm, conf.int = TRUE, exponentiate = T) 
#交互效应解读
#sex和NT
exam.MZ.NT = exam.MZ.NT %>% mutate(IMT_prob = model.NT.glm$fitted.values)
exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_prob, colour = sex)) + geom_point() + stat_smooth(method = lm) + 
  labs(x="��������",y="������Ӳ����Ԥ�����") + scale_colour_manual(name="�Ա�",values=c("#7EC0EE","#FA8072"),labels=c("��","Ů"))
#男性负相关更明�?
#smoking和NT
exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_prob, colour = smoking)) + geom_point() + stat_smooth(method = lm, se=F) +
  scale_colour_manual(name="吸烟状�?",values=c("#7EC0EE","#FA8072","#458B00"),labels=c("不吸�?","从前吸烟","现在吸烟")) +
  labs(x="牙齿数量",y="患动脉硬化的预测概率")
#不吸烟组整体风险降低，但相关性上看和当前吸烟组差不多，而从前吸烟组的相关性最�?

#模型选择
QIC(model.NT.glm) #81.9
QIC(model.scoreMP.glm) #75.3
#ROC曲线

par(pty = "s")
roc(as.vector(t(exam.MZ.NT$IMT01)), as.vector(t(exam.MZ.NT$IMT_prob)), plot = T, legacy.axes = T, xlab = "假正例率", ylab = "真正例率", col = "#7EC0EE",
    print.auc = T,print.thres=F) # NT�?0.08作为阈�?
plot.roc(as.vector(t(exam.MZ.cleanMP$IMT01)),as.vector(t(exam.MZ.cleanMP$IMT_prob)), col = "#FA8072", print.auc = T, print.auc.y = 0.4, add = T, print.thres=F) #scoreMP阈�?0.032
legend("bottomright", legend = c("牙齿数量","咀嚼能力得�?"), col = c("#7EC0EE","#FA8072"), lwd = 2)


#混淆矩阵

exam.MZ.cleanMP = exam.MZ.cleanMP %>% mutate(IMT01_pred = as.numeric(IMT_prob > 0.032))
confusionMatrix(table(factor(exam.MZ.cleanMP$IMT01), factor(exam.MZ.cleanMP$IMT01_pred)), positive = "1") #accuracy=0.713
exam.MZ.NT = exam.MZ.NT %>% mutate(IMT01_pred = as.numeric(IMT_prob > 0.08))
confusionMatrix(table(factor(exam.MZ.NT$IMT01), factor(exam.MZ.NT$IMT01_pred)), positive = "1")#accuracy=0.857

#totaltooth模型中和普通logistic模型对比
model.NT = glm(IMT01 ~ sex + age + smoking + bmi + NT + NT*sex + NT*smoking, data = exam.MZ,
               family = binomial)
summary(model.NT)
tidy(model.NT, conf.int = TRUE, exponentiate = T) 

# calculate pseudo R square
ll.null=model.NT$null.deviance/-2
ll.proposed=model.NT$deviance/-2
pesudo_R_square=(ll.null-ll.proposed)/ll.null
pesudo_R_square  #0.237
#p-value
1-pchisq(2*(ll.proposed-ll.null),df=(length(model.NT$coefficients)-1))

#GEE模型较好


#-----------------------------------------------------------------------------
#深入检查scoreMP模型中交互效�?#sex/age ~ scoreMP
#检查lm
inact.MP.M = exam.MZ.cleanMP %>% filter(sex == "M" )
model.inactMP.M = geeglm(IMT ~ age + smoking + bmi + scoreMP + scoreMP*age, data = inact.MP.M,
                       id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.inactMP.M) #scoreMP依然显著
inact.MP.M %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄�?",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="咀嚼能力得�?",y="IMT拟合�?")
#男性中是年龄低的负相关性更�?


inact.MP.F = exam.MZ.cleanMP %>% filter(sex == "F" )
model.inactMP.F = geeglm(IMT ~ age + smoking + bmi + scoreMP + scoreMP*age, data = inact.MP.F,
                         id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.inactMP.F) #scoreMP依然显著
inact.MP.F %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄�?",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="咀嚼能力得�?",y="IMT拟合�?")
#女性中除了低年龄组外就没有负相关性了
#检查glm,模型失去解释�?



#sex ~ NT 检查lm模型系数NT不显�?,检查glm模型失去解释�?
inact.NT.M = exam.MZ.NT %>% filter(sex == "M")
model.inactNT.M = geeglm(IMT01 ~ age + smoking + bmi + NT  + NT*smoking, data = inact.NT.M,
                         id = twinid, family = binomial, corstr = "exchangeable")
summary(model.inactNT.M)
inact.NT.F = exam.MZ.NT %>% filter(sex == "F")
model.inactNT.F = geeglm(IMT01 ~ age + smoking + bmi + NT  + NT*smoking, data = inact.NT.F,
                         id = twinid, family = binomial, corstr = "exchangeable")
summary(model.inactNT.F)
#在普通逻辑斯蒂模型中检�?,普通logistic就没有意义了

model.inactNT.M = glm(IMT01 ~ age + smoking + bmi + NT + NT*smoking , data = inact.NT.M,family = binomial)
summary(model.inactNT.M)

ll.null = model.inactNT.M$null.deviance/-2
ll.proposed = model.inactNT.M$deviance/-2
#p-value = 0.214 
1-pchisq(2*(ll.proposed-ll.null),df=(length(model.inactNT.M$coefficients)-1))

model.inactNT.F = glm(IMT01 ~ age + smoking + bmi + NT + NT*smoking , data = inact.NT.F,family = binomial)
summary(model.inactNT.F)

ll.null = model.inactNT.F$null.deviance/-2
ll.proposed = model.inactNT.F$deviance/-2
#p-value = 0.443 
1-pchisq(2*(ll.proposed-ll.null),df=(length(model.inactNT.F$coefficients)-1))


#======================================================================
#模型预测
#生成训练集和测试�?
set.seed(120)
srate=8/10 #划分�?
train_test <- c(rep(1,length=trunc((srate)*nrow(exam.MZ.cleanMP.ed))),rep(2,length=(nrow(exam.MZ.cleanMP.ed)-trunc((srate)*nrow(exam.MZ.cleanMP.ed)))))
exam.MZ.cleanMP.ed = exam.MZ.cleanMP.ed %>% mutate(traintest = sample(train_test))
exam.MZ.cleanMP.ed$traintest = factor(exam.MZ.cleanMP.ed$traintest, levels=c(1,2), labels=c("train","test"))

#train data
scoreMP.train = subset(exam.MZ.cleanMP.ed,traintest=="train")
scoreMP.test = subset(exam.MZ.cleanMP.ed,traintest=="test")

scoreMP.model.pred = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = scoreMP.train,
                             id = twinid, family = gaussian, corstr = "exchangeable")
summary(scoreMP.model.pred) 

scoreMP.train$IMT_pred = predict(scoreMP.model.pred,scoreMP.train)
scoreMP.train$IMT_resi = scoreMP.train$IMT - scoreMP.train$IMT_pred
scoreMP.test$IMT_pred = predict(scoreMP.model.pred,scoreMP.test)
scoreMP.test$IMT_resi = scoreMP.test$IMT - scoreMP.test$IMT_pred

#计算均方误差和均方根误差
MSE.train = 1/nrow(scoreMP.train)*sum(scoreMP.train$IMT_resi^2)
RMSE.train = sqrt(MSE.train)
MSE.test = 1/nrow(scoreMP.test)*sum(scoreMP.test$IMT_resi^2)
RMSE.test = sqrt(MSE.test)

#预测可视�?
plot.frame = rbind(scoreMP.train,scoreMP.test)
library(lattice)
xyplot(IMT_pred ~ IMT | traintest, data = plot.frame,pch=c(20,20),xlim=c(0.2,1.4),ylim=c(0.3,0.9),
       type=c("p","g"),aspect=1,col=c("#7EC0EE","#7EC0EE"),panel=function(x,y,...)
         {panel.xyplot(x,y,...)
           panel.segments(0.4,0.4,1,1,col="#FA8072",cex=1)
         },xlab="IMT实际�?",ylab="IMT拟合�?")
