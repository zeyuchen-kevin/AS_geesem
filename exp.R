#the rational in inverse gaussian
exam.MZ = filter(exam.MZ, IMT!=2.61)
exam.MZ$agegroup = cut(exam.MZ$age,breaks=4)
imt.var = with(exam.MZ, tapply(IMT, list(agegroup,sex,smoking), 'var'))
imt.mean = with(exam.MZ, tapply(IMT, list(agegroup,sex,smoking), 'mean'))
plot(log(imt.var)~log(imt.mean))
mf.lm = lm( c(log(imt.var)) ~ c(log(imt.mean)))
coef(mf.lm)   

#goodnes of fit on inverse gaussian
hist(exam.MZ$IMT)
cleandata = exam.MZ$IMT[which(exam.MZ$IMT!=max(exam.MZ$IMT))]
hist(cleandata)
library(statmod)
mu = mean(exam.MZ$IMT)
lamda=3
#lamda = 1/(length(exam.MZ$IMT))*sum(1/exam.MZ$IMT-1/mu)


theorticalprob=diff(pinvgauss(seq(0,max(exam.MZ$IMT),0.2),mu,lamda))
actualprob=as.vector(table(cut(exam.MZ$IMT,breaks=seq(0,max(exam.MZ$IMT),0.2))))/length(exam.MZ$IMT)


imt.fit = as.data.frame(rbind(
          cbind(names(table(cut(exam.MZ$IMT,breaks=seq(0,max(exam.MZ$IMT),0.2)))),
                           theorticalprob,rep("theory",(length(seq(0,max(exam.MZ$IMT),0.2))-1))),
          cbind(names(table(cut(exam.MZ$IMT,breaks=seq(0,max(exam.MZ$IMT),0.2)))),
                 actualprob,rep("actual",(length(seq(0,max(exam.MZ$IMT),0.2))-1)))))
colnames(imt.fit) = c("bin","prob","type")
ggplot(imt.fit, aes(x=bin, y=prob, fill=type)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.2))
             
#GLM using inverse gaussian
modelA = glm(IMT ~ sex.F + age + smoking.Former + smoking.Current + bmi + scoreMP + age*scoreMP + sex.F*scoreMP, 
             data = exam.MZ.cleanMP, family = gaussian)
summary(modelA) 

modelB = glm(IMT ~ sex.F + age + smoking.Former + smoking.Current + bmi + scoreMP + age*scoreMP + sex.F*scoreMP, data = exam.MZ.cleanMP,
             family = Gamma(link="inverse"),start=coef(modelA))
summary(modelB) 


# calculate pseudo R square
ll.null=modelB$null.deviance/-2
ll.proposed=modelB$deviance/-2
pesudo_R_square=(ll.null-ll.proposed)/ll.null
pesudo_R_square  #0.326
#p-value
1-pchisq(2*(ll.proposed-ll.null),df=(length(modelA$coefficients)-1))

#model diagonois
plot(rstandard(modelB)~modelB$fitted.values)
qqnorm(y=qresid(modelB),las=1)
qqline(qresid(modelB))
library(statmod)
plot(cooks.distance(modelB))
cooksd=as.vector(cooks.distance(modelB))
abline(h = 8/nrow(exam.MZ.cleanMP), col="red")  # add cutoff line

#regressing after removing high impact points
exam.MZ.cleanMP$numid = rownames(exam.MZ.cleanMP)
exam.MZ.cleanMP.no = filter(exam.MZ.cleanMP,numid %in% as.vector(which(cooksd <= 8/nrow(exam.MZ.cleanMP))))
modelB = glm(IMT ~ sex + age + smoking + bmi + scoreMP + age*scoreMP + sex*scoreMP, data = exam.MZ.cleanMP.no,
             family = inverse.gaussian(link="identity"),start=coef(modelA))
summary(modelB) 


#model fit 
fitdata=cbind.data.frame(exam.MZ.cleanMP.no$numid,exam.MZ.cleanMP.no$IMT,modelB$fitted.values)
plot(fitdata[,2]~fitdata[,3],xlim=c(0.4,1),ylim=c(0.4,1))

#original model
model.scoreMP.lm = geeglm(IMT ~ sex + age + smoking + bmi + scoreMP + scoreMP*age + sex*scoreMP, data = exam.MZ.cleanMP,
                          id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.scoreMP.lm) 

library(car)
avPlots(modelB,layout=c(1,1))



#totaltooth
#原模型
model.NT.lm = geeglm(IMT ~ sex + age + smoking + bmi + NT + NT*sex + smoking*NT, data = exam.MZ,
                     id = twinid, family = gaussian, corstr = "exchangeable")
summary(model.NT.lm)



exam.MZ.NTnew = read.csv("C:/Users/Administrator/Desktop/b.csv")
exam.MZ.NTnew =  filter(exam.MZ.NTnew,twinid!=50)
nt.others = lm(NT~age+sex.F+bmi+smoking.Former+smoking.Current,data=exam.MZ.NTnew)
plot(exam.MZ.NTnew$resid_ont~exam.MZ.NTnew$NT)


#dummy variable
dmy<-dummyVars(~sex+smoking,data=exam.MZ)
trfs<-data.frame(predict(dmy,newdata=exam.MZ))
exam.MZ=cbind(exam.MZ,trfs)
write.csv(exam.MZ,"C:/Users/Administrator/Desktop/b.csv")

