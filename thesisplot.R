library(gridExtra)
library(ggplot2)
setwd("C:/Users/Administrator/Desktop/thesis2")

#twindata的协方差可视化

factor=rep(c("基因","共同环境","各异环境"),each=6,times=1)
sex=rep(c("男","女"),each=3,times=3)
oral=rep(c("牙齿数量","咀嚼能力得分","牙周袋平均深度"),each=1,times=6)
var.imt=c(0.00,0.00,0.20,0.01,0.01,0.01,0.29,0.30,0.06,0.49,0.48,0.41,0.53,0.53,0.57,0.10,0.10,0.10)
cov.imtoral=c(0.00,0.00,0.08,0.06,0.05,0.04,-0.18,-0.22,0.14,-0.26,-0.18,0.24,-0.19,-0.04,0.02,-0.02,-0.01,0.03)
var.oral=c(0.00,0.00,0.03,0.25,0.42,0.31,0.42,0.43,0.36,0.14,0.07,0.14,0.30,0.46,0.43,0.32,0.35,0.41)
twindata=cbind.data.frame(factor,sex,oral,var.imt,cov.imtoral,var.oral)
colnames(twindata)=c("factor","sex","oral","varimt","covimtoral","varoral")
twindata$factor=factor(twindata$factor,levels=unique(twindata$factor))


a = ggplot(data=twindata)+ geom_bar(aes(x=factor,y=varimt,fill=oral),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    facet_grid(.~sex) + scale_fill_manual(name="牙齿健康状况",values=c("grey0","grey40","grey80")) + 
    labs(x=NULL,y="Var(IMT)") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

b = ggplot(data=twindata)+ geom_bar(aes(x=factor,y=covimtoral,fill=oral),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    facet_grid(.~sex) + scale_fill_manual(name="牙齿健康状况",values=c("grey0","grey40","grey80")) + 
    labs(x=NULL,y="Cov(IMT,Oral)") + scale_y_continuous(limits=c(-0.3,0.3),breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

c = ggplot(data=twindata)+ geom_bar(aes(x=factor,y=varoral,fill=oral),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    facet_grid(.~sex) + scale_fill_manual(name="牙齿健康状况",values=c("grey0","grey40","grey80")) + 
    labs(x=NULL,y="Var(Oral)") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
twindata.plot = grid.arrange(a,b,c,nrow=3)
ggsave(plot=twindata.plot,width=16,height=21,units="cm",dpi=600,filename = "twindata.png")


#=====================================================================================================
#50岁前后牙齿健康状况对比
exam50 = read.csv("C:/Users/Administrator/Desktop/exam50.csv")
oralhealth50 = select(exam50,c(num,totaltooth,scoreMP,avePD,age50))
nt50 = oralhealth50 %>% ggplot(aes(x = age50, y = totaltooth)) + geom_boxplot() + 
    stat_compare_means(size=3.7) + labs(x = "年龄",y = "牙齿数量") + ylim(0,35) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

mp50 = oralhealth50 %>% ggplot(aes(x = age50, y = scoreMP)) + geom_boxplot() + 
    stat_compare_means(size=3.7) + labs(x = "年龄",y = "牙齿咀嚼能力得分") + ylim(0,9.5) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

pd50 = oralhealth50 %>% ggplot(aes(x = age50, y = avePD)) + geom_boxplot() + 
    stat_compare_means(size=3.7) + labs(x = "年龄",y = "牙周袋平均深度") + ylim(1.6,5.1) +
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
oralhealth50.plot = grid.arrange(nt50,mp50,pd50,nrow=1)
ggsave(plot=oralhealth50.plot,width=16,height=8,units="cm",dpi=600,filename = "oralhealth50.png")

#50岁前后IMT水平对比
exam50.ed = exam50 %>% filter(IMT != 2.61)
IMT50.plot = exam50.ed %>% ggplot(aes(x = IMT, fill = age50)) + geom_density(alpha=0.5) +
    scale_fill_manual(name = "年龄",values = c("gray3","gray60")) + labs(x = "IMT水平", y = "核密度估计") + theme_classic() + 
    theme(legend.position = "top",text=element_text(size=10.5),axis.text = element_text(size = 10.5),legend.text = element_text(size = 10.5))
ggsave(plot=IMT50.plot,width=10,height=8,units="cm",dpi=600,filename = "IMT50.png")

#>50岁同卵双胞胎数据的可视化
exam.MZ = exam50 %>% filter(age >= 50 & zyg == "MZ")
exam.MZ$sex=factor(exam.MZ$sex,levels=c("M","F"))
exam.MZ$smoking=factor(exam.MZ$smoking,levels=c("Never","Former","Current"))
exam.MZ.ed = filter(exam.MZ, IMT != 2.61)

#年龄和咀嚼能力得分
agescoremp.plot = exam.MZ %>% ggplot(aes(x = scoreMP_level, y = age)) + geom_boxplot() +
    stat_compare_means(size=3.7) + labs(x="牙齿咀嚼能力得分",y="年龄") + ylim(50,90) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
ggsave(plot=agescoremp.plot,width=5,height=8,units="cm",dpi=600,filename = "age_scoremp.png")

#性别和IMT,totaltooth
seximt = exam.MZ.ed %>% ggplot(aes(x = sex, y = IMT)) + geom_boxplot() + ylim(0.3,1.5) +
    scale_x_discrete(labels=c("M"="男","F"="女")) + stat_compare_means(size=3.7) + labs(x = "性别", y = "IMT水平") + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
sextooth = exam.MZ %>% ggplot(aes(x = sex, y = totaltooth)) + geom_boxplot() + 
    scale_x_discrete(labels=c("M"="男","F"="女")) + stat_compare_means(size=3.7) +
    labs(x = "性别", y = "牙齿数量") + ylim(0,35) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
seximttooth.plot=grid.arrange(seximt,sextooth,nrow=1)   
ggsave(plot=seximttooth.plot,width=10,height=8,units="cm",dpi=600,filename = "sex_imttotaltooth.png")

#吸烟状态和IMT，牙齿健康关系
smokingimt = exam.MZ.ed %>% ggplot(aes(x=smoking, y=IMT)) + geom_boxplot() + 
    stat_compare_means(size=3.7) + labs(x = "吸烟状态",y="IMT水平") + scale_x_discrete(labels=c("Never"="从不","Former"="过去","Current"="现在")) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + ylim(0.3,1.5) + theme_classic()

smokingnt = exam.MZ %>% ggplot(aes(x=smoking, y=totaltooth)) + geom_boxplot() + 
    stat_compare_means(size=3.7) + labs(x = "吸烟状态",y="牙齿数量") + ylim(0,35) + scale_x_discrete(labels=c("Never"="从不","Former"="过去","Current"="现在")) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

my_comparisons=list(c("Never","Former"),c("Former","Current"),c("Never","Current"))
smokingmp = exam.MZ %>% ggplot(aes(x=smoking, y=scoreMP)) + geom_boxplot() +  
    stat_compare_means(comparisons = my_comparisons,size=3.7) + labs(x = "吸烟状态",y="牙齿咀嚼能力得分") + ylim(0,12) + 
    scale_x_discrete(labels=c("Never"="从不","Former"="过去","Current"="现在")) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
smokingimtoral.plot=grid.arrange(smokingimt,smokingnt,smokingmp,nrow=1)
ggsave(plot=smokingimtoral.plot,width=18,height=8,units="cm",dpi=600,filename = "smoking_imtntmp.png")


#==========================================================================================
#scoremp的GEE模型交互作用分析
exam.MZ.cleanMP = read.csv("C:/Users/Administrator/Desktop/scoremp.csv")
exam.MZ.cleanMP$sex=factor(exam.MZ.cleanMP$sex,levels=c("M","F"))
exam.MZ.cleanMP$smoking=factor(exam.MZ.cleanMP$smoking,levels=c("Never","Former","Current"))

#在性别作用下IMT和scoremp关系
sexlegend = exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP_level, y = IMT_pred, linetype = sex)) + geom_boxplot() +
    scale_linetype_manual(name="性别",values=c(1,2),labels=c("M"="男","F"="女")) + labs(x="牙齿咀嚼能力得分",y="IMT水平拟合值") +
    stat_compare_means(size=3.7,aes(group=sex),label="p.format") + ylim(0.4,0.9) + theme_classic() + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5),legend.position = "top",legend.text = element_text(size = 10.5))

mplegend = exam.MZ.cleanMP %>% ggplot(aes(x = sex, y = IMT_pred, linetype = scoreMP_level)) + geom_boxplot() +
    scale_linetype_manual(name="咀嚼能力",values=c(1,2,3)) + labs(x="性别",y="IMT水平拟合值") +
    stat_compare_means(size=3.7,aes(group=scoreMP_level),label="p.format") + ylim(0.4,0.9) + scale_x_discrete(labels=c("M"="男","F"="女")) + theme_classic() + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5),legend.position = "top",legend.text = element_text(size = 10.5))
imtmp.sex.plot = grid.arrange(sexlegend,mplegend,nrow=1)
ggsave(plot=imtmp.sex.plot,width=18,height=8,units="cm",dpi=600,filename = "scoreMP_IMTpred_sex1.png")

#在年龄作用下IMT和score MP的关系
mpimtpred = exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_pred, shape = age_group)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") +
    scale_shape_manual(name="年龄",values=c(16,3,17,8)) + labs(x="咀嚼能力得分",y="IMT水平拟合值") + guides(shape=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

mpimtprob = exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_prob, shape = age_group)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") +
    scale_shape_manual(name="年龄",values=c(16,3,17,8)) + labs(x="咀嚼能力得分",y="患动脉硬化预测概率") + guides(shape=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
imtmp.age.plot = grid.arrange(mpimtpred,mpimtprob,nrow=1)
ggsave(plot=imtmp.age.plot,width=18,height=8,units="cm",dpi=600,filename = "scoreMP_IMTpred_age1.png")

#======================================================================
#对在性别作用下IMT和scoremp关系的进一步考察--分性别模型
inact.MP.M = read.csv("C:/Users/Administrator/Desktop/inactMPM.csv")
inact.MP.F = read.csv("C:/Users/Administrator/Desktop/inactMPF.csv")

male = inact.MP.M %>% ggplot(aes(x = scoreMP, y = IMT_pred, shape = age_group)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") +
    scale_shape_manual(name="年龄",values=c(16,3,17,8)) + labs(x="咀嚼能力得分（男）",y="IMT水平拟合值") + guides(shape=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

female = inact.MP.F %>% ggplot(aes(x = scoreMP, y = IMT_pred, shape = age_group)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") +
    scale_shape_manual(name="年龄",values=c(16,3,17,8)) + labs(x="咀嚼能力得分（女）",y="IMT水平拟合值") + guides(shape=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
imtmp.disex.plot = grid.arrange(male,female,nrow=1)
ggsave(plot=imtmp.disex.plot,width=18,height=8,units="cm",dpi=600,filename = "scoreMP_IMTpred_agesex.png")

#====================================================================
#NT的IMT01模型
exam.MZ.NT = read.csv("C:/Users/Administrator/Desktop/totaltooth.csv")
exam.MZ.NT$sex=factor(exam.MZ.NT$sex,levels=c("M","F"))
exam.MZ.NT$smoking=factor(exam.MZ.NT$smoking,levels=c("Never","Former","Current"))

#性别作用下的totaltooth和IMT
sexnt = exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_prob, shape = sex)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") + 
    labs(x="牙齿数量",y="患动脉硬化的预测概率") + scale_shape_manual(name="性别",values=c(16,8),labels=c("M"="男","F"="女")) + theme_classic() + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5),legend.position = "top",legend.text = element_text(size = 10.5))
#吸烟状态作用下的totaltooth和IMT
smokingnt = exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_prob, shape = smoking)) + geom_point() + stat_smooth(method = lm,se=F,linetype=2,colour="grey40") +
    scale_shape_manual(name="吸烟状态",values=c(16,3,8),labels=c("Never"="从不","Former"="从前","Current"="现在")) + theme_classic() + 
    labs(x="牙齿数量",y="患动脉硬化的预测概率") + theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5),legend.position = "top",legend.text = element_text(size = 10.5))
ntsexsmoke.plot = grid.arrange(sexnt,smokingnt,nrow=1)
ggsave(plot=ntsexsmoke.plot,width=18,height=8,units="cm",dpi=600,filename = "totaltooth_IMTprob_sexsmoking.png")


#===================================================================
#IMT拟合值和实际值对比图--scoremp的GEE模型
exam.MZ.cleanMP.ed = filter(exam.MZ.cleanMP,IMT!=2.61)
imtpred.plot = exam.MZ.cleanMP.ed %>% ggplot() + geom_point(aes(x=IMT_pred,y=IMT)) + geom_abline(colour="grey40",linetype=2) +
               labs(x="IMT拟合值",y="IMT实际值") + theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
ggsave(plot=imtpred.plot,width=8,height=8,units="cm",dpi=600,filename = "imtpred.png")


#==================================================================
#PPT中单变量ACEage模型协方差解释可视化
factor=rep(c("基因","共同环境","各异环境","年龄"),each=2,times=1)
sex=rep(c("男","女"),each=1,times=4)
IMT=c(0.00,0.00,0.30,0.50,0.53,0.10,0.13,0.38)
NT=c(0.00,0.39,0.43,0.00,0.30,0.32,0.22,0.26)
scoreMP=c(0.00,0.49,0.44,0.00,0.46,0.35,0.06,0.14)
avePD=c(0.00,0.45,0.40,0.00,0.43,0.41,0.12,0.12)
uniacedata=cbind.data.frame(factor,sex,IMT,NT,scoreMP,avePD)
uniacedata$factor=factor(uniacedata$factor,levels=unique(uniacedata$factor))

imt.plot = ggplot(data=uniacedata)+ geom_bar(aes(x=factor,y=IMT,fill=sex),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    scale_fill_manual(name="性别",values=c("grey0","grey80")) + 
    labs(x=NULL,y="IMT值") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()
#ggsave(plot=imt.plot,width=7,height=4.5,units="cm",dpi=600,filename = "uniaceplot_legend.png")


nt.plot = ggplot(data=uniacedata)+ geom_bar(aes(x=factor,y=NT,fill=sex),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    scale_fill_manual(name="性别",values=c("grey0","grey80")) + 
    labs(x=NULL,y="牙齿数量") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

scoremp.plot = ggplot(data=uniacedata)+ geom_bar(aes(x=factor,y=scoreMP,fill=sex),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    scale_fill_manual(name="性别",values=c("grey0","grey80")) + 
    labs(x=NULL,y="咀嚼能力得分") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

avepd.plot = ggplot(data=uniacedata)+ geom_bar(aes(x=factor,y=avePD,fill=sex),stat="identity",width=0.5,position=position_dodge(width=0.6)) + 
    scale_fill_manual(name="性别",values=c("grey0","grey80")) + 
    labs(x=NULL,y="牙周袋平均深度") + scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1)) + guides(fill=F) + 
    theme(text=element_text(size=10.5),axis.text = element_text(size = 10.5)) + theme_classic()

uniace.plot = grid.arrange(imt.plot,nt.plot,scoremp.plot,avepd.plot,nrow=2)
ggsave(plot=uniace.plot,width=14,height=9,units="cm",dpi=600,filename = "uniaceplot.png")
