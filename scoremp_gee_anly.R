exam.MZ.cleanMP = read.csv("C:/Users/Administrator/Desktop/scoremp.csv")

#理解scoreMP和age的交互效应
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄段",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="牙齿咀嚼能力得分",y="IMT水平拟合值")

#理解scoreMP和sex的交互效应
exam.MZ.cleanMP %>% ggplot(aes(x = scoreMP_level, y = IMT_pred, colour = sex)) + geom_boxplot() +
  scale_colour_manual(name="性别",values=c("#FA8072","#7EC0EE"),labels=c("M"="男","F"="女")) + labs(x="牙齿咀嚼能力得分",y="IMT水平拟合值") +
  stat_compare_means(aes(group=sex),label="p.format") + ylim(0.4,0.9)

exam.MZ.cleanMP %>% ggplot(aes(x = sex, y = IMT_pred, colour = scoreMP_level)) + geom_boxplot() +
  scale_colour_manual(name="咀嚼能力得分",values=c("#7EC0EE","#FA8072","#458B00")) + labs(x="性别",y="IMT水平拟合值") +
  stat_compare_means(aes(group=scoreMP_level),label="p.format") + ylim(0.4,0.9) + scale_x_discrete(labels=c("M"="男","F"="女"))

#对交互作用更深入的解读,分性别的线性模型：  男性：
inact.MP.M = read.csv("C:/Users/Administrator/Desktop/inactMPM.csv")

inact.MP.M %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄段",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="咀嚼能力得分",y="IMT水平拟合值")

inact.MP.F = read.csv("C:/Users/Administrator/Desktop/inactMPF.csv")

inact.MP.F %>% ggplot(aes(x = scoreMP, y = IMT_pred, colour = age_group)) + geom_point() + stat_smooth(method = lm,se=F) +
  scale_colour_manual(name="年龄段",values=c("#7EC0EE","#FA8072","#458B00","#8B4726")) + labs(x="咀嚼能力得分",y="IMT水平拟合值")

#=============================================================================================
exam.MZ.NT = read.csv("C:/Users/Administrator/Desktop/totaltooth.csv")

#totaltooth和sex交互
exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_pred, colour = sex)) + geom_point() + stat_smooth(method = lm) + 
  labs(x="牙齿数量",y="IMT水平拟合值") + scale_colour_manual(name="性别",values=c("#FA8072","#7EC0EE"),labels=c("M"="男","F"="女"))

#totaltooth和smoking交互
exam.MZ.NT %>% ggplot(aes(x = totaltooth, y = IMT_pred, colour = smoking)) + geom_point() + stat_smooth(method = lm, se=F) +
  scale_colour_manual(name="吸烟状态",values=c("#7EC0EE","#FA8072","#458B00"),labels=c("从不吸烟","从前吸烟","现在吸烟")) +
  labs(x="牙齿数量",y="IMT水平拟合值")
