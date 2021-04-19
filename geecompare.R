#scoremp的几个GEE模型比较
exam.MZ.MPnew=read.csv("C:/Users/Administrator/Desktop/a.csv")

#deviance residual
#igaussian-identity-exch
exam.MZ.MPnew$dresid_ig = sign(exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_ig)*
  sqrt((exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_ig)^2/(exam.MZ.MPnew$IMT*exam.MZ.MPnew$fitted_ig^2)) 
#igaussian-identity-ind
exam.MZ.MPnew$dresid_ig2 = sign(exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_ig2)*
  sqrt((exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_ig2)^2/(exam.MZ.MPnew$IMT*exam.MZ.MPnew$fitted_ig2^2)) 
#igaussian-inverse-exch
exam.MZ.MPnew$dresid = sign(exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted)*
  sqrt((exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted)^2/(exam.MZ.MPnew$IMT*exam.MZ.MPnew$fitted^2))
#igaussian-inverse-ind
exam.MZ.MPnew$dresid2 = sign(exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted2)*
  sqrt((exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted2)^2/(exam.MZ.MPnew$IMT*exam.MZ.MPnew$fitted2^2))
#igaussian-log-exch
exam.MZ.MPnew$dresid_log = sign(exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_log)*
  sqrt((exam.MZ.MPnew$IMT-exam.MZ.MPnew$fitted_log)^2/(exam.MZ.MPnew$IMT*exam.MZ.MPnew$fitted_log^2))

exam.MZ.MPnew.ed = filter(exam.MZ.MPnew,twinid!=50)


#predicted effect
#igaussian-1/mu^2
plot(IMT~fitted,data=exam.MZ.MPnew.ed) #exch 
plot(IMT~fitted2,data=exam.MZ.MPnew.ed) #ind 好
#igaussian-identity
plot(IMT~fitted_ig,data=exam.MZ.MPnew.ed)#exch 好
plot(IMT~fitted_ig2,data=exam.MZ.MPnew.ed)#ind 
#gaussian-identity
plot(IMT~fitted_gau,data=exam.MZ.MPnew.ed)#exch
plot(IMT~fitted_gau2,data=exam.MZ.MPnew.ed) #ind
#igaussian-log
plot(IMT~fitted_log,data=exam.MZ.MPnew.ed)#exch
plot(IMT~fitted_log2,data=exam.MZ.MPnew.ed)#ind

#R square
attach(exam.MZ.MPnew)
n=nrow(exam.MZ.MPnew)
k=8
#gaussian
Rsquare_gaus = 1 - sum((IMT - fitted_gau)^2) / sum((IMT - mean(IMT))^2)
Rsquare_gaus #0.240
Rsquare_gaus2 = 1 - sum((IMT - fitted_gau2)^2) / sum((IMT - mean(IMT))^2)
Rsquare_gaus2 #0.246
aRsquare_gaus = 1 - (1/(n-k-1))*sum((IMT - fitted_gau)^2) / ((1/(n-1))*sum((IMT - mean(IMT))^2))
aRsquare_gaus #0.203
aRsquare_gaus2 = 1 - (1/(n-k-1))*sum((IMT - fitted_gau2)^2) / ((1/(n-1))*sum((IMT - mean(IMT))^2))
aRsquare_gaus2 #0.210

#igaussian-1/mu^2
Rsquare = 1 - sum((IMT-fitted)^2/(IMT*fitted^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare  #0.313
Rsquare2 = 1 - sum((IMT-fitted2)^2/(IMT*fitted2^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare2  #0.326
aRsquare = 1 - (1/(n-k-1))*sum((IMT-fitted)^2/(IMT*fitted^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare  #0.279
aRsquare2 = 1 - (1/(n-k-1))*sum((IMT-fitted2)^2/(IMT*fitted2^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare2  #0.294

#igaussian-identity
Rsquare_ig = 1 - sum((IMT-fitted_ig)^2/(IMT*fitted_ig^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_ig #0.314
Rsquare_ig2 = 1 - sum((IMT-fitted_ig2)^2/(IMT*fitted_ig2^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_ig2 #0.338
aRsquare_ig = 1 - (1/(n-k-1))*sum((IMT-fitted_ig)^2/(IMT*fitted_ig^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_ig #0.281
aRsquare_ig2 = 1 - (1/(n-k-1))*sum((IMT-fitted_ig2)^2/(IMT*fitted_ig2^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_ig2 #0.306

#igaussian-log
Rsquare_log = 1 - sum((IMT-fitted_log)^2/(IMT*fitted_log^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_log #0.322
Rsquare_log2 = 1 - sum((IMT-fitted_log2)^2/(IMT*fitted_log2^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_log2 #0.336
aRsquare_log = 1 - (1/(n-k-1))*sum((IMT-fitted_log)^2/(IMT*fitted_log^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_log #0.289
aRsquare_log2 = 1 - (1/(n-k-1))*sum((IMT-fitted_log2)^2/(IMT*fitted_log2^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_log2 #0.304
detach(exam.MZ.MPnew)

#examize link function of inverse
z=exam.MZ.MPnew.ed$lmpred+(exam.MZ.MPnew.ed$IMT-exam.MZ.MPnew.ed$fitted_log)*(-2)/exam.MZ.MPnew.ed$fitted_log^3
plot(z ~ lmpred,data=exam.MZ.MPnew.ed,xlim=c(1,5),ylim=c(1,5))

#residual plot
#model igaussian-identity
plot(exam.MZ.MPnew.ed$fitted_ig,exam.MZ.MPnew.ed$resid_ig)
plot(exam.MZ.MPnew.ed$fitted_ig,exam.MZ.MPnew.ed$presid_ig)
plot(exam.MZ.MPnew.ed$fitted_ig,exam.MZ.MPnew.ed$dresid_ig)
plot(exam.MZ.MPnew.ed$fitted_ig2,exam.MZ.MPnew.ed$dresid_ig2) #ind
hist(exam.MZ.MPnew.ed$presid_ig2)
#model igaussian-1/mu^2
plot(exam.MZ.MPnew.ed$fitted,exam.MZ.MPnew.ed$resid)
plot(exam.MZ.MPnew.ed$fitted,exam.MZ.MPnew.ed$presid)
plot(exam.MZ.MPnew.ed$fitted,exam.MZ.MPnew.ed$dresid)
plot(exam.MZ.MPnew.ed$lmpred,exam.MZ.MPnew.ed$dresid)
plot(exam.MZ.MPnew.ed$fitted2,exam.MZ.MPnew.ed$dresid2) #ind
hist(exam.MZ.MPnew.ed$presid)

#residual randomness test
library(randtests)
runs.test(exam.MZ.MPnew.ed$dresid_ig)


#choose model igaussian-identity-ind


#predicted effect--fancy plot
ggplot(exam.MZ.MPnew.ed) + geom_point(aes(x=fitted_ig,y=IMT)) + geom_abline(colour="blue",size=1) +
  labs(x="IMT拟合值",y="IMT实际值")
