#totaltooth的几个模型比较
exam.MZ.NTnew=read.csv("C:/Users/Administrator/Desktop/b.csv")

#deviance residual
#igaussian-identity-exch
exam.MZ.NTnew$dresid_ig = sign(exam.MZ.NTnew$IMT-exam.MZ.NTnew$fitted_ig)*
  sqrt((exam.MZ.NTnew$IMT-exam.MZ.NTnew$fitted_ig)^2/(exam.MZ.NTnew$IMT*exam.MZ.NTnew$fitted_ig^2)) 
#igaussian-identity-ind
exam.MZ.NTnew$dresid_ig2 = sign(exam.MZ.NTnew$IMT-exam.MZ.NTnew$fitted_ig2)*
  sqrt((exam.MZ.NTnew$IMT-exam.MZ.NTnew$fitted_ig2)^2/(exam.MZ.NTnew$IMT*exam.MZ.NTnew$fitted_ig2^2)) 


exam.MZ.NTnew.ed = filter(exam.MZ.NTnew,twinid!=50)


#predicted effect
#igaussian-identity
plot(IMT~fitted_ig,data=exam.MZ.NTnew.ed)#exch
plot(IMT~fitted_ig2,data=exam.MZ.NTnew.ed)#ind 好
#gaussian-identity
plot(IMT~fitted_gau,data=exam.MZ.NTnew.ed)#exch
plot(IMT~fitted_gau2,data=exam.MZ.NTnew.ed) #ind 好


#R square
attach(exam.MZ.NTnew)
n=nrow(exam.MZ.NTnew)
k=9
#gaussian
Rsquare_gaus = 1 - sum((IMT - fitted_gau)^2) / sum((IMT - mean(IMT))^2)
Rsquare_gaus #0.157
Rsquare_gaus2 = 1 - sum((IMT - fitted_gau2)^2) / sum((IMT - mean(IMT))^2)
Rsquare_gaus2 #0.184
aRsquare_gaus = 1 - (1/(n-k-1))*sum((IMT - fitted_gau)^2) / ((1/(n-1))*sum((IMT - mean(IMT))^2))
aRsquare_gaus #0.113
aRsquare_gaus2 = 1 - (1/(n-k-1))*sum((IMT - fitted_gau2)^2) / ((1/(n-1))*sum((IMT - mean(IMT))^2))
aRsquare_gaus2 #0.141


#igaussian-identity
Rsquare_ig = 1 - sum((IMT-fitted_ig)^2/(IMT*fitted_ig^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_ig #0.218
Rsquare_ig2 = 1 - sum((IMT-fitted_ig2)^2/(IMT*fitted_ig2^2)) / sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2))
Rsquare_ig2 #0.252
aRsquare_ig = 1 - (1/(n-k-1))*sum((IMT-fitted_ig)^2/(IMT*fitted_ig^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_ig #0.177
aRsquare_ig2 = 1 - (1/(n-k-1))*sum((IMT-fitted_ig2)^2/(IMT*fitted_ig2^2)) / ((1/(n-1))*sum((IMT-mean(IMT))^2/(IMT*mean(IMT)^2)))
aRsquare_ig2 #0.213

detach(exam.MZ.NTnew)


#residual plot
attach(exam.MZ.NTnew.ed)
#model igaussian-identity
plot(fitted_ig,resid_ig)
plot(fitted_ig,presid_ig)
plot(fitted_ig,dresid_ig)
plot(fitted_ig2,dresid_ig2) #ind 差
hist(presid)
detach(exam.MZ.NTnew.ed)