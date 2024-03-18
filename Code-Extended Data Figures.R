library(ggplot2)
library(MetBrewer)


# ------ Extended Data Fig. 1 ------ #
Data.EDF1<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 1.csv")
col.value<-met.brewer("Hokusai3",n=2, type="discrete")[2]
# plot number of participants over age
ggplot(data=Data.EDF1)+
geom_bar(data=Data.EDF1[Data.EDF1$group==1,],aes(x=age_group,y=persons),fill=col.value[1],color="black",stat = "identity",width = 0.6,size=0.3)+
scale_x_continuous(expand=c(0,0),breaks = c(seq(1,7,1)),labels = c("M","0m","6m","1-2y","3-4y","5-6y","7-11y"))+
theme_minimal() + ylab('Frequency')+ xlab('Age at follow-up visits (months or years)')
# plot age-specific mean concentration by sex
ggplot(data=Data.EDF1)+
geom_point(data=Data.EDF1,aes(x=age_group,y=exp(GMT),group=factor(group),shape=factor(group)),position = position_dodge(width=0.5),fill=col.value[1],color=col.value[1],size=1.5)+
geom_linerange(data=Data.EDF1,aes(x=age_group,ymin=exp(lower),ymax=exp(upper),group=factor(group)),position = position_dodge(width=0.5),color=col.value[1],size=0.3)+
scale_shape_manual(breaks = c("1","2","3"),values=c(16,15,17),labels=c("All","Boys","Girls"),name="")+
scale_x_continuous(expand=c(0,0),breaks = c(seq(1,7,1)),labels = c("M","0m","6m","1-2y","3-4y","5-6y","7-11y"))+
scale_y_continuous(trans="log",expand=c(0,0),limits = c(-10,2900),breaks = c(0.1,10.009,100,1000,2000),labels = c(0.1,"10","100",1000,"2000"),name="Concentration (mIU/ml)")+
theme_minimal() + xlab('Age at follow-up visits (months or years)')

# ------ Extended Data Fig. 2 ------ #
Data.EDF2<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 2.csv")
# plot estimated and observed Abs
ggplot(Data.EDF2, aes(obs,est,col=factor(group)))+ 
geom_point(alpha=0.5)+ 
geom_smooth(aes(obs,est),method = "lm", formula = 'y ~ x', col="green",size=0.5)+
scale_color_manual(breaks = c(1,2,3,4),values = c("#F6A9A3","#B2CD6F","#6FD6D9","#DAB2F9"),labels=c("0 months","4 months","12 months","36 months"),name="Age")+
theme_minimal()+ ylab('Estimated log-concentration')+ xlab('Measured log-concentration')


# ------ Extended Data Fig. 3 ------ #
library(multimode)
Data.EDF3<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 3.csv")
# panel A
xx<-locmodes(Data.EDF3$alpha_est,mod0=2,display=TRUE)
ggplot(data=Data.EDF3)+
geom_density(aes(Data.EDF3$alpha_est),size=0.5)+
annotate("segment",x=xx$locations[2],xend=xx$locations[2],y=0,yend=xx$fvalue[2],color="black",size=0.5,lty=2)+
theme_minimal()+ ylab('Density')+ xlab('Log-concentration increase (MCV1)')
#panel B-D
library(ggbeeswarm)
ggplot(data=Data.EDF3)+
geom_quasirandom(data=Data.EDF3,aes(x=group,y=Tbv_est,color=factor(group)),size=0.8,alpha=0.3,dodge.width = 0.8)+
geom_boxplot(data=Data.EDF3,aes(x=group,y=Tbv_est,color=factor(group)),size=0.5,alpha=0.3)+
scale_color_manual(breaks = c(1,2),values = c("#F6A9A3","#6FD6D9"),labels=c("Non-responder","Responder"),name="Group")+
scale_x_continuous(expand=c(0,0),breaks = c(seq(1,2,1)),labels = c("Non-responder","Responder"))+
theme_minimal()+ylab("Log-concentration (pre-MCV1)")+ xlab('Group')
ggplot(data=Data.EDF3)+
geom_quasirandom(data=Data.EDF3,aes(x=group,y=alpha_est,color=factor(group)),size=0.8,alpha=0.3,dodge.width = 0.8)+
geom_boxplot(data=Data.EDF3,aes(x=group,y=alpha_est,color=factor(group)),size=0.5,alpha=0.3)+
scale_color_manual(breaks = c(1,2),values = c("#F6A9A3","#6FD6D9"),labels=c("Non-responder","Responder"),name="Group")+
scale_x_continuous(expand=c(0,0),breaks = c(seq(1,2,1)),labels = c("Non-responder","Responder"))+
theme_minimal()+ylab("Log-concentration increase (MCV1)")+ xlab('Group')
ggplot(data=Data.EDF3)+
geom_quasirandom(data=Data.EDF3,aes(x=group,y=Tv_est,color=factor(group)),size=0.8,alpha=0.3,dodge.width = 0.8)+
geom_boxplot(data=Data.EDF3,aes(x=group,y=Tv_est,color=factor(group)),size=0.5,alpha=0.3)+
scale_color_manual(breaks = c(1,2),values = c("#F6A9A3","#6FD6D9"),labels=c("Non-responder","Responder"),name="Group")+
scale_x_continuous(expand=c(0,0),breaks = c(seq(1,2,1)),labels = c("Non-responder","Responder"))+
theme_minimal()+ylab("Log-concentration (post-MCV1)")+ xlab('Group')

# ------ Extended Data Fig. 4 ------ #
Data.EDF4<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 4.csv")
bkL<-250
bkU<-280
tbX<-35
col.value<-c("#F6A9A3","#B2CD6F","#6FD6D9")
ggplot()+geom_line(data=Data.EDF4[Data.EDF4$group==1&Data.EDF4$time<=bkL,], aes(time,exp(median)), col=col.value[1],size=0.3)+
geom_line(data=Data.EDF4[Data.EDF4$group==2&Data.EDF4$time<=bkL,], aes(time,exp(median)), col=col.value[2],size=0.3)+
geom_line(data=Data.EDF4[Data.EDF4$group==3&Data.EDF4$time<=bkL,], aes(time,exp(median)), col=col.value[3],size=0.3)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==1&Data.EDF4$time<=bkL,], aes(x=time,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#EDBBA6', alpha=0.2)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==2&Data.EDF4$time<=bkL,], aes(x=time,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#9CD7EC', alpha=0.2)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==3&Data.EDF4$time<=bkL,], aes(x=time,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#C7E2B2', alpha=0.2)+
geom_line(data=Data.EDF4[Data.EDF4$group==1&Data.EDF4$time>=bkU,], aes(time-tbX,exp(median)), col=col.value[1],size=0.3)+
geom_line(data=Data.EDF4[Data.EDF4$group==2&Data.EDF4$time>=bkU,], aes(time-tbX,exp(median)), col=col.value[2],size=0.3)+
geom_line(data=Data.EDF4[Data.EDF4$group==3&Data.EDF4$time>=bkU,], aes(time-tbX,exp(median)), col=col.value[3],size=0.3)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==1&Data.EDF4$time>=bkU,], aes(x=time-tbX,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#EDBBA6', alpha=0.2)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==2&Data.EDF4$time>=bkU,], aes(x=time-tbX,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#9CD7EC', alpha=0.2)+
geom_ribbon(data=Data.EDF4[Data.EDF4$group==3&Data.EDF4$time>=bkU,], aes(x=time-tbX,y=exp(median),ymin=exp(q1),ymax=exp(q3)), fill='#C7E2B2', alpha=0.2)+
scale_y_continuous(trans="log",expand=c(0,0),limits = c(-10,120000),breaks = c(0.1,10.009,100,1000,2000,5000),labels = c(0.1,"10","100",1000,"2000",5000),name="Concentration (mIU/ml)")+
theme_minimal()+xlab("Age at follow-up visits (days)")

# ------ Extended Data Fig. 5 ------ #
Data.EDF5<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 5.csv")
# plot estimated and observed Abs
ggplot(Data.EDF5, aes(obs,est,col=factor(group)))+ 
geom_point(alpha=0.5)+ 
geom_smooth(aes(obs,est),method = "lm", formula = 'y ~ x', col="green",size=0.5)+
scale_color_manual(breaks = c(1,2,3,4),values = c("#F6A9A3","#B2CD6F","#6FD6D9","#DAB2F9"),labels=c("Mothers","0 months","4 months","12 months"),name="Age")+
theme_minimal()+ ylab('Estimated log-concentration')+ xlab('Measured log-concentration')

# ------ Extended Data Fig. 6 ------ #
Data.EDF6<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 6.csv")
# plot estimated and observed Abs
ggplot(Data.EDF6, aes(obs,est,col=factor(group)))+ 
geom_point(alpha=0.5)+ 
geom_smooth(aes(obs,est),method = "lm", formula = 'y ~ x', col="green",size=0.5)+
scale_color_manual(breaks = c(1,2,3,4),values = c("#F6A9A3","#B2CD6F","#6FD6D9","#DAB2F9"),labels=c("0 months","4 months","12 months","36 months"),name="Age")+
theme_minimal()+ ylab('Estimated log-concentration')+ xlab('Measured log-concentration')

# ------ Extended Data Fig. 7 ------ #
Data.EDF7<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 7.csv")
# plot estimated and observed Abs
ggplot(Data.EDF6, aes(obs,est,col=factor(group)))+ 
geom_point(alpha=0.5)+ 
geom_smooth(aes(obs,est),method = "lm", formula = 'y ~ x', col="green",size=0.5)+
scale_color_manual(breaks = c(1,2,3,4),values = c("#F6A9A3","#B2CD6F","#6FD6D9","#DAB2F9"),labels=c("Visit 1","Visit 3","Visit 5","Visit 7"),name="Age")+
theme_minimal()+ ylab('Estimated log-concentration')+ xlab('Measured log-concentration')

# ------ Extended Data Fig. 8 ------ #
Data.EDF8<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 8.csv")
#example: the infant cohort
ggplot(data=Data.EDF8)+ 
geom_point(data=Data.EDF8[Data.EDF8$cohort_group==2,],aes(x=factor(age/30),y=GMT,group=factor(dat_group),color=factor(dat_group)),position = position_dodge(width=0.3),size=1.5)+
geom_linerange(data=Data.EDF8[Data.EDF8$cohort_group==2,],aes(x=factor(age/30),ymin=lower,ymax=upper,color=factor(dat_group),group=factor(dat_group)),position = position_dodge(width = 0.3))+
scale_color_manual(breaks = c(1,2),values = c("#F6A9A3","#6FD6D9"),labels=c("Observations","Predictions"),name="Data source")+
theme_minimal()+ ylab('Log-concentration (log mIU/ml)')+ xlab('Age (months)')

# ------ Extended Data Fig. 9 ------ #
# panel A
Data.EDF9A<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 9A.csv")
#example: MCV2 at 15m = 1; MCV2 at 24m = 2 
ggplot(data=Data.EDF9A[Data.EDF9A$MCV2==1,])+ 
geom_ribbon(data=Data.EDF9A[Data.EDF9A$MCV2==1,],aes(x=day/365,ymin=exp(P2_5), ymax=exp(P97_5),fill=factor(group)),alpha=0.1)+
geom_ribbon(data=Data.EDF9A[Data.EDF9A$MCV2==1,],aes(x=day/365,ymin=exp(P25), ymax=exp(P75),fill=factor(group)),alpha=0.3)+
geom_line(data=Data.EDF9A[Data.EDF9A$MCV2==1,],aes(x=day/365,y=exp(mean),color=factor(group)),size=0.2,lty=1)+
scale_color_manual(breaks = c(1,2,3),values = c("#DF0784","#008241","#0000AC"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_fill_manual(breaks = c(1,2,3),values = c("#FFCFE7","#99CDB3","#9999DE"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_y_continuous(trans="log",expand=c(0,0),limits = c(-10,121000),breaks = c(0.1,100,1000,10000,120000),labels = c(0.1,100,1000,10000,120000),name="Concentration (mIU/ml)")+
theme_minimal()+ xlab('Age (years)')
# panel B
Data.EDF9B<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 9B.csv")#example: MCV2 at 15m = 1; MCV2 at 24m = 2 
ggplot(data=Data.EDF9B)+
geom_ribbon(data=Data.EDF9B[Data.EDF9B$MCV2==1,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.EDF9B[Data.EDF9B$MCV2==1,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.EDF9B[Data.EDF9B$MCV2==1,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_line(data=Data.EDF9B[Data.EDF9B$MCV2==1,],aes(x=day/365,y=mean,color=factor(group)),size=0.5,lty=1)+
scale_color_manual(breaks = c(1,2,3),values = c("#DF0784","#008241","#0000AC"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_fill_manual(breaks = c(1,2,3),values = c("#FFCFE7","#99CDB3","#9999DE"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
theme_minimal()+ ylab('Percent protect')+ xlab('Age (years)')

# ------ Extended Data Fig. 10 ------ #
Data.EDF10<-read.csv("C:/Users/Administrator/Desktop/Extended Data Figures/Data-Extended Data Fig. 10.csv")#example: single-dose (dose = 1); two-dose (dose = 2)
ggplot(data=Data.EDF10[Data.EDF10$dose==2,])+
geom_ribbon(data=Data.EDF10[Data.EDF10$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.EDF10[Data.EDF10$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.EDF10[Data.EDF10$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_line(data=Data.EDF10[Data.EDF10$dose==2,],aes(x=day/365,y=mean,color=factor(group)),size=0.5,lty=1)+
scale_color_manual(breaks = c(1,2,3),values = c("#DF0784","#008241","#0000AC"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_fill_manual(breaks = c(1,2,3),values = c("#FFCFE7","#99CDB3","#9999DE"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
theme_minimal()+ ylab('Percent protect')+ xlab('Age (years)')
