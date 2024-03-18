library(ggplot2)
library(MetBrewer)

# ------ Fig. 1 ------ #
Data.MF1b<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 1b.csv")
ggplot(data=Data.MF1b,aes(x=age,fill=factor(dose),color=factor(dose)))+
geom_histogram(binwidth =43,alpha=0.8,size=0.05)+
scale_fill_manual(breaks = c(1,2,3,4,5),values = c("#CC521D","#E1A929","#74A232","#50B1DD","#086CAD"),labels =c("MCV1", "MCV2", "MCV3", "MCV4", "MCV5"),name="Antibody type")+
scale_color_manual(breaks = c(1,2,3,4,5),values = c("#CC521D","#E1A929","#74A232","#50B1DD","#086CAD"),labels =c("MCV1", "MCV2", "MCV3", "MCV4", "MCV5"),name="Antibody type")+
theme_minimal()+xlab("Age at vaccination (days)")+ylab("No. children")
Data.MF1c<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 1c.csv")
ggplot()+geom_ribbon(data=Data.MF1c,aes(x=x_axis,ymin = lower, ymax = upper,fill=factor(dose)),alpha=0.3)+
geom_line(data=Data.MF1c,aes(x=x_axis,y=mean,color=factor(dose)),size=0.3,alpha=1.0)+
scale_color_manual(breaks = c(0,1,2,3),values = c("grey65","#CC521D","#E1A929","#74A232"),labels=c("mAbs","MCV1","MCV2","MCV3"),name="Antibody type")+
scale_fill_manual(breaks = c(0,1,2,3),values = c("#CCCCCC","#F3C3A5","#F9E4A9","#CFE2B2"),labels=c("mAbs","MCV1","MCV2","MCV3"),name="Antibody type")+
theme_minimal()+xlab("Age (days)")+ylab("M:F concentration ratio")
Data.MF1d<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 1d.csv")
ggplot(data=Data.MF1d)+geom_point(data=Data.MF1d,aes(x=log_titer.st,y= decay_rate,color=factor(group)),size=1.0,size=0.8,alpha=0.3)+
scale_color_manual(breaks = c(0,1,2,3,4),values = c("grey","#CC521D","#E1A929","#74A232","#50B1DD"),labels=c("mAbs","MCV1","MCV2","MCV3","MCV4"),name="Antibody type")+
theme_minimal()+xlab('Starting log-concentration \n (log mIU/ml)')+ylab('Daily antibody decrease \n (log mIU/ml)')

# ------ Fig. 2 ------ #
Data.MF2b_main<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2b_main.csv")
Data.MF2b_sub<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2b_sub.csv")
ggplot(data=Data.MF2b_main)+ 
geom_ribbon(data=Data.MF2b_main,aes(x=time/30,ymin=exp(y_lower), ymax=exp(y_upper),fill=factor(group)),alpha=0.4)+
geom_line(data=Data.MF2b_main,aes(x=time/30,y=exp(y_mean),color=factor(group)),size=0.4,lty=1)+
geom_segment(data=Data.MF2b_sub[Data.MF2b_sub$group==2,],aes(x=0,xend=0,y=exp(bv_mean),yend=exp(total_mean)),color="#CC521D",size=0.8)+
geom_segment(data=Data.MF2b_sub[Data.MF2b_sub$group==3,],aes(x=0,xend=0,y=exp(bv_mean),yend=exp(total_mean)),color="#E1A929",size=0.8)+
geom_segment(data=Data.MF2b_sub[Data.MF2b_sub$group==4,],aes(x=0,xend=0,y=exp(bv_mean),yend=exp(total_mean)),color="#74A232",size=0.8)+
geom_segment(data=Data.MF2b_sub[Data.MF2b_sub$group==5,],aes(x=0,xend=0,y=exp(bv_mean),yend=exp(total_mean)),color="#50B1DD",size=0.8)+
scale_color_manual(breaks = c(1,2,3,4,5),values = c("grey","#CC521D","#E1A929","#74A232","#50B1DD"),labels=c("mAbs","MCV1","MCV2","MCV3","MCV4"),name="Antibody type")+
scale_fill_manual(breaks = c(1,2,3,4,5),values = c("#CCCCCC","#F3C3A5","#F9E4A9","#CFE2B2","#BDE9F9"),labels=c("mAbs","MCV1","MCV2","MCV3","MCV4"),name="Antibody type")+
scale_y_continuous(expand=c(0.03,0),limits = c(0,2500),name="Concentration (mIU/ml)")+
theme_minimal()+xlab('Age (months)')
Data.MF2c<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2c.csv")
ggplot()+geom_point(data=Data.MF2c,aes(x=bV_mean,y=alphaV_mean),size=1.2,stat = "identity")+
theme_minimal()+xlab('Log-concentration\n(pre-MCV1)')+ylab('Log antibody increase\n(MCV1)')
Data.MF2d<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2d.csv")
ggplot()+geom_point(data=Data.MF2d,aes(x=bV_mean,y=alphaV_mean),size=1.2,stat = "identity")+
theme_minimal()+xlab('Log-concentration\n(pre-MCV2)')+ylab('Log antibody increase\n(MCV2)')
Data.MF2e<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2e.csv")
ggplot()+geom_point(data=Data.MF2e,aes(x=bV_mean,y=alphaV_mean),size=1.2,stat = "identity")+
theme_minimal()+xlab('Log-concentration\n(pre-MCV3)')+ylab('Log antibody increase\n(MCV3)')
Data.MF2f<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 2f.csv")
ggplot()+geom_point(data=Data.MF2f,aes(x=bV_mean,y=alphaV_mean),size=1.2,stat = "identity")+
theme_minimal()+xlab('Log-concentration\n(pre-MCV4)')+ylab('Log antibody increase\n(MCV4)')

# ------ Fig. 3 ------ #
Data.MF3A<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 3a.csv")
ggplot(data=Data.MF3A)+ 
geom_ribbon(data=Data.MF3A,aes(x=day/365,ymin=exp(P2_5), ymax=exp(P97_5),fill=factor(group)), alpha=0.1)+
geom_ribbon(data=Data.MF3A,aes(x=day/365,ymin=exp(P25), ymax=exp(P75),fill=factor(group)),alpha=0.3)+
geom_line(data=Data.MF3A,aes(x=day/365,y=exp(mean),color=factor(group)),size=0.2,lty=1)+
scale_color_manual(breaks = c(1,2,3),values = c("#DF0784","#008241","#0000AC"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_fill_manual(breaks = c(1,2,3),values = c("#FFCFE7","#99CDB3","#9999DE"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_y_continuous(trans="log",expand=c(0,0),limits = c(-10,121000),breaks = c(0.1,100,1000,10000,120000),labels = c(0.1,100,1000,10000,120000),name="Concentration (mIU/ml)")+
theme_minimal()+xlab('Age (years)')
Data.MF3B<-read.csv("C:/Users/Administrator/Desktop/Figures/Data-Fig. 3b.csv")#example: single-dose (dose=1); two-dose (dose=2)
ggplot(data=Data.MF3B[Data.MF3B$dose==2,])+
geom_ribbon(data=Data.MF3B[Data.MF3B$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.MF3B[Data.MF3B$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_ribbon(data=Data.MF3B[Data.MF3B$dose==2,],aes(x=day/365,ymin=lower, ymax=upper,fill=factor(group)),alpha=0.4)+
geom_line(data=Data.MF3B[Data.MF3B$dose==2,],aes(x=day/365,y=mean,color=factor(group)),size=0.15,lty=2)+
scale_color_manual(breaks = c(1,2,3),values = c("#DF0784","#008241","#0000AC"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
scale_fill_manual(breaks = c(1,2,3),values = c("#FFCFE7","#99CDB3","#9999DE"),labels=c("6 months","8 months","12 months"),name="Age at MCV1")+
theme_minimal()+ylab('Percent protect')+xlab('Age (years)')

