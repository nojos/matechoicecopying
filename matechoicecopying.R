install.packages('ggplot2')
install.packages("lmerTest") 
install.packages('tidyr')
install.packages('brms')
install.packages('dplyr')
install.packages('exactRankTests')
install.packages('bayesm')
install.packages("boot")
install.packages("multcomp")
install.packages("igraph")
install.packages("tidyverse")
install.packages("car")
library(ARTool)
library(car)
library(exactRankTests)
library(lmerTest)
library(dplyr)
library(tidyr)
library(Rcmdr)
library(ggplot2)

library(ggplots)
library(gridExtra)

library(lme4)
library(lmerTest)
library(optimx)
library(boot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(brms)
library(irr)

hfemale_l<-read.table(file="C:\\Users\\nojik\\R\\meanhlf_2.txt",header=TRUE)
hfemale_s<-read.table(file="C:\\Users\\nojik\\R\\meanhsf_2.txt",header=TRUE)
hfemale_t<-read.table(file="C:\\Users\\nojik\\R\\meanhtf_2.txt",header=TRUE)
ofemale_l<-read.table(file="C:\\Users\\nojik\\R\\meanolf_2.txt",header=TRUE)
ofemale_s<-read.table(file="C:\\Users\\nojik\\R\\meanosf_2.txt",header=TRUE  )
ofemale_t<-read.table(file="C:\\Users\\nojik\\R\\meanotf_2.txt",header=TRUE  )
hmale_l<-read.table(file="C:\\Users\\nojik\\R\\meanhlm_2.txt",header=TRUE  )
hmale_s<-read.table(file="C:\\Users\\nojik\\R\\meanhsm_2.txt",header=TRUE  )
hmale_t<-read.table(file="C:\\Users\\nojik\\R\\meanhtm_2.txt",header=TRUE  )
omale_l<-read.table(file="C:\\Users\\nojik\\R\\meanolm_2.txt",header=TRUE  )
omale_s<-read.table(file="C:\\Users\\nojik\\R\\meanosm_2.txt",header=TRUE  )
omale_t<-read.table(file="C:\\Users\\nojik\\R\\meanotm_2.txt",header=TRUE  )

hfemale_lp<-read.table(file="C:\\Users\\nojik\\R\\meanhlpf_2.txt",header=TRUE  )
hfemale_sp<-read.table(file="C:\\Users\\nojik\\R\\meanhspf_2.txt",header=TRUE  )
hfemale_tp<-read.table(file="C:\\Users\\nojik\\R\\meanhtpf_2.txt",header=TRUE  )
ofemale_lp<-read.table(file="C:\\Users\\nojik\\R\\meanolpf_2.txt",header=TRUE  )
ofemale_sp<-read.table(file="C:\\Users\\nojik\\R\\meanospf_2.txt",header=TRUE  )
ofemale_tp<-read.table(file="C:\\Users\\nojik\\R\\meanotpf_2.txt",header=TRUE  )
hmale_lp<-read.table(file="C:\\Users\\nojik\\R\\meanhlpm_2.txt",header=TRUE  )
hmale_sp<-read.table(file="C:\\Users\\nojik\\R\\meanhspm_2.txt",header=TRUE  )
hmale_tp<-read.table(file="C:\\Users\\nojik\\R\\meanhtpm_2.txt",header=TRUE  )
omale_lp<-read.table(file="C:\\Users\\nojik\\R\\meanolpm_2.txt",header=TRUE  )
omale_sp<-read.table(file="C:\\Users\\nojik\\R\\meanospm_2.txt",header=TRUE  )
omale_tp<-read.table(file="C:\\Users\\nojik\\R\\meanotpm_2.txt",header=TRUE  )


  hfl<- tidyr::gather(data=hfemale_l, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_l, key = subjid, value = point, -sample, -subject, -target)
  Data<-rbind(hfl,ofl)
  Data$point<-as.numeric(Data$point)
  
  hfl<- tidyr::gather(data=hfemale_s, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_s, key = subjid, value = point, -sample, -subject, -target)
  Data2<-rbind(hfl,ofl)
  Data2$point<-as.numeric(Data2$point)
  
  hfl<- tidyr::gather(data=hfemale_t, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_t, key = subjid, value = point, -sample, -subject, -target)
  Data3<-rbind(hfl,ofl)
  Data3$point<-as.numeric(Data3$point)
  

  
  Data$cont<-'Long-term'
  Data2$cont<-'Short-term'
  Data3$cont<-'Trustworthiness'
  F<-rbind(Data,Data2,Data3)
  F$Sex<-'Female'
  
  Data$subject2<-Data$subject-0.5;
  Data$target2<-Data$target-0.5;
  Data2$subject2<-Data2$subject-0.5;
  Data2$target2<-Data2$target-0.5;
  Data3$subject2<-Data3$subject-0.5;
  Data3$target2<-Data3$target-0.5;
  #model<- lmer(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data,REML=FALSE)
  #summary(model)

  model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data,
                prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
                chains  = 3, #chainの回数を指定
                iter    = 10000,  #繰り返しの回数を指定
                warmup  = 5000 #ウォームアップの回数を指定
  )
  
  summary(model2)
  
  model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data2,
                prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
                chains  = 3, #chainの回数を指定
                iter    = 10000,  #繰り返しの回数を指定
                warmup  = 5000 #ウォームアップの回数を指定
  )
  
  summary(model2)



model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,
              prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
              chains  = 3, #chainの回数を指定
              iter    = 10000,  #繰り返しの回数を指定
              warmup  = 5000 #ウォームアップの回数を指定
)

summary(model2)

Data11<-subset(Data,subject==1)
model2 <- brm(point~target2+(1|subjid)+(1|sample),data=Data11,
              prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
              chains  = 4, #chainの回数を指定
              iter    = 1000,  #繰り返しの回数を指定
              warmup  = 500 #ウォームアップの回数を指定
)

summary(model2)

hfl<- tidyr::gather(data=hmale_l, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_l, key = subjid, value = point, -sample, -subject, -target)
Data<-rbind(hfl,ofl)
Data$point<-as.numeric(Data$point)

hfl<- tidyr::gather(data=hmale_s, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_s, key = subjid, value = point, -sample, -subject, -target)
Data2<-rbind(hfl,ofl)
Data2$point<-as.numeric(Data2$point)

hfl<- tidyr::gather(data=hmale_t, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_t, key = subjid, value = point, -sample, -subject, -target)
Data3<-rbind(hfl,ofl)
Data3$point<-as.numeric(Data3$point)

summary(aov(point~subject*target+Error(subjid),data=Data2))
model<- lmer(point~subject+target+subject:target+(1|subjid)+(1|sample),data=Data3,REML=FALSE)
summary(model)

Data$cont<-'Long-term'
Data2$cont<-'Short-term'
Data3$cont<-'Trustworthiness'
M<-rbind(Data,Data2,Data3)
M$Sex<-'Male'


Data$subject2<-Data$subject-0.5;
Data$target2<-Data$target-0.5;
Data2$subject2<-Data2$subject-0.5;
Data2$target2<-Data2$target-0.5;
Data3$subject2<-Data3$subject-0.5;
Data3$target2<-Data3$target-0.5;
model<- lmer(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data2,REML=FALSE)
summary(model)




model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,
              prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
              chains  = 3, #chainの回数を指定
              iter    = 10000,  #繰り返しの回数を指定
              warmup  = 5000 #ウォームアップの回数を指定
)

summary(model2)


  hfl<- tidyr::gather(data=hfemale_lp, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_lp, key = subjid, value = point, -sample, -subject, -target)
  Data<-rbind(hfl,ofl)
  Data$point<-as.numeric(Data$point)
  
  hfl<- tidyr::gather(data=hfemale_sp, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_sp, key = subjid, value = point, -sample, -subject, -target)
  Data2<-rbind(hfl,ofl)
  Data2$point<-as.numeric(Data2$point)
  
  hfl<- tidyr::gather(data=hfemale_tp, key = subjid, value = point, -sample, -subject, -target)
  ofl<- tidyr::gather(data=ofemale_tp, key = subjid, value = point, -sample, -subject, -target)
  Data3<-rbind(hfl,ofl)
  Data3$point<-as.numeric(Data3$point)
  
  summary(aov(point~subject*target+Error(subjid),data=Data3))
  model<- lmer(point~subject+target+subject:target+(1|subjid)+(1|sample),data=Data3,REML=FALSE)
  summary(model)
  
  Data$subject2<-Data$subject-0.5;
  Data$target2<-Data$target-0.5;
  Data2$subject2<-Data2$subject-0.5;
  Data2$target2<-Data2$target-0.5;
  Data3$subject2<-Data3$subject-0.5;
  Data3$target2<-Data3$target-0.5;
  model<- lmer(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,REML=FALSE)
  summary(model)
  
  
  
  
  model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,
                prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
                chains  = 3, #chainの回数を指定
                iter    = 10000,  #繰り返しの回数を指定
                warmup  = 5000 #ウォームアップの回数を指定
  )
  
  summary(model2)


copy

hfl<- tidyr::gather(data=hmale_lp, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_lp, key = subjid, value = point, -sample, -subject, -target)
Data<-rbind(hfl,ofl)
Data$point<-as.numeric(Data$point)

hfl<- tidyr::gather(data=hmale_sp, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_sp, key = subjid, value = point, -sample, -subject, -target)
Data2<-rbind(hfl,ofl)
Data2$point<-as.numeric(Data2$point)

hfl<- tidyr::gather(data=hmale_tp, key = subjid, value = point, -sample, -subject, -target)
ofl<- tidyr::gather(data=omale_tp, key = subjid, value = point, -sample, -subject, -target)
Data3<-rbind(hfl,ofl)
Data3$point<-as.numeric(Data3$point)

summary(aov(point~subject*target+Error(subjid),data=Data2))
model<- lmer(point~subject+target+subject:target+(1|subjid)+(1|sample),data=Data3,REML=FALSE)
summary(model)

Data$subject2<-Data$subject-0.5;
Data$target2<-Data$target-0.5;
Data2$subject2<-Data2$subject-0.5;
Data2$target2<-Data2$target-0.5;
Data3$subject2<-Data3$subject-0.5;
Data3$target2<-Data3$target-0.5;
model<- lmer(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,REML=FALSE)
summary(model)




model2 <- brm(point~subject2+target2+subject2:target2+(1|subjid)+(1|sample),data=Data3,
              prior   = NULL, #事前分布を指定。NULLと記述した場合は一様分布
              chains  = 3, #chainの回数を指定
              iter    = 10000,  #繰り返しの回数を指定
              warmup  = 5000 #ウォームアップの回数を指定
)

summary(model2)

meanab<-read.table(file="C:\\Users\\nojik\\R\\Meanab2.txt",header=TRUE  )
copyab<-read.table(file="C:\\Users\\nojik\\R\\Copyab2.txt",header=TRUE  )
levels(meanab$p) <- c("Without lateral views", "With lateral views")
levels(meanab$p)

g<-ggplot(meanab) 
g<-g+scale_linetype(name = "Participants", labels = c("MJT",  "OI"))+scale_x_continuous(breaks=c(0.5,1),labels=c("MJT","OI"),limits = c(0.3, 1.2)) 
g<-g+geom_line(aes(x = Target, y=y,group=Subject,linetype=Subject) )+labs(linetype='Participants')
g<-g+xlab("Target")+ylab("Score")
g<- g+theme_bw()+facet_grid(p~cont)+theme(panel.grid.major = element_blank(),
                                                                  panel.grid.minor = element_blank(),
                                                                  panel.background = element_blank(),
                                                                  text = element_text(family = "serif"),
                                                                  title = element_text(size=9),
                                                                  strip.text.y = element_text(size=9),
                                                                  strip.text.x = element_text(size=9),
                                                                  axis.title.y= element_text(size=9),
                                                                  axis.title.x= element_text(size=11),
                                                                  axis.title= element_text(size=11),
                                                                  legend.text = element_text(size=9), 
                                                                  legend.title = element_text(size=9),
                                                                  axis.text.y = element_text(size=9),
                                                                  axis.text.x = element_text(size=9))
g

ggsave(file="Fig.4.jpg",dpi=1000,width=7.480, height=5.543,plot=g)


levels(copyab$cont)
levels(meanab$p) <- c("Short-term (Female)", "Trustworthiness (Male)")

g<-ggplot(copyab) 
g<-g+scale_linetype(name = "Participants", labels = c("MJT",  "OI"))+scale_x_continuous(breaks=c(0.5,1),labels=c("MJT","OI"),limits = c(0.3, 1.2)) 
g<-g+geom_line(aes(x = Model, y=y,group=Subject,linetype=Subject) )+labs(linetype='Participants')
g<-g+xlab("Target")+ylab("Increase from the first judgements")
g<- g+theme_bw()+facet_grid(~cont)+theme(panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          panel.background = element_blank(),
                                          text = element_text(family = "serif"),
                                          title = element_text(size=9),
                                          strip.text.y = element_text(size=9),
                                          strip.text.x = element_text(size=9),
                                          axis.title.y= element_text(size=9),
                                          axis.title.x= element_text(size=11),
                                          axis.title= element_text(size=11),
                                          legend.text = element_text(size=9), 
                                          legend.title = element_text(size=9),
                                          axis.text.y = element_text(size=9),
                                          axis.text.x = element_text(size=9))
g

ggsave(file="Fig.5.jpg",dpi=1000,width=7.480, height=3.543,plot=g)


