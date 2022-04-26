library(ggplot2)

#bargraph for T2minusT1 contrast_good_poor for  90dph dataset use Left_right_T2-T1_good_poor_90 or 55dph csv file
bold=read.csv(file.choose(),header=TRUE)
bold.df=as.data.frame(bold)
bold.df$Learner=factor(bold.df$Learner,levels=c("Good","Bad"))
bold.df$Hemisphere=factor(bold.df$Hemisphere,levels=c("Left","Right"))
ggplot(bold.df,aes(Hemisphere,BOLD,fill=Learner))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,5),breaks=seq(-1.5, 5,1))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

#correlation with similarity scores
#test correleation between learning strenght and BOLD estimates
correlation=read.csv(file.choose(),header=TRUE)
correlation.df=as.data.frame(correlation)

cor(correlation.df$Left, correlation.df$T1_Song_90,  method = "pearson", use = "complete.obs")
cor.test(correlation.df$Left, correlation.df$T1_Song_90, method=c("pearson"))
cor(correlation.df$Right, correlation.df$T2_song_90,  method = "pearson", use = "complete.obs")
cor.test(correlation.df$Right, correlation.df$T2_song_90, method=c("pearson"))

model = lm(correlation.df$Right ~ correlation.df$Learner,
           data = correlation.df)
summary(model)
model = lm(correlation.df$Left ~ correlation.df$Learner,
           data = correlation.df)
summary(model)

# with axis
plot(correlation.df$T2_song_90,correlation.df$Left,xlim=c(0,100),ylim= c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=TRUE,lwd=2)+axis(2,pos=0,at=seq(-5,5,2),labels=TRUE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$Left~correlation.df$T2_song_90),lwd=3)
# without axis
plot(correlation.df$T2_song_90,correlation.df$Left,xlim=c(0,100),ylim=c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=FALSE,tick=FALSE,lwd=2)+axis(2,pos=0,seq(-5,5,2),labels=FALSE,tick=FALSE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$Left~correlation.df$T2_song_90),lwd=3)

#correlationwiht T2_song_90 and right BOLDe stimates 
# with axis
plot(correlation.df$T2_song_90,correlation.df$Right,xlim=c(0,100),ylim= c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=TRUE,lwd=2)+axis(2,pos=0,at=seq(-5,5,2),labels=TRUE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$Right~correlation.df$T2_song_90),lwd=3)
# without axis
plot(correlation.df$T2_song_90,correlation.df$Right,xlim=c(0,100),ylim=c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=FALSE,tick=FALSE,lwd=2)+axis(2,pos=0,seq(-5,5,2),labels=FALSE,tick=FALSE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$Right~correlation.df$T2_song_90),lwd=3)

