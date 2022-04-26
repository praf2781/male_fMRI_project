library(ggplot2)
#test correleation between learning strenght and BOLD estimates
correlation=read.csv(file.choose(),header=TRUE)
correlation.df=as.data.frame(correlation)
cor(correlation.df$LR_pTH, correlation.df$LearningScore,  method = "pearson", use = "complete.obs")
cor.test(correlation.df$LR_pTH, correlation.df$LearningScore, method=c("pearson"))

cor(correlation.df$Left, correlation.df$T2vsT1,  method = "pearson", use = "complete.obs")
cor.test(correlation.df$T2_T1_left, correlation.df$T2vsT1, method=c("pearson"))
model = lm(correlation.df$LR_pTH ~ correlation.df$Learner_type,
           data = correlation.df)
summary(model)
# with axis
plot(correlation.df$LearningScore,correlation.df$LR_pTH,xlim=c(0,100),ylim= c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=TRUE,lwd=2)+axis(2,pos=0,at=seq(-5,5,2),labels=TRUE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$LR_pTH~correlation.df$LearningScore),lwd=3)
# without axis
plot(correlation.df$T2_song_90,correlation.df$Right,xlim=c(0,100),ylim=c(-5,5),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(factor(correlation.df$Learner))],col="black",bg="black")+axis(1,pos=0,labels=FALSE,tick=FALSE,lwd=2)+axis(2,pos=0,seq(-5,5,2),labels=FALSE,tick=FALSE,lwd=2)
clip(0,100,5,-5)
abline(lm(correlation.df$Right~correlation.df$T2_song_90),lwd=3)

