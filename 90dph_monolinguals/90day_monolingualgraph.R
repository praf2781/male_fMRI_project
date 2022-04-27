library(ggplot2)

# graph for left_midbrain cluster from contrast TUT-CON in 90 dph monolingual birds
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Group=factor(BOLD.df$Playback,levels=c("TUT","CON"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,fill=Group))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Group),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Group),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#ECECEC"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,1.5),breaks=seq(-1.5,1.5,0.5))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())
