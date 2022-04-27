library(ggplot2)
#graph MLD ROI 90dph using file MLD_estimates_T1_T2_C_Mean_SD_90dph.csv
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Tutor=factor(BOLD.df$Tutor,levels=c("T1","T2","C"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,group=Tutor))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",position="dodge",color="black")+ geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",color="black",show_guide=FALSE, position="dodge")+ geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.0,2),breaks=seq(-1.0, 2,0.5))+ scale_fill_manual(values = c( "#9E9C9C","#3C3C3C","#ECECEC"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

#graph for Right_Left_T2-T1_cluster (midbrain)_sequentialbirds_90dph
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Tutor=factor(BOLD.df$Tutor,levels=c("T1","T2","C"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,group=Tutor))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",position="dodge",color="black")+ geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",color="black",show_guide=FALSE, position="dodge")+ geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.0,2.0),breaks=seq(-1.0, 2.0,0.5))+ scale_fill_manual(values = c( "#9E9C9C","#3C3C3C","#ECECEC"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())


#graph for Right_Left_T2-C_cluster (midbrain)_sequentialbirds_90dph
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Tutor=factor(BOLD.df$Tutor,levels=c("T1","T2","C"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,group=Tutor))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",position="dodge",color="black")+ geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",color="black",show_guide=FALSE, position="dodge")+ geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.0,2.0),breaks=seq(-1.0, 2.0,0.5))+ scale_fill_manual(values = c( "#9E9C9C","#3C3C3C","#ECECEC"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

#graph FieldL_medial 90dph using file fieldL _medial-Right_left_990dph_Mean_SD.csv
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Tutor=factor(BOLD.df$Tutor,levels=c("T1","T2","C"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,group=Tutor))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",position="dodge",color="black")+ geom_bar(aes(x=Hemisphere,y=BOLD,fill=Tutor),stat="identity",color="black",show_guide=FALSE, position="dodge")+ geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.0,2.0),breaks=seq(-1.0, 2.0,0.5))+ scale_fill_manual(values = c( "#9E9C9C","#3C3C3C","#ECECEC"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())


# graph for NCM_NCL_cluster from contrast TUT2-CON in 90 dph bilingual birds
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Group=factor(BOLD.df$Tutor,levels=c("T1","T2","C"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,BOLD,fill=Group))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Group),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Group),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C","#ECECEC"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,1.5),breaks=seq(-1.5, 1.5,0.5))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())



# graph for left_midbrain cluster from contrast TUT-CON in 90 dph monolingual birds
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Group=factor(BOLD.df$Group,levels=c("TUT1","CON"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,Mean,fill=Group))+geom_bar(aes(x=Hemisphere,y=Mean,fill=Group),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=Mean,fill=Group),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#ECECEC"))+geom_errorbar(aes(ymin=Mean,ymax=Mean+SEM),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,1.5),breaks=seq(-1.5,1.5,0.5))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

# graph for MLD TUT_ CON estimates in control birds
BOLD=read.csv(file.choose(),header=TRUE)
BOLD.df=as.data.frame(BOLD)
BOLD.df$Tutor=factor(BOLD.df$Tutor,levels=c("TUT","CON"))
BOLD.df$Hemisphere=factor(BOLD.df$Hemisphere,levels=c("Left","Right"))
ggplot(BOLD.df,aes(Hemisphere,Mean,fill=Tutor))+geom_bar(aes(x=Hemisphere,y=Mean,fill=Tutor),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=Mean,fill=Tutor),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#ECECEC"))+geom_errorbar(aes(ymin=Mean,ymax=Mean+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1,1),breaks=seq(-1,1,0.5))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

