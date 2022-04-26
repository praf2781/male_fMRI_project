#bargraph for mon and bilinguals (TUT-CON contrast at 90dph)
library(ggplot2)
bold=read.csv(file.choose(),header=TRUE)
bold.df=as.data.frame(bold)
bold.df$Group=factor(bold.df$Group,levels=c("Control","Sequential"))
bold.df$Hemisphere=factor(bold.df$Hemisphere,levels=c("Left","Right"))
ggplot(bold.df,aes(Group,Mean,fill=Hemisphere))+geom_bar(aes(x=Group,y=Mean,fill=Hemisphere),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Group,y=Mean,fill=Hemisphere),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C"))+geom_errorbar(aes(ymin=Mean,ymax=Mean+SEM),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-0.5,5),breaks=seq(-0.5, 5,1))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())
# bargraph for good and poor learners use file LEft_Right_T2_T1_rest_55dph_Mean_SD.csv
bold=read.csv(file.choose(),header=TRUE)
bold.df=as.data.frame(bold)
bold.df$Tutor=factor(bold.df$Tutor,levels=c("T2","T1"))
bold.df$Learner_type=factor(bold.df$Learner,levels=c("Good","Bad"))
bold.df$Hemisphere=factor(bold.df$Hemisphere,levels=c("Left","Right"))
ggplot(bold.df,aes(Hemisphere,BOLD,fill=Learner_type))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner_type),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner_type),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,5),breaks=seq(-1.5, 5,1))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

# bargraph for good and poor learners use file Left_Right_goodvspoor_T1_ T2_vsrest_90dph_Mean_SD.csv
bold1=read.csv(file.choose(),header=TRUE)
bold1.df=as.data.frame(bold1)
bold1.df$Tutor=factor(bold1.df$Tutor,levels=c("T2","T1"))
bold1.df$Learner=factor(bold1.df$Learner,levels=c("Good","Bad"))
bold1.df$Hemisphere=factor(bold1.df$Hemisphere,levels=c("Left","Right"))
ggplot(bold1.df,aes(Tutor,BOLD,fill=Learner))+geom_bar(aes(x=Tutor,y=BOLD,fill=Learner),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Tutor,y=BOLD,fill=Learner),stat="identity",position="dodge", color="black")+facet_wrap(.~Hemisphere,scales="free_x")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,4),breaks=seq(-1.5, 4,0.5))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

#bargraph for T2minusT1 contrast_good_poor for 55 and 90dph dataset use Left_right_T2-T1_good_poor_90 or 55dph csv file
bold=read.csv(file.choose(),header=TRUE)
bold.df=as.data.frame(bold)
bold.df$Learner_type=factor(bold.df$Learner,levels=c("Good","Bad"))
bold.df$Hemisphere=factor(bold.df$Hemisphere,levels=c("Left","Right"))
ggplot(bold.df,aes(Hemisphere,BOLD,fill=Learner_type))+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner_type),stat="identity",position="dodge",color="black")+geom_bar(aes(x=Hemisphere,y=BOLD,fill=Learner_type),stat="identity",position="dodge", color="black")+scale_fill_manual(values = c( "#9E9C9C","#3C3C3C"))+geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(limit=c(-1.5,5),breaks=seq(-1.5, 5,1))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())
