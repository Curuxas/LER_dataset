#Figure FO and SO metaanalyses divided by Full-Incomplete-Partial
### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

# require packages
require(readxl);require(data.table); require(ggplot2);library(ggpubr)

# read in the excel sheet for the data
metaresult_group<- readxl::read_xlsx('data/results_for_figure.xlsx',sheet = "FirstO")
metaresult_group <- as.data.table(metaresult_group)

# subset the dataset
mydata <- metaresult_group[`Group` == 'Total']

# make the plot Full LER
p1 <- ggplot(data = mydata,
             aes(x=System,y=mean,shape=MA,fill=MA)) +
  geom_hline(yintercept=1,linetype = "dashed",linewidth=0.3)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.3,size=0.8)+
  geom_point(position=position_dodge(0.7), size=4, stroke = 0.5) +
  scale_shape_manual(values=c("F.O."=21,"S.O."=22))+
  geom_text(aes(x = System, y = ci.ub +0.025, label = n),
            position = position_dodge(width = 0.7),vjust = 0, hjust=0.5,
            size = 4.5, check_overlap = FALSE)+
  scale_x_discrete(limits=(c("O","CC","CR","IC","CL","ST","TC")),
                   labels = (c("O","CC","CR","IC","CL","ST","TC")))+
  
  scale_y_continuous(limits=c(0.7,2.4), breaks = c(0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4))+
  labs(x = "Agricultural system", y = "Total LER",colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.1),
        legend.key = element_rect(fill = "white",linewidth = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_text(size=14, colour = 'black'),
        axis.title.x=element_text(size=14, colour = 'black'),
        axis.text.y = element_text(colour = 'black', size = 14),
        axis.text.x = element_text(colour = 'black', size = 14, hjust = 0.5, vjust = 0.5))
p1

mydata <- metaresult_group[`MA` == 'F.O.']
mydata$Group <- factor(mydata$Group,
                       levels = c("Total", "Co-benefit", "No-benefit"))

# make the plot (a)
p2 <- ggplot(data = mydata,
             aes(x=System,y=mean,shape=Group,fill=Group)) +
  geom_hline(yintercept=1,linetype = "dashed",linewidth=0.3)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.3,size=0.8)+
  geom_point(position=position_dodge(0.7), size=4, stroke = 0.5) +
  scale_shape_manual(values=c("Total"=21, "Co-benefit"=23, "No-benefit"=24))+
  scale_fill_manual(values = c(
    "Total" = "#e37761",       
    "Co-benefit" = "#39b34a", 
    "No-benefit" = "#4c85c4"     
  )) +
  geom_text(aes(x = System, y = ci.ub + 0.05, label = n),
            position = position_dodge(width = 0.7),vjust = 0,
            hjust=0.5, size = 4.5, check_overlap = FALSE)+
  scale_x_discrete(limits=(c("CR","IC","ST","TC","AGF","CL","SP")),
                   labels = (c("CR","IC","ST","TC","AGF","CL","SP")))+
  scale_y_continuous(limits=c(0.5,2.4), breaks = c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4))+
  labs(x = "Agricultural systems", y = "LER F.O." ,colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.1),
        legend.key = element_rect(fill = "white",linewidth = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(size=18, colour = 'black'),
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 18),
        axis.text.x = element_text(colour = 'black', size = 18, hjust = 0.5, vjust = 0.5))


mydata <- metaresult_group[`MA` == 'S.O.']
mydata$Group <- factor(mydata$Group,
                       levels = c("Total", "Co-benefit", "No-benefit"))
# make the plot (b)
p3 <- ggplot(data = mydata,
             aes(x=System,y=mean,shape=Group,fill=Group)) +
  geom_hline(yintercept=1,linetype = "dashed",linewidth=0.3)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.3,size=0.8)+
  geom_point(position=position_dodge(0.7), size=4, stroke = 0.5) +
  scale_shape_manual(values=c("Total"=21, "Co-benefit"=23, "No-benefit"=24))+
  scale_fill_manual(values = c(
    "Total" = "#e37761",       
    "Co-benefit" ="#39b34a", 
    "No-benefit" = "#4c85c4"     
  )) +
  geom_text(aes(x = System, y = ci.ub + 0.05, label = n),
            position = position_dodge(width = 0.7),vjust = 0,
            hjust=0.5, size = 4.5, check_overlap = FALSE)+
  scale_x_discrete(limits=(c("CR","IC","ST","TC", "AGF","CL","SP")),
                   labels = (c("CR","IC","ST","TC", "AGF","CL","SP")))+
  scale_y_continuous(limits=c(0.7,2), breaks = c(0.8,1,1.2,1.4,1.6,1.8,2))+
  labs(x = "Agricultural systems", y = "LER S.O." ,colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.1),
        legend.key = element_rect(fill = "white",linewidth = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_text(size=18, colour = 'black'),
        axis.title.y=element_text(size=18, colour = 'black'),
        #axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 18),
        axis.text.x = element_text(colour = 'black', size = 18, hjust = 0.5, vjust = 0.5))



# combine figures p1 and p2 into plot p
p<-ggarrange(p2, p3, ncol = 1, nrow = 2, align = "v",#common.legend = TRUE,legend = "bottom",
             labels = c("a", "b"), label.x = 0,label.y = c(1,1.05),
             font.label=list(size=22, face = "plain"),hjust = -0.2, vjust = 1)
p
# save the plot
ggsave(plot = p, file = "articles/ncoms23/Figure 2.png",width = 410,height = 270, units = "mm")
