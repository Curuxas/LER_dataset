#Figure Land Use Footrpint influence on LER for Crop-livestock integrated systems
### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

# require packages
require(readxl);require(data.table); require(ggplot2);library(ggpubr)

# read in the excel sheet for the data
metaresult_group<- readxl::read_xlsx('data/results_for_figure.xlsx',sheet = "CL_LUF")
metaresult_group <- as.data.table(metaresult_group)
metaresult_group$LER_type <- factor(
  metaresult_group$LER_type,
  levels = c("Total", "No-benefit")
)

# make plot LUF
p1 <- ggplot(data = metaresult_group,
             aes(x = LUF_type, y = LER, shape = LER_type, fill = LER_type, group = LER_type))+
  geom_hline(yintercept=1,linetype = "dashed",linewidth=0.3)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.3,size=0.8)+
  geom_point(position=position_dodge(0.7), size=4, stroke = 0.5) +
  scale_shape_manual(
    values = c("Total"=21, "Co-benefit"=23, "No-benefit"=22),
    breaks = c("Total", "Co-benefit", "No-benefit")
  )+
  geom_text(aes(x = LUF_type, y = ci.ub +0.025, label = n),
            position = position_dodge(width = 0.7),vjust = 0, hjust=0.5,
            size = 4.5, check_overlap = FALSE)+
  scale_x_discrete(limits=(c("Min","Average","Max")),
                   labels = (c("Minimum","Average","Maximum")))+
  
  scale_y_continuous(limits=c(0.8,2.8), breaks = c(0.8,1,1.25,1.5,1.75,2,2.2,2.4,2.6,2.8))+
  labs(x = "Land use footprint of animal products", y = "LER",colour = 'black')+
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
        axis.title=element_text(size=15, colour = 'black'),
        axis.title.x=element_text(size=15, colour = 'black', vjust = 0.7),
        axis.text.y = element_text(colour = 'black', size = 14),
        axis.text.x = element_text(colour = 'black', size = 14, hjust = 0.5, vjust = 0.7))
p1

# save the plot
#ggsave(plot = p1, file = "articles/ncoms23/Figure 2.png",width = 410,height = 270, units = "mm")