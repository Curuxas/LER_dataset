### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')
# making plot

require(data.table)
require(ggplot2)
require(patchwork)
library(readxl)
# load file
#d1 <- readxl::read_xlsx('data/260207 SO_metaanalysis.xlsx',sheet='Diff rotcov')
#d1 <- as.data.table(d1)

# load source file
d1 <- readxl::read_xlsx('../LER/data/merged_rotcov_noout_nometa_nocroplivmaxmin.xlsx')
d1 <- as.data.table(d1)

d1$LERt_cv[d1$system == "agroforest"] <- 1.38214452772197

d1$V_LERt[d1$system == "agroforest"] <- 
  d1$LERt[d1$system == "agroforest"] * d1$LERt_cv[d1$system == "agroforest"]

#d1$LERt_cv <- NA
#d1$LERt_cv[d1$system == "agroforest"] <- 1.38214452772197

#d1$V_LERt <- d1$LERt * d1$LERt_cv


# make subset
d2 <- d1[,.(system=div,ler_mean = LERt,ler_sd = sqrt(V_LERt), ler_n = n_c)]
d2$system <- factor(
  d2$system,
  levels = c("agroforest","sparse","alley","ic","mono","cover","rotation","cropliv", "silvopas")
)



# estimate CV and estimate SD when missing
d2[is.na(ler_n), ler_n := 2]
d2[,ler_cv := mean(ler_sd/ler_mean,na.rm=T),by='system']
d2$ler_cv[d2$system == "agroforest"] <- 1.38214452772197
d2[is.na(ler_sd), ler_sd := ler_mean * ler_cv * 1.25]
d2[, ler_se := ler_sd / sqrt(ler_n)]
d2 <- d2[!is.na(ler_mean)]

setorder(d2,system,ler_mean)
d2[,id := 1:.N,by='system']


# make a plot per system
p1 <- ggplot(data=d2,aes(x = id, y = ler_mean,group = system,color = system)) + geom_line() + ylim(0,5) +
  geom_errorbar(aes(ymin = ler_mean - ler_se, ymax = ler_mean + ler_se),width=0.2)+
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999"),
                     labels = c('agroforest','alley','ic','mono','rotation','silvopas','sparse', 'cropliv', 'cover')) + theme_bw() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.5,0.8),
        axis.title = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16),
        axis.text = element_text(size=16))+
  guides(color=guide_legend(ncol=2)) + ylab('mean LER') + xlab('Observation number')
ggsave(plot=p1,filename='products/meanLER_combined.jpg',width = 20,height=16)

# make separate plots
plot_format <- theme(legend.position = c(0.8,0.8),
                     axis.title = element_text(size=12),
                     legend.title = element_text(size=12),
                     legend.text = element_text(size=12),
                     axis.text = element_text(size=12),
                     plot.title = element_text(size=12))
#plot_cols <-  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"),
#                                 labels = c('agroforest','sparse','alley','ic','mono','cover','rotation','cropliv','silvopas'))

plot_cols <-  scale_color_manual(values = c(agroforest = "#000000", sparse="#E69F00", alley="#56B4E9",ic="#999999", mono="#F0E442", cover="#0072B2", rotation="#CC79A7", cropliv="#D55E00", silvopas="#009E73"),
                                 labels = c(
                                   agroforest = "Agroforest",
                                   sparse = "Sparse trees",
                                   alley = "Tree+crop",
                                   ic = "Intercropping",
                                   mono = "Orchards",
                                   cover = "Cover crops",
                                   rotation = "Crop rotation",
                                   cropliv = "Crop-livestock",
                                   silvopas = "Silvopastoralism"
                                   )
                                 )
p3 <- ggplot() +
  # 1?????? Error bars FIRST (background, light)
  geom_errorbar(
    data = d2[system %in% c('sparse','alley')],
    aes(x = id,
        ymin = pmax(0, ler_mean - ler_se),
        ymax = ler_mean + ler_se,
        group = system,
        color = system),
    width = 0.2,
    alpha = 0.25
  ) +
  geom_errorbar(
    data = d2[system == 'agroforest'],
    aes(x = id,
        ymin = pmax(0, ler_mean - ler_se),
        ymax = ler_mean + ler_se,
        group = system,
        color = system),
    width = 0.2,
    alpha = 0.15
  ) +
  
  # 2?????? Lines on top (clear)
  geom_line(
    data = d2[system %in% c('sparse','alley')],
    aes(x = id, y = ler_mean, group = system, color = system),
    linewidth = 0.7
  ) +
  geom_line(
    data = d2[system == 'agroforest'],
    aes(x = id, y = ler_mean, group = system, color = system),
    linewidth = 0.9
  ) +
  
  # styling
  plot_cols +
  coord_cartesian(ylim = c(0, 5)) +
  theme_bw() +
  plot_format +
  ylab('LER') +
  xlab('Observation number') +
  ggtitle('Agroforestry systems')



p2 <- ggplot(data=d2[system %in% c('ic')],aes(x = id, y = ler_mean,group = system,color = system)) +
      geom_line() +
      geom_errorbar(aes(ymin = pmax(0,ler_mean - ler_se), ymax = ler_mean + ler_se),width=0.2)+
      coord_cartesian(ylim = c(0, 5)) +
      plot_cols + theme_bw() + plot_format +
      ylab('LER') + xlab('Observation number') + ggtitle('Intercropping')
p1 <- ggplot(data=d2[system %in% c('mono','cover', 'rotation')],aes(x = id, y = ler_mean,group = system,color = system)) +
      geom_line() + 
      geom_errorbar(aes(ymin = pmax(0,ler_mean - ler_se), ymax = ler_mean + ler_se),width=0.2)+
      coord_cartesian(ylim = c(0, 5)) +
      plot_cols + theme_bw() + plot_format +
      ylab('LER') + xlab('Observation number') + ggtitle('Orchards and rotations with cover crops and other harvested crops')
p4 <- ggplot(data=d2[system %in% c('cropliv','silvopas')],aes(x = id, y = ler_mean,group = system,color = system)) +
      geom_line() + 
      geom_errorbar(aes(ymin = pmax(0,ler_mean - ler_se), ymax = ler_mean + ler_se),width=0.2)+
      coord_cartesian(ylim = c(0, 5)) +
      plot_cols + theme_bw() + plot_format +
      ylab('LER') + xlab('Observation number') + ggtitle('Crop-livestock integrated and silvopastoral systems')
p5 <- p1 + p2 + p3 + p4
ggsave(plot=p5,filename='products/meanLER_combined_patchwork.jpg',width = 20,height=16)








p1 <- ggplot() +
  # First: dense series (drawn underneath)
  geom_line(
    data = d2[system %in% c('sparse', 'alley')],
    aes(x = id, y = ler_mean, group = system, color = system),
    linewidth = 0.7,
    alpha = 0.5
  ) +
  geom_errorbar(
    data = d2[system %in% c('sparse', 'alley')],
    aes(x = id, ymin = pmax(0, ler_mean - ler_se), ymax = ler_mean + ler_se, color = system),
    width = 0.2,
    alpha = 0.5
  ) +
  
  # Then: sparse series (drawn ON TOP)
  geom_line(
    data = d2[system == 'agroforest'],
    aes(x = id, y = ler_mean, group = system, color = system),
    linewidth = 1.2
  ) +
  geom_errorbar(
    data = d2[system == 'agroforest'],
    aes(x = id, ymin = pmax(0, ler_mean - ler_se), ymax = ler_mean + ler_se, color = system),
    width = 0.2
  ) +
  
  # Scales and styling
  plot_cols +
  coord_cartesian(ylim = c(0, 5)) +
  theme_bw() +
  plot_format +
  ylab('LER') +
  xlab('Observation number') +
  ggtitle('Agicultural systems')



p1 <- ggplot(data=d2[system %in% c('agroforest','sparse', 'alley')],aes(x = id, y = ler_mean,group = system,color = system)) +
  geom_line() + 
  geom_errorbar(aes(ymin = pmax(0,ler_mean - ler_se), ymax = ler_mean + ler_se),width=0.2)+
  coord_cartesian(ylim = c(0, 5)) +
  plot_cols + theme_bw() + plot_format +
  ylab('LER') + xlab('Observation number') + ggtitle('Agroforestry systems')
