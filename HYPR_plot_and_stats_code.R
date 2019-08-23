



library(ggplot2)
library(ggpubr)
library(devtools)
library(gridExtra)

#if you are trying to re-execute the exact plot, you may need to reorder factor levels after importing 
#the cell_sizes_for_plotting dataset
a<- ggplot(cell_sizes_for_plotting, aes(x=variable, y=cell_area, fill=Expected_ploidy))+
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x=element_text(angle=45, hjust=1))+
  xlab("")+
  ylab(bquote('Cell area ('*mu~m^2~')'))+
  labs(fill="Expected ploidy")


#pearson correlation test (repeated for all regressions)
cor_cellarea_cellsize<- cor.test(x=mean_cell_areas_and_expected_genome_sizes$sppider_size, y=mean_cell_areas_and_expected_genome_sizes$mean_area, method=c("pearson"))

#linear model fit (repeated for all regressions)
fit_cellarea_cellsize<- lm(mean_cell_areas_and_expected_genome_sizes$mean_area~mean_cell_areas_and_expected_genome_sizes$sppider_size)

#ggscatter from ggpubr - can't seem to do superscript in ggscatter so I'll do it in Illustrator
b<- ggscatter(data=mean_cell_areas_and_expected_genome_sizes, x="sppider_size", y="mean_area", 
          col="red", size=2, add="reg.line",add.params = list(color = "blue", fill = "gray"),
          cor.coeff.args = list(method = "pearson", label.x = 30, label.y=85, label.sep = "\n"), conf.int=TRUE, cor.coef=TRUE, cor.method="pearson",
          xlab="Sequencing-estimated genome size (MB)", ylab="placeholder")

cor_flourescence_genomesize<- cor.test(x=flow_cytometry_dataset$sppider_size, y=flow_cytometry_dataset$G1_mean)

fit_flourescence_genomesize<- lm(flow_cytometry_dataset$G1_mean~flow_cytometry_dataset$sppider_size)

c<- ggscatter(data=flow_cytometry_dataset, x="sppider_size", y="G1_mean", 
          col="red", size=2, add="reg.line",add.params = list(color = "blue", fill = "gray"),
          cor.coeff.args = list(method = "pearson", label.x = 30, label.y=41000, label.sep = "\n"), conf.int=TRUE, cor.coef=TRUE, cor.method="pearson",
          xlab="Sequencing-estimated genome size (MB)", ylab="Mean BL1-A")


cor_flourescence_expgenomesize<- cor.test(x=flow_cytometry_dataset$Expected_genome_size, y=flow_cytometry_dataset$G1_mean)

fit_flourescence_expgenomesize<- lm(flow_cytometry_dataset$G1_mean~flow_cytometry_dataset$Expected_genome_size)

d<- ggscatter(data=flow_cytometry_dataset, x="Expected_genome_size", y="G1_mean",
              col="red", size=2, add="reg.line",add.params = list(color = "blue", fill = "gray"),
              cor.coeff.args = list(method = "pearson", label.sep = "\n", label.x=30, label.y=46000), conf.int=TRUE, cor.coef=TRUE, cor.method="pearson",
              xlab="Euploid genome size (MB)", ylab="Mean BL1-A")

grid.arrange(a, b, d, c, nrow=2)
