
library(ggplot2)
ggplot(x, aes(x=variable, y=value, fill=Expected_ploidy))+
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("")+
  ylab(bquote('Cell area ('*mu~m^2~')'))+
  labs(fill="Expected ploidy")

ggplot(mean_sizes, aes(x=Expected.genome.size, y=mean_area))+
  geom_point(col="red", size=2)+
  ylab(bquote('Cell area ('*mu~m^2~')'))+
  xlab("Expected genome size (MB)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_smooth(method="lm")

