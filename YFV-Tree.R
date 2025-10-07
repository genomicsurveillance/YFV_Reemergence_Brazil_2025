# Load necessary libraries
library(ggplot2)
library(ape)
library(tidyverse)
library(readxl)
library(ggtree)
library(ggnewscale)
library(ggimage)
library(treeio)
library(ggtreeExtra)

# Load the tree
HAtree <- read.beast('YFV.tree')

# Load metadata
HAmetadata_df <- read_excel('Annotation.xlsx')

# Ensure dates are correctly parsed
HAmetadata_df$date <- as.Date(HAmetadata_df$date, format = "%Y-%m-%d")

p<-ggtree(HAtree, mrsd="2025-03-10", as.Date=TRUE,color='grey80',size=0.3, right  = T) %<+% HAmetadata_df  + theme_tree2() 
p

p2<-p +
  scale_fill_manual(values=c(
    'antiquewhite4', 'dodgerblue3', 'darkseagreen4', 'hotpink3', 
                   'purple3', 'goldenrod2', 'grey30', 'darkorange2', 
                   'coral4', 'indianred', 'white', 'bisque2',
                   'deepskyblue', 'orchid', 'forestgreen', "yellow", "red", "black"  # Added three more colors
  ), name='Sampling locations',na.value="grey90")+
  geom_tippoint(aes(fill=Location),size=3, color='black',shape=21, stroke=0.1) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "24 month") +
  theme(axis.text=element_text(size=5)) +
  ggplot2::ylim(0, 220)+
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p2

### Option_2 ###

tree<-read.tree('new_tree_extracted.nwk')
#Switch between these two metadata depending on what you need to plot (ALL or just REDE)
#metadata_df <- read_excel('Brazil_metadata_lineage_clusters.xlsx') ### All Brazil

metadata_df <- read_excel('Annotation.xlsx') ### Only REDE Genomes
metadata_df$host <- as.character(metadata_df$host)


# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))


p <- ggtree(tree, mrsd= "2025-03-10", right  = T)%<+% metadata_df +
  theme(legend.position = "left")  + 
  theme_tree2() +
  geom_tippoint(aes(fill=Location), shape = 21, size= 2.5) +
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024, 2025))+
  ggplot2::ylim(0, 230)
p

#my_palette2 <- colorRampPalette(c("coral2", "deeppink", "dodgerblue", "chartreuse4", "aquamarine4", "antiquewhite", "deepskyblue4", "deeppink1", "darkred", "darkorchid2", "firebrick2", "hotpink", "mediumorchid", "thistle4", "plum", "darkorange1", "darkgoldenrod1", "chocolate4", "slategray1")) (n=12)

#create heatmap w tree
metaDATA_YFV<- data.frame(H = metadata_df$host)
rownames(metaDATA_YFV) <- metadata_df$name

metaDATA_YFVheat <- gheatmap(p, metaDATA_YFV, offset = 0.01, width=0.15, font.size=3, colnames_position= "top", colnames_angle = 0, colnames_offset_y = 0, hjust = 0) + 
  scale_fill_manual(values= c( "#f6c99c", "#95A5DF", "#EE6A50", "#6F62D7", "#8CD9B3","#458B1F", "#E9E2CD", "#8B3A8F" ,"#920A2B", "#E32E4F", "#DF5FC2", "#927E92", "#C6E2FF", "#f6c99c", "#95A5DF", "#EE6A50", "#6F62D7", "#8CD9B3","#458B1F", "#E9E2CD", "#8B3A8F" ,"#920A2B", "#E32E4F", "#DF5FC2", "#927E92", "#C6E2FF"))
metaDATA_YFVheat
