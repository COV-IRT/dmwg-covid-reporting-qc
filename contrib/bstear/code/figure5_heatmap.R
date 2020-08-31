
library(pheatmap)

dat <- read.csv('~/ratios_for_heatmap.csv',row.names = 1, header= TRUE)
df_ <-  data.frame(dat)

colnames(df_) <- c('12-16_CDCmay','12-16_CDCaug', '17-21', '22-26',  '27-31')
rownames(df_)[rownames(df_) == "AAusa"] <- "Unites States"

clr3  <-  RColorBrewer::brewer.pal(11,"PiYG")

out <- pheatmap(df_,
                cellheight=7.5,  color = rev(clr3[3:10]),
                cellwidth=15,
                fontsize_row=6, 
                cluster_rows=FALSE,show_colnames = TRUE,
                cluster_cols = FALSE, gaps_col = 1, gaps_row = 1,
                show_rownames=TRUE, 
                filename='/Users/stearb/desktop/cassie_data/public_trackers/heatmaps/figure5_heatmap.pdf')

