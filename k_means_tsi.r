library(rasterVis)
library(raster)
library(data.table)
library(stats)
library(ggplot2)
library(gridExtra)

rm(list=ls())

main_dir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox (IDM)/Malaria Team Folder/projects/map_intervention_impact/tsi_classification")

rotation <- fread(file.path(main_dir, "svd_rotations.csv"))

all_tsi <- fread(file.path(main_dir, "tsi_vals.csv"))

nclust <- 10
palette = "Paired"

for (nclust in 3:10){
  print(paste("finding clusters for k of", nclust))
  k_out <- kmeans(rotation[, 1:2], centers=nclust)
  rotation[, cluster:= k_out$cluster]
  
  print("making map")
  new <- matrix(nrow=1609, ncol=1945)
  new[rotation$id] <- rotation$cluster
  new <- ratify(raster(new))
  map_plot <- levelplot(new, att="ID", col.regions=brewer.pal(nclust, palette), xlab=NULL, ylab=NULL, scales=list(draw=F))
  
  print("finding mean time series")
  all_tsi <- merge(all_tsi, rotation[, list(id, cluster)], by="id", all=T)
  summary_tsi <- all_tsi[, list(tsi=mean(tsi)), by=list(cluster, month)]
  all_tsi[, cluster:=NULL]
  
  lines <- ggplot(summary_tsi, aes(x=month, y=tsi)) +
              geom_line(aes(color=factor(cluster)), size=2) +
              facet_grid(cluster~.) +
              scale_color_brewer(type="qual", palette=palette) +
              scale_x_continuous(breaks=1:12, labels=1:12) +
              theme_minimal() +
              theme(legend.position="none") +
              labs(title="Mean TSI Trend",
                   x="Month",
                   y="TSI")
  
  
  layout <- rbind(c(1,1,2),
                  c(1,1,2))
  
  print("saving")
  png(file.path(main_dir, paste0("results_k_", nclust, ".png")), width=700, height=500)
    grid.arrange(map_plot, lines, layout_matrix=layout)
  graphics.off()

}


