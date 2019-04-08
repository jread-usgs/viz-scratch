plot_site_map <- function(filepath, states_spatial, sites_spatial, sites_meta, ...){
  
  
  
  floor_n_groups <- floor(length(unique(sites_meta$OrganizationFormalName))/25)*25
  
  
  png(filename = filepath, height=9, width = 16, res=200, units='in')
  par(mar=c(0,0,0,0), omi = c(0,0,0,0), bg = NA)
  plot(states_spatial, col='#E0E0E0D3', border=NA, expandBB=c(0,0,0.2,0.30), xaxs = 'i', yaxs = 'i')
  usr <- par()$usr
  x0 <- usr[1]
  x1 <- usr[2]
  y0 <- usr[3]
  y1 <- usr[4]
  
  plot(states_spatial[!names(states_spatial) %in% c("alaska","hawaii","puerto")], add=TRUE, col='#E0E0E0D3', border='white', lwd=2)
  
  points(sites_spatial, pch=16, cex=0.3, col='#58A4B0C8')
  add_plots <- list(...)
  new_sites <- 0
  for (i in seq_len(length(add_plots))){
    
    point_list <- add_plots[[i]]
      if (!is.null(point_list$coords.x1)){
        points(point_list$coords.x1, point_list$coords.x2, pch=16, cex=0.3, col='#58A4B0C8')
      } else {
        for (j in 1:length(point_list)){
          points(point_list[[j]], pch=16, cex=0.3, col='#58A4B0C8')
          new_sites <- new_sites + length(point_list[[j]])
        }
      }
    
    
  }
  
  num_sites <- nrow(sites_meta) + new_sites
  
  if (num_sites > 100000){
    floor_n_sites <- paste0(floor(num_sites/10000) * 10, 'K')
  } else if (num_sites > 1000){
    floor_n_sites <- paste0(floor(num_sites/1000), 'K')
  } else{
    floor_n_sites <- floor(num_sites/100)*100
  }
  
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.5, sprintf('> %s sites', floor_n_sites), cex = 2, pos = 4)
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.43, sprintf('> %s monitoring groups', floor_n_groups ), cex = 2, pos = 4)
  
  dev.off()
}