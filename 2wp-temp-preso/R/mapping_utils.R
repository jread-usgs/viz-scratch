plot_site_map <- function(filepath, states_spatial, discrete_sites_spatial, discrete_sites_meta, ...){
  
  
  
  floor_n_groups <- floor(length(unique(discrete_sites_meta$OrganizationFormalName))/25)*25
  
  
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
  
  num_sites <- nrow(discrete_sites_meta) + new_sites
  
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

plot_uv_map <- function(filepath, states_spatial, nwis_sites_spatial = nwis_sites, norwest_sites, noreast_sites, ecosheds_sites){
  png(filename = filepath, height=9, width = 16, res=200, units='in')
  par(mar=c(0,0,0,0), omi = c(0,0,0,0), bg = NA)
  plot(states_spatial, col='#E0E0E0D3', border=NA, expandBB=c(0,0,0.2,0.30), xaxs = 'i', yaxs = 'i')
  usr <- par()$usr
  x0 <- usr[1]
  x1 <- usr[2]
  y0 <- usr[3]
  y1 <- usr[4]
  
  plot(states_spatial[!names(states_spatial) %in% c("alaska","hawaii","puerto")], add=TRUE, col='#E0E0E0D3', border='white', lwd=2)
  
  
  nw_cnt <- 0
  for (j in 1:length(norwest_sites)){
    points(norwest_sites[[j]], pch=16, cex=0.3, col='#d95f02')
    nw_cnt = nw_cnt + length(norwest_sites[[j]])
  }
  ne_cnt <- 0
  for (j in 1:length(noreast_sites)){
    points(noreast_sites[[j]], pch=16, cex=0.3, col='#7570b3')
    ne_cnt = ne_cnt + length(noreast_sites[[j]])
  }
  
  points(ecosheds_sites, pch=16, cex=0.3, col='#e7298a')
  
  points(nwis_sites_spatial, pch=16, cex=0.3, col='#66a61e')
  
  points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.503, pch=16, cex=1.5, col='#66a61e')
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.5, sprintf('NWIS sites (n = %s)', prettyNum(nrow(nwis_sites_spatial), big.mark=",")), cex = 2, pos = 4)
  points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.453, pch=16, cex=1.5, col='#d95f02')
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.45, sprintf('NorWeST (n = %s)', prettyNum(nw_cnt, big.mark=",")), cex = 2, pos = 4)
  points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.403, pch=16, cex=1.5, col='#7570b3')
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.40, sprintf('NorEaST (n = %s)', prettyNum(ne_cnt, big.mark=",")), cex = 2, pos = 4)
  points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.353, pch=16, cex=1.5, col='#e7298a')
  text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.35, sprintf('EcoSheds (n = %s)', prettyNum(length(ecosheds_sites), big.mark=",")), cex = 2, pos = 4)
  
  dev.off()
}

plot_all_map <- function(filepath, states_spatial, nwis_sites_spatial = nwis_sites, norwest_sites, noreast_sites, ecosheds_sites, discrete_sites_spatial){
  png(filename = filepath, height=9, width = 16, res=200, units='in')
  par(mar=c(0,0,0,0), omi = c(0,0,0,0), bg = NA)
  plot(states_spatial, col='#E0E0E0D3', border=NA, expandBB=c(0,0,0.2,0.30), xaxs = 'i', yaxs = 'i')
  usr <- par()$usr
  x0 <- usr[1]
  x1 <- usr[2]
  y0 <- usr[3]
  y1 <- usr[4]
  
  plot(states_spatial[!names(states_spatial) %in% c("alaska","hawaii","puerto")], add=TRUE, col='#E0E0E0D3', border='white', lwd=2)
  
  points(discrete_sites_spatial,pch=16, cex=0.3, col = '#58A4B0C8')
  
  nw_cnt <- 0
  for (j in 1:length(norwest_sites)){
    points(norwest_sites[[j]], pch=16, cex=0.3, col='#66a61e')
    nw_cnt = nw_cnt + length(norwest_sites[[j]])
  }
  ne_cnt <- 0
  for (j in 1:length(noreast_sites)){
    points(noreast_sites[[j]], pch=16, cex=0.3, col='#7570b3')
    ne_cnt = ne_cnt + length(noreast_sites[[j]])
  }
  
  points(ecosheds_sites, pch=16, cex=0.3, col='#e7298a')
  
  points(nwis_sites_spatial, pch=16, cex=0.3, col='#d95f02')
  
  # 
  # points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.503, pch=16, cex=1.5, col='#d95f02')
  # text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.5, sprintf('NWIS site (n = %s)', prettyNum(nrow(nwis_sites_spatial), big.mark=",")), cex = 2, pos = 4)
  # points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.453, pch=16, cex=1.5, col='#66a61e')
  # text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.45, sprintf('NorWeST (n = %s)', prettyNum(nw_cnt, big.mark=",")), cex = 2, pos = 4)
  # points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.403, pch=16, cex=1.5, col='#7570b3')
  # text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.40, sprintf('NorEaST (n = %s)', prettyNum(ne_cnt, big.mark=",")), cex = 2, pos = 4)
  # points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.353, pch=16, cex=1.5, col='#e7298a')
  # text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.35, sprintf('EcoSheds (n = %s)', prettyNum(length(ecosheds_sites), big.mark=",")), cex = 2, pos = 4)
  # points(x0 + (x1 - x0)*0.745, y0 + (y1 - y0)*0.303, pch=16, cex=1.5, col='#58A4B0C8')
  # text(x0 + (x1 - x0)*0.75, y0 + (y1 - y0)*0.3, sprintf('WQPortal (n = %s)', prettyNum(nrow(discrete_sites_spatial), big.mark=",")), cex = 2, pos = 4)
  
  dev.off()
}