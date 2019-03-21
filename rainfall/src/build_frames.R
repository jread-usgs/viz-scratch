

# generate drops, but do it based on average number of 
# drops visible per frame (intensity), so we can make that dynamic

# each drop has a t_0, t_end, x, y_end, velocity, and y_0 (calculated from y_end, velocity, and t_end-t_0)
# if t_end is not uniformly distributed, we can have dynamic rain
library(dplyr)
n_frames <- 100

n_drops <- 200
drops_df <- data.frame(t_end = sample(1:n_frames, size = n_drops, replace = TRUE), 
                       velocity = runif(n = n_drops, min = 1, max = 1.4), # distance y per frame
                       x = runif(n = n_drops, min = -108, max = -98),
                       y_end = runif(n = n_drops, min = 32, max = 36), 
                       w = runif(n = n_drops, min = 0.2, max = 0.4),
                       stringsAsFactors = FALSE) %>% 
  mutate(t_0 = t_end - sample(2:5, size = n_drops, replace = TRUE), 
         y_0 = y_end + (t_end - t_0) * velocity) %>% 
  select(t_0, t_end, y_0, y_end, x, w, velocity)

n_drops <- 20
drops_df2 <- data.frame(t_end = sample(1:n_frames, size = n_drops, replace = TRUE), 
                       velocity = runif(n = n_drops, min = 1, max = 1.4), # distance y per frame
                       x = runif(n = n_drops, min = -95, max = -89),
                       y_end = runif(n = n_drops, min = 41, max = 46), 
                       w = runif(n = n_drops, min = 0.2, max = 0.4),
                       stringsAsFactors = FALSE) %>% 
  mutate(t_0 = t_end - sample(2:5, size = n_drops, replace = TRUE), 
         y_0 = y_end + (t_end - t_0) * velocity) %>% 
  select(t_0, t_end, y_0, y_end, x, w, velocity)



tails <- c(rgb(0,0,0,0.2), rgb(0,0,0,0.1), rgb(0,0,0,0.05), rgb(0,0,0,0.03), rgb(0,0,0,0.01))

files <- c()
for (i in 1:60){
  png_frame <- stringr::str_pad(i, 3, pad = "0")
  filename <- sprintf('figures/%s_frame.png', png_frame)
  files <- c(files, filename)
  png(filename = filename,
      width = 4.5, height = 3, units = 'in', res = 100)
  
  par(mai = c(0,0,0,0), omi = c(0,0,0,0))
  maps::map(database = 'usa')
  
  for (j in 1:nrow(drops_df)){
    this_drop <- drops_df[j, ]
    if (i < this_drop$t_end & i >= this_drop$t_0){ # normal drop in motion
      for (t in 1:length(tails)){
        
        rect(xleft = this_drop$x - this_drop$w/2, 
             ybottom = this_drop$y_0 - (i - this_drop$t_0) * this_drop$velocity - this_drop$w/2 + this_drop$w * (t - 1),
             ytop = this_drop$y_0 - (i - this_drop$t_0) * this_drop$velocity + this_drop$w/2 + this_drop$w * (t - 1),
             xright = this_drop$x + this_drop$w/2, 
             col = tails[t], border = NA)
        
      }
    } else if ( i == this_drop$t_end){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.7, col = tails[1L], border = NA)
    } else if ( i == this_drop$t_end + 1){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.5, col = tails[2L], border = NA)
    } else if ( i == this_drop$t_end + 2){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.4, col = tails[3L], border = NA)
    }
  }
  for (j in 1:nrow(drops_df2)){
    this_drop <- drops_df2[j, ]
    if (i < this_drop$t_end & i >= this_drop$t_0){ # normal drop in motion
      for (t in 1:length(tails)){
        
        rect(xleft = this_drop$x - this_drop$w/2, 
             ybottom = this_drop$y_0 - (i - this_drop$t_0) * this_drop$velocity - this_drop$w/2 + this_drop$w * (t - 1),
             ytop = this_drop$y_0 - (i - this_drop$t_0) * this_drop$velocity + this_drop$w/2 + this_drop$w * (t - 1),
             xright = this_drop$x + this_drop$w/2, 
             col = tails[t], border = NA)
        
      }
    } else if ( i == this_drop$t_end){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.7, col = tails[1L], border = NA)
    } else if ( i == this_drop$t_end + 1){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.5, col = tails[2L], border = NA)
    } else if ( i == this_drop$t_end + 2){ # splat 
      rect(xleft = this_drop$x - this_drop$w, 
           ybottom = this_drop$y_end,
           xright = this_drop$x + this_drop$w, 
           ytop = this_drop$y_end + this_drop$w*0.4, col = tails[3L], border = NA)
    }
  }
  dev.off()
  
}

magick_command <- sprintf(
  'convert -delay %d -loop 0 %s %s', 5, paste(files, collapse = ' '), 'gif/rain_test.gif')

system(magick_command)
# 
# n_col <- 8192
# n_row <- 4096 
# snow_depth <- readBin('~/Downloads/SNODAS_unmasked_20190320/zz_ssmv11036tS__T0001TTNATS2019032005HP001.dat', integer(), n=n_row*n_col, size=2, signed=TRUE, endian='big')
# snow_depth[snow_depth <= 0] <- NA
# snow_depth_mat <- matrix(snow_depth, nrow = n_col)
# 
# pallette <- colorRampPalette(c("#FAFBF399", "#F0F8E3CC", "#D4E9CAE6", "#BBE0CE", "#B7DAD0", "#B0CCD7", "#A9B8D7"))#, "#A297C2", "#8F6F9E", "#684A77", "#41234D"))
# par(bg = 'grey60')
# filled.contour(snow_depth_mat[4801:7200, 3500:501], levels = seq(0,1000, by = 100), color.palette = pallette)
