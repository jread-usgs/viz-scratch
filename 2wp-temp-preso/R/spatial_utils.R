get_region <- function(xs){
  regions <- xs
  for (i in 1:length(xs)){
    twod <- substr(xs[i],1,2)
    if (twod == '19'){
      region = 'AK'
    } else if (twod == '20'){
      region = 'HI'
    } else if (twod == '21'){
      region = 'PR'
    } else {
      region = 'US'
    }
    regions[i] <- region
  }
  
  return(regions)
}

get_shifted_states <- function(){
  
  library(sp)
  library(dplyr)
  conus <- to_sp('state')
  HI <- to_sp("world", "USA:hawaii")
  AK <- to_sp("world", "USA:alaska")
  PR <- to_sp("world", "Puerto Rico")
  
  # thanks to Bob Rudis (hrbrmstr):
  # https://github.com/hrbrmstr/rd3albers
  
  # -- if moving any more states, do it here: --
  alaska <- shift_sp(AK, 0.6, shift = c(80,-470), rotate=-50, proj.string = proj4string(conus), row.names = 'alaska') 
  hawaii <- shift_sp(HI, 1, shift=c(520, -110), rotate=-35, proj.string = proj4string(conus), row.names = 'hawaii') 
  puerto <- shift_sp(PR, 2.5, shift = c(-140, 90), rotate=20, proj.string = proj4string(conus), row.names = 'puerto')
  
  return(rbind(puerto, conus, alaska, hawaii, makeUniqueIDs = TRUE))
}


proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

shift_sp <- function(sp, scale, shift, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}

read_ecosheds <- function(file, proj_string){
  sheds_sites <- read_csv(file)
  
  coords <- matrix(c(sheds_sites$longitude, sheds_sites$latitude), ncol = 2, byrow = FALSE)
  SpatialPoints(coords, proj4string = CRS("+init=epsg:4326")) %>% 
    spTransform(CRS(proj_string))
}

site_shifted_nwis <- function(nwis_site_nums){
  
  sites <- whatNWISsites(sites = nwis_site_nums$site_no) %>% 
    left_join(nwis_site_nums) 
  
  # to make this compatible w/ WQP fields and format...
  sites$coordinates <- lapply(1:nrow(sites), function(j) {
      c(sites[j,]$dec_long_va, sites[j,]$dec_lat_va)
    })
  
  select(sites, coordinates, HUCEightDigitCode = huc) %>% 
    sites_shifted()
}

sites_shifted <- function(sites){
  
  site.meta <- sites %>% filter(!is.na(HUCEightDigitCode)) %>% 
    mutate(region = get_region(HUCEightDigitCode))
  
  wgs84 <- "+init=epsg:4326"
  
  
  coords = unlist(site.meta$coordinates) %>% matrix(ncol = 2, byrow = TRUE)
  
  all.sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>% 
    spTransform(CRS(proj.string))
  
  
  if (sum(site.meta$region == 'AK') > 0){
    ak.sites <- shift_sp(all.sites[site.meta$region == 'AK',], 0.6, shift = c(80,-470), rotate=-50, proj.string = proj4string(to_sp('state')), ref=to_sp("world", "USA:alaska")) %>%
      as.data.frame %>% coordinates()
  } else {
    ak.sites <- c()
  }
  
  if (sum(site.meta$region == 'HI') > 0){
    hi.sites = shift_sp(all.sites[site.meta$region == 'HI'], 1, shift=c(520, -110), rotate=-35, proj.string = proj4string(to_sp('state')), ref=to_sp("world", "USA:hawaii")) %>%
      as.data.frame %>% coordinates()
  } else {
    hi.sites <- c()
  }
  
  if (sum(site.meta$region == 'PR') > 0){
    pr.sites = shift_sp(all.sites[site.meta$region == 'PR'],  2.5, shift = c(-140, 90), rotate=20, proj.string = proj4string(to_sp('state')), ref=to_sp("world", "Puerto Rico"))  %>%
      as.data.frame %>% coordinates()
  } else {
    pr.sites <- c()
  }
  
  sites.df <- all.sites %>% 
    as.data.frame()
  
  sites.df[site.meta$region == 'AK', ] <- ak.sites
  sites.df[site.meta$region == 'HI', ] <- hi.sites
  sites.df[site.meta$region == 'PR', ] <- pr.sites
  
  return(sites.df)
}


read_shp_zips <- function(dir, proj.string){
  zips <- file.path(dir, dir(dir))
  temp.dir <- tempdir()
  data.out <- list()
  for (i in seq_len(length(zips))){
    out <- utils::unzip(zipfile = zips[i], exdir = temp.dir)
    out.i <- grep(pattern = '.prj', out)
    layer <- basename(out[out.i[1]]) %>% gsub(replacement = "", x = ., pattern = '.prj')
    
    data <- list(readOGR(dirname(out[out.i[1]]), layer) %>% spTransform(proj.string)) %>% setNames(layer)
    data.out <- append(data.out, data)
  }
  unlink(temp.dir)
  return(data.out)
}