wqp_temperature_sites <- function(){
  sites <- whatWQPdata(characteristicName = c("Temperature, sample",
                                              "Temperature, water",
                                              "Temperature",
                                              "Temperature, water, deg F")) %>% 
    filter(!ResolvedMonitoringLocationTypeName %in% c("Aggregate groundwater use", 
                                                     "Well",
                                                     "Estuary",
                                                     "Ocean",
                                                     "Subsurface",
                                                     "Aggregate groundwater use",
                                                     "Atmosphere",
                                                     "Aggregate groundwater use "))
  return(sites)
}


filter_nresults <- function(data, n_resultCount){
  
  filter(data, resultCount >= n_resultCount)
}

nwis_iv_sites <- function(){
  
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  
  sites <- data.frame(site_no = c(), huc_cd = c(), stringsAsFactors = FALSE)
  for(huc in hucs){
    sites <- whatNWISdata(huc = huc, service = "iv", 
                          parameterCd = "00010") %>% 
      mutate(huc = huc) %>% 
      select(site_no, huc) %>% 
      rbind(sites)
      
  }
  return(sites)
}