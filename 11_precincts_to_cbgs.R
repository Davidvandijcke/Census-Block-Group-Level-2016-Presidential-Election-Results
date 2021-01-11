

# Setup -------------------------------------------------------------------

pacman::p_load(sf, tidyverse, rgdal, wrapr, rgeos, tmap)

fips <- fread(file.path(datain, "election", "placeCountyCBG.csv"), 
              select = c("state" = "character", "stateFIPS" = "numeric", "CBGFIPS" = "numeric"))
stateFips <- fips %>% group_by(state) %>% dplyr::summarise(stateFIPS = mean(stateFIPS, na.rm = T)) %>% na.omit()


zip_out <- list()

dir <- file.path(datain, "election")

#fix <- c("ID", "MO", "NV")

pb = txtProgressBar(min = 0, max = nrow(stateFips)) 

for (i in state.abb) { 
  
  setTxtProgressBar(pb,i)
  
  fi <- str_pad(stateFips[stateFips$state == i,]$stateFIPS, 2, 'left', '0')
  snam <- tolower(i)
  
  tryCatch(
    {
    cbg_state <- # get cbg shapefilezip
      read_sf(file.path(dir, "census_bg_shapefiles", str_c("tl_2019_", fi, "_bg"))) %>% 
      st_zm() %>% # to avoid 'OGR: not enough data' error
      st_transform('+proj=laea +lat_0=10 +lon_0=-81 +ellps=WGS84 +units=m +no_defs') %>%
      st_buffer(0) %>%
      set_names(colnames(.) %>% str_to_lower())
    
    election_state <-  # get precinct-level election results
      read_sf(file.path(dir, 'result_shapefiles', str_c(snam, '_2016/'))) %>% 
      st_zm() %>% 
      st_transform('+proj=laea +lat_0=10 +lon_0=-81 +ellps=WGS84 +units=m +no_defs') %>%
      st_buffer(0) %>%
      set_names(colnames(.) %>% str_to_lower()) %>% 
      select(contains('g16')) %>% 
      mutate(state = i,
             precinct_key_national = snam %p% 1:nrow(.))
    
    precincts_selected <- st_intersects(cbg_state, election_state)
    cbg_state <- cbg_state[sapply(precincts_selected, 
                                            function(x) length(x) > 0), ]
    precincts_selected <- precincts_selected[sapply(precincts_selected, 
                                                    function(x) length(x) > 0)]
    
    g16 <- str_extract(colnames(election_state), 'g16.*') %>% na.omit()
    cbg_state[g16] <- as.numeric('')
    
    
    for(j in 1:nrow(cbg_state) ) {
      # narrow down what to intersect:
      election_state %>% 
        slice(precincts_selected[[j]]) -> precincts_matched
      
      # intersect:
      sf::st_intersection(cbg_state[j, ],
                      precincts_matched) -> intersection
      
      share <- st_area(intersection) / st_area(precincts_matched)
      # precincts.matched$label <- round(share, 2)
      
      map(precincts_matched %>% # weight election results of precincts by share of precinct area that overlaps w CBG,
                                # then assign to CBG
            st_drop_geometry %>%
            select(starts_with('g16')),
          function(x)
            unlist(x) %>% `%*%`(share)
      ) %>%
        unlist() %>%  
        as.list() -> cbg_state[j, g16]
      
    } # end j loop
    
    zip_out[[snam]] <- cbg_state %>%
                  st_drop_geometry()
    
    fwrite(cbg_state %>% st_drop_geometry(),
              file.path(dir, 'intersected', '/cbg_match_' %p% snam %p% '.csv.gz'))
    }, 
    error=function(cond) {
      message(cond)
    }
    )
  
} # end i loop

zip_out %>% 
  bind_rows() -> zip_out2

anyNA(zip_out2$g16preljoh)
# all(zip_out2$ZCTA5CE10 %in% nyt$zip)
# all(nyt$zip %in% zip_out$ZCTA5CE10) # only makes sense for whole dataset

fwrite(zip_out2, file.path(dir, 'intersected', 'precinct_cbgs_all.csv.gz'))







# Plot --------------------------------------------------------------------


tm_shape(precincts_matched) +
  tm_fill('grey') +
  tm_borders() +
  tm_shape(zip) +
  tm_fill('purple', alpha = 0.3)

tm_shape(precincts_matched) +
  tm_fill('grey') +
  tm_borders() +
  tm_text('Label', size = 0.5) +
  tm_shape(zip) +
  tm_fill('purple', alpha = 0.3)

tm_shape(precincts_matched) +
  tm_fill('grey') +
  tm_borders() +
  tm_shape(intersection) +
  tm_fill('purple', alpha = 0.3)

tm_shape(intersection) +
  tm_fill('purple', alpha = 0.3) +
  tm_borders() +
  tm_shape(zipcodes_state) +
  tm_fill('purple', alpha = 0.3)




