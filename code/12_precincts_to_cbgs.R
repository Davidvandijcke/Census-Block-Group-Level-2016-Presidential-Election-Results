#***********************************************************************

# 12_precincts_to_cbgs.R
# Intersect precinct-level 2016 election shapefiles with state-level 
# cbg shapefiles

#***********************************************************************


#***********************************************************************
#### INTERSECT PRECINCT AND CBG SHAPEFILES #### 
#***********************************************************************



# Setup -------------------------------------------------------------------

pacman::p_load(sf, tidyverse, rgdal, wrapr, rgeos, tmap)

fips <- fread(file.path(dataIn, "election", "placeCountyCBG.csv"), 
              select = c("state" = "character", "stateFIPS" = "numeric", "CBGFIPS" = "numeric"))
stateFips <- fips %>% group_by(state) %>% dplyr::summarise(stateFIPS = mean(stateFIPS, na.rm = T)) %>% na.omit()


zip_out <- list()

dir <- file.path(dataIn, "election")

redo_intersection <- FALSE

if (redo_intersection) { 
  
#fix <- c("ID", "MO", "NV")

states_loop <- c(state.abb, "DC")
pb = txtProgressBar(min = 0, max = length(states_loop)) 

count <- 1

for (i in states_loop) { 
  
  
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
        
        share <- sf::st_area(intersection) / sf::st_area(precincts_matched)
        # precincts.matched$label <- round(share, 2)
        
        purrr::map(precincts_matched %>% # weight election results of precincts by share of precinct area that overlaps w CBG,
              # then assign to CBG
              st_drop_geometry %>%
              select(starts_with('g16')),
            function(x)
              unlist(x)  %>% `%*%` (share)
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
  
  setTxtProgressBar(pb,count)
  
  count <- count + 1
  
} # end i loop


zip_out %>% 
  bind_rows() -> zip_out2

anyNA(zip_out2$g16preljoh)
# all(zip_out2$ZCTA5CE10 %in% nyt$zip)
# all(nyt$zip %in% zip_out$ZCTA5CE10) # only makes sense for whole dataset


fwrite(zip_out2, file.path(dir, 'intersected', 'precinct_cbgs_all.csv.gz'))

} else {
  
  zip_out2 <- SafeGraphR::read_many_csvs(dir = file.path(dataIn, 'election', 'intersected'))
  
  fwrite(zip_out2, file.path(dir,'precinct_cbgs_all.csv.gz'))
  
}






#***********************************************************************
#### STACK AND PROCESS INTERSECTED SHAPEFILES #### 
#***********************************************************************


# read data
votes_cbg <- fread(file.path(dataIn, "election", "precinct_cbgs_all.csv.gz"),
                   select = c("statefp", "geoid", "g16prertru", "g16predcli", "g16preljoh",
                              "g16pregste", "g16preccas", "g16preifue", "g16preowri"))



votes_cbg[votes_cbg$g16preowri < 0,] <- 0 # must be coding mistake
votes_cbg[, total := rowSums(.SD, na.rm = TRUE), .SDcols = 3:9] # get total votes_cbg cast

# merge in cbg population data
#cbg_pop <- SafeGraphR::cbg_pop[, c("poi_cbg", "unweighted_pop")] # get census data
cbg_pop <- fread(file.path("/home/antonvocalis/Dropbox (University of Michigan)/Floyd_Protests/DATA/RAW/safegraph_open_census_data/data/cbg_b01.csv"),
                 select = c("census_block_group", "B01001e1"))
setnames(cbg_pop, c("census_block_group", "B01001e1"), c("poi_cbg", "pop"))
setnames(votes_cbg, "geoid", "poi_cbg")
votes_cbg[,poi_cbg := str_pad(poi_cbg, 12, 'left', '0')]
cbg_pop[,poi_cbg := str_pad(poi_cbg, 12, 'left', '0')]

votes_cbg <- cbg_pop[votes_cbg, on = "poi_cbg"]


# create vote share variables (trump / clinton vote over total population)
votes_cbg[pop == 0, pop := NA] # if no one lives there, set to NA
votes_cbg$trump_share <- votes_cbg$g16prertru / votes_cbg$pop
votes_cbg$clinton_share <- votes_cbg$g16predcli / votes_cbg$pop


# create vote share variables (trump / clinton vote over total population)
votes_cbg[pop == 0, pop := NA] # if no one lives there, set to NA
votes_cbg$trump_share_votes <- votes_cbg$g16prertru / votes_cbg$total
votes_cbg$clinton_share_votes <- votes_cbg$g16predcli / votes_cbg$total

votes_cbg$trump <- as.numeric(votes_cbg$g16prertru > votes_cbg$g16predcli)


votes_cbg <- votes_cbg[, c("poi_cbg", "trump_share", "clinton_share", "trump_share_votes", "clinton_share_votes", "g16prertru", "g16predcli", "trump", "total")]

fwrite(votes_cbg, file.path(dataIn, 'votes2016_byCbg.csv.gz'))



