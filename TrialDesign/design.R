
library(sf)
library(data.table)
library(broom)
library(tidyverse)

setwd('/Users/tmieno2/Dropbox/Mieno_Bullock_shared/Larson')

#--- import the boundary data ---#
boundary <- st_read(dsn = ".", "Larson Farms-Larson Farms-EB2-Boundary_2") %>% 
  st_transform(32616)
st_crs(boundary) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"

#--- import the ab_line data ---#
ab_line <- st_read(dsn = ".", "ab_line") %>% 
	st_transform(32616) # to UTM 16
st_crs(ab_line) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"

#--------------------------
# Parameters 
#--------------------------
#--- direction ---#
# pick one of c('SN','NS','WE','EW')
long_in <- 'WE' # the direction in which the applicator moves first

# pick one of c('SN','NS','WE','EW')
# SN means the applicator goes from south to north 
short_in <- 'SN'  

#--- grid parameters in feet ---#
planter_width_ft <- 60
plot_length_ft <- 280
plot_width_ft <- 120

#--- meter to feet ---#
feetinameter <- 3.28084

#--- grid parameters in meter ---#
plot_length_meter <- plot_length_ft/feetinameter
plot_width_meter <- plot_width_ft/feetinameter
planter_width_meter <- planter_width_ft/feetinameter

#--- identify the starting point ---#
bbox_field <- st_bbox(boundary)
# Notes: change this to control the alignment of grids
starting_point <- c(bbox_field['xmin'],bbox_field['ymin'])

make_grids <- function(boundary,ab_line,long_in,short_in,starting_point){
  
  #===================================
  # Define the long and short vectors
  #===================================
  ab_1 <- st_geometry(ab_line)[[1]][1,]
  ab_2 <- st_geometry(ab_line)[[1]][2,]
  
  #--------------------------
  # find the origin, end point, and the rotation matrix
  #--------------------------
  if (long_in=='SN'){ # if the plot is long in SN direction
    #--- find the origin and end points ---#
    if (ab_1[2]>=ab_2[2]){
      origin <- ab_2
      end_point <- ab_1
    } else{
      origin <- ab_1
      end_point <- ab_2
    }
    #--- find rotation vector ---#
    #if (short_in=='WE'){
    #rotate_mat <- matrix(c(cos(-pi/2),sin(-pi/2),-sin(-pi/2),cos(-pi/2)),nrow=2)
    #} else{
    #rotate_mat <- matrix(c(cos(pi/2),sin(pi/2),-sin(pi/2),cos(pi/2)),nrow=2)
    #}
    if (short_in=='WE'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    } else{
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='NS') {
    #--- find the origin and end points ---#
    if (ab_1[2]>=ab_2[2]){
      origin <- ab_1
      end_point <- ab_2
    } else{
      origin <- ab_2
      end_point <- ab_1
    }
    #--- find rotation vector ---#
    if (short_in=='WE'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    } else{
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='WE') {
    #--- find the origin and end points ---#
    if (ab_1[1]>=ab_2[1]){
      origin <- ab_2
      end_point <- ab_1
    } else{
      origin <- ab_1
      end_point <- ab_2
    }
    #--- find rotation vector ---#
    if (short_in=='SN'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    } else if (short_in=='NS'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='EW'){
    #--- find the origin and end points ---#
    if (ab_1[1]>=ab_2[1]){
      origin <- ab_1
      end_point <- ab_2
    } else{
      origin <- ab_2
      end_point <- ab_1
    }
    
    #--- find rotation vector ---#
    if (short_in=='SN'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    } else if (short_in=='NS'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    }
  }
  
  #--------------------------
  # Find the long and short vectors
  #--------------------------
  #--- long vector ---#
  long_vec <- end_point-origin 
  
  #--- short vector ---#
  short_vec <- rotate_mat %*% long_vec
  
  #--------------------------
  # normalize the vectors
  #--------------------------
  vector_len <- sqrt(long_vec[1]^2+long_vec[2]^2)
  long_norm <- long_vec/vector_len*plot_length_meter 
  short_norm <- (short_vec/vector_len*plot_width_meter) %>% as.vector()
  
  #===================================
  # Create grids
  #===================================
  
  
  #--- how many rows and columns ---#
  if (long_in %in% c('SN','NS')){
    num_rows <- ceiling((bbox_field['ymax']-bbox_field['ymin'])/plot_length_meter)
    num_cols <- ceiling((bbox_field['xmax']-bbox_field['xmin'])/plot_width_meter)
  } else if (long_in %in% c('WE','EW')){
    num_rows <- ceiling((bbox_field['ymax']-bbox_field['ymin'])/plot_width_meter)
    num_cols <- ceiling((bbox_field['xmax']-bbox_field['xmin'])/plot_length_meter)
  }
  
  #--------------------------
  # Create grids
  #--------------------------
  
  all_polygons_ls <- list()
  if (long_in %in% c('SN','NS')){ # if the applicator moves NS or SN
    for (i in 1:num_cols){
      if(i==1){
        col_start <- starting_point
      } else{
        col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][4,]
      }
      
      col_polygons_ls <- list()
      for (j in 1:num_rows){
        if(j==1){
          point_1 <- col_start
        } else{
          point_1 <- col_polygons_ls[[j-1]][[1]][2,]
        }
        
        point_2 <- point_1+long_norm
        point_3 <- point_2+short_norm
        point_4 <- point_3-long_norm
        
        p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>% 
          list() %>% 
          st_polygon()
        col_polygons_ls[[j]] <- p_temp
      }
      
      all_polygons_ls[[i]] <- 
        st_sf(
          plotid=(1+(i-1)*num_rows):(i*num_rows),
          st_sfc(col_polygons_ls)
        )
    }
  } else{ # if the applicator moves WE or EW
    for (i in 1:num_rows){
      if(i==1){
        col_start <- starting_point
      } else{
        col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][4,]
      }
      
      col_polygons_ls <- list()
      for (j in 1:num_cols){
        if(j==1){
          point_1 <- col_start
        } else{
          point_1 <- col_polygons_ls[[j-1]][[1]][2,]
        }
        
        point_2 <- point_1+long_norm
        point_3 <- point_2+short_norm
        point_4 <- point_3-long_norm
        
        p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>% 
          list() %>% 
          st_polygon()
        col_polygons_ls[[j]] <- p_temp
      }
      
      all_polygons_ls[[i]] <- 
        st_sf(
          plotid=(1+(i-1)*num_cols):(i*num_cols),
          st_sfc(col_polygons_ls)
        )
    }
  }
  #--- combine all the grids ---#
  all_grids <- do.call(rbind,all_polygons_ls)
  
  return(all_grids)
}

design_grids <- make_grids(boundary,ab_line,long_in,short_in,starting_point)
st_crs(design_grids) <- st_crs(boundary)
st_write(design_grids,'design_grids.shp')

#===================================
# Visualization for confirmation
#===================================
grids_td <- design_grids %>% 
	as('Spatial') %>% 
	tidy() %>% 
	data.table()

boundary_td <- boundary %>% 
	as('Spatial') %>% 
	tidy() %>% 
	data.table()

ab_line_td <- ab_line %>% 
	as('Spatial') %>% 
	tidy() %>% 
	data.table()

ggplot() +
	geom_polygon(data=boundary_td,aes(y=lat,x=long,group=group)) +
	geom_polygon(data=ab_line_td,aes(y=lat,x=long,group=group),color='red',alpha=0,size=1) +
	geom_polygon(data=grids_td,aes(y=lat,x=long,group=group),color='blue',alpha=0) 

#===================================
# Design experiments
#===================================
N_levels <- c(120,160,200,240)

trial_grids <- st_intersection(boundary,design_grids) %>% 
  mutate(area=as.numeric(st_area(.))) 

trial_grids_used <- trial_grids %>% 
  filter(area>2800) %>% 
  mutate(N=as.character(sample(N_levels,nrow(.),replace=TRUE)))

trial_grids_unused <- trial_grids %>% 
  filter(area<=2800) %>% 
  mutate(N='Not Used')

trial <- rbind(trial_grids_used,trial_grids_unused) %>% 
  mutate(N=factor(N,levels=(c('Not Used',as.character(N_levels)))))

library(RColorBrewer)
colourCount = length(unique(trial$N))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

ggplot() +
  geom_sf(data=trial,aes(fill=N)) +
  scale_fill_manual(values=c('darkgreen',getPalette(colourCount-1)))

ggsave()

