#########################################################
##### 2025 PurpleAir vs Tree cover analyses README #####
##Data files, scripts and workflow are described below ##
#########################################################

#########################################################
##################### DATA FILES  #######################
#########################################################
1. Purple Air data, from all GRIT sensors available 
    online at, via datadownload tool. Data are in the 
    following folders
    a. data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct
2. Purple Air data, downloaded directly from SD cards in 
    GRIT PurpleAir monitors (for those that were not con-
    nected to WiFi for large periods of time) in the fol-
    lowing folders:
    a. data/PurpleAir/PurpleAirSDCardDowload2025Oct13
3. LiDAR-derived land cover data from City of Tacoma 
    a. data/tacoma2024lidar/data/LandCover2024.gdb


#########################################################
################ DATA PROCESSING FILES  #################
#########################################################
1. analyses/PA_corrected_July2025-Oct2025_SB.R 
    puts together all PurpleAir data (Data files 1a. and 2a)
2. analyses/format_simcarddat.R
    formats simcard data to match that format of data
    downloeded from the datadownload tool
3. grit_lidar2024_aq2025_analyses.R
    pulls out and summarizes canopy cover, mean height, and 
    conifer abundance from circles of different radii around 
    purpleairs
4. 
  


    