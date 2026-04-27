#########################################################
##### 2025 PurpleAir vs Tree cover analyses README #####
##Data files, scripts and workflow are described below ##
#########################################################

#########################################################
##################### DATA FILES  #######################
#########################################################
1. Purple Air data, from all GRIT and TPCHD sensors available 
    online at via datadownload tool at
    https://community.purpleair.com/t/purpleair-data-download-tool/3787. 
    Data are in the following folders:
    a. data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct
    b. data/PurpleAir/PurpleAir_Download_2025Nov_to_2026Feb
    c. data/PurpleAir/AQ_API_Download_TPCHTacomaPAs_02272026

2. Purple Air data, downloaded directly from SD cards in 
    GRIT PurpleAir monitors (for those that were not con-
    nected to WiFi for large periods of time) in the fol-
    lowing folders:
    a. data/PurpleAir/PurpleAirSDCardDowload2025Oct13
    b. data/PurpleAir/PurpleAirSDCardDownland_2026Feb11
    c. data/PurpleAir/PurpleAirSDCardDownload_2026Mar17
    d. data/PurpleAir/PurpleAirSDCardDownload2026April2
    e. data/PurpleAir/PurpleAirSDCardDownload2026April12
    Note:
    There were two problem Purple Air Sensors that required special treatment:
    -GRIT23 (in PurpleAirSDCardDownload2026April2): files 20251203.csv, 20251204.csv, 20251205.csv,20251210.csv, 20251211.csv, 20251216.csv contained weird errors that needed to be fixed by hand in excel
      (i.e., meaningless rows and characters deleted)
    -GRIT13 (in PurpleAirSDCardDowload2025Oct13) worked when read through on its own (for some rease csv files were not considered valid or safe within "safe_folder_process"" function)
3. LiDAR-derived land cover data from City of Tacoma 
    a. data/tacoma2024lidar/data/LandCover2024.gdb 
4. Pierce County Equity Index (downloaded March 17, 2025)
    a. Data/pierce county equity index


#########################################################
################ DATA PROCESSING FILES  #################
#########################################################
1.  PA_corrected_July2025-Oct2025_SB.R 
    PA_Nov2025-Feb2026.R
    PA_TPCH_Aug2025-Feb2026.R
    puts together PurpleAir data (Data files 1a. and 1b)
2. format_sdcarddat.R
    formats sdcard data to match that format of data
    downloaded from the datadownload tool
3. merge_aqdat.R
    stitches together all the API and sdcard-derived data
4. plot_and_fit_models.R 
5. grit_lidar2024_aq2025_analyses.R
    pulls out and summarizes canopy cover, mean height, and 
    conifer abundance from circles of different radii around 
    purpleairs
6. grit_aqvstrees.R
  plots and fits models to analyze relationships between 
  number of trees, tree height and identity (conifer vs 
  angiosperm) on air quality
  


    