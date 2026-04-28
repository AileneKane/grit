##Code to get PurpleAir AQdata from sd card 
##into same format as what is downloaded 
##from PurpleAir Data Download Tool
##Started December 3, 2025
##By Ailene, ailene.ettinger@tnc.org
##########################################
# still to do in this code:
# 1. fix code so it works with problem subfolders
# 2. convert time to Pacific
#
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries 
# install.packages(c("readr", "dplyr", "purrr", "stringr"))
library(readr)
library(dplyr)
library(purrr)
library(stringr)

# set working directory
setwd("~/GitHub/grit")

#need to convert sd card data files, e.g
#sd1df <- read_csv("data/PurpleAir/PurpleAirSDCardDownload_2026Mar17/GRIT05/20250903.csv",
#                   col_select = 1:40)
#colnames(sd1df)
#head(sd1df) 
#which are one per day and have ## columns
#to downloaded file format, which have
#9 columns:
#time_stamp	humidity_a	humidity_b	temperature_a	temperature_b	pressure_a	pressure_b	pm2.5_atm_a	pm2.5_atm_b
#convert time from UTC to Pacific

# ---- PARAMETERS ----
base_dir      <- "data/PurpleAir"  
cols_of_interest <- c("UTCDateTime","mac_address","current_temp_f","current_humidity","pm2_5_atm_a", "pm2_5_atm_b" )
output_dir <- "analyses/output"                      # folder to save combined csvfiles in
output_suffix <- "_PurpleAirSDCardDownload_2026Mar17_purpleairsdcard_combined.csv"                      # name suffix for per-folder output


# --- alias map (for columns named something different) ---
alias_map <- list(
  pm2_5_atm_a    = c("pm2_5_atm")
)

# --- functions! 

# function to auto‑detect UTF‑16 vs UTF‑8
read_csv_auto <- function(f) {
  
  raw <- readBin(f, what = "raw", n = 1000)
  
  is_utf16 <- length(raw) > 2 &&
    (
      identical(raw[1:2], as.raw(c(0xFF, 0xFE))) ||   # UTF‑16LE BOM
        identical(raw[1:2], as.raw(c(0xFE, 0xFF)))      # UTF‑16BE BOM
    ) ||
    mean(raw == as.raw(0x00)) > 0.2                   # lots of NULs
  
  enc <- if (is_utf16) "UTF-16LE" else "UTF-8"
  
  readr::read_csv(
    f,
    show_col_types = FALSE,
    locale = readr::locale(encoding = enc),
    name_repair = "minimal"
  )
}

# Safe UTF‑8 sanitation function
sanitize_utf8 <- function(x) {
if (is.character(x)) {
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}
x
}

# function to safely reader that adds file and folder names
safe_read_csv_old <- purrr::safely(function(f) {
  
  df <- readr::read_csv(
    f,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "Latin1"),
    name_repair = "minimal"
  )
  
  # Sanitize column names
  names(df) <- make.names(
    stringi::stri_enc_toutf8(names(df)),
    unique = TRUE
  )
  
  # Normalize character data
  df <- dplyr::mutate(
    df,
    dplyr::across(where(is.character), stringi::stri_enc_toutf8)
  )
  
  # Force gas numeric
  if ("gas" %in% names(df)) {
    df$gas <- as.numeric(df$gas)
  }
  
  df$.source_file <- basename(f)
  df
})

safe_read_csv <- purrr::safely(function(f) {
  
  df <- read_csv_auto(f)
  
  # Fix column names
  names(df) <- sanitize_utf8(names(df))
  names(df) <- make.names(names(df), unique = TRUE)
  
  # Fix all character columns
  df[] <- lapply(df, sanitize_utf8)
  
  names(df) <- make.names(
    stringi::stri_enc_toutf8(names(df)),
    unique = TRUE
  )
  
  df <- dplyr::mutate(
    df,
    dplyr::across(where(is.character), stringi::stri_enc_toutf8)
  )
  
  if ("gas" %in% names(df)) {
    df$gas <- as.numeric(df$gas)
  }
  
  df$.source_file <- basename(f)
  df
})
``

harmonize_columns <- function(df, alias_map) {
  # Normalize existing names (no spaces at ends, consistent case)
  orig_names <- names(df)
  norm_names <- str_trim(orig_names)
  names(df) <- norm_names  # keep original case for values, only fix name whitespace
  
  # Build a lookup for case-insensitive matching
  lower_names <- tolower(names(df))
  
  for (canonical in names(alias_map)) {
    aliases <- alias_map[[canonical]]
    aliases_lower <- tolower(str_trim(aliases))
    
    # Which aliases exist in df?
    idx <- match(aliases_lower, lower_names)
    present <- !is.na(idx)
    
    if (!any(present)) {
      # No alias present: make an NA column
      df[[canonical]] <- NA
    } else {
      # Get the actual existing alias columns (could be multiple)
      existing_cols <- names(df)[idx[present]]
      
      if (length(existing_cols) == 1) {
        # Single alias -> rename to canonical (if different)
        if (existing_cols != canonical) {
          df <- df %>%
            rename(!!canonical := all_of(existing_cols))
        }
      } else {
        # Multiple aliases -> coalesce left-to-right into canonical
        df[[canonical]] <- coalesce(!!!df[existing_cols])
        # Optionally drop the extra alias columns (keep dataset clean)
        df <- df %>% select(-all_of(setdiff(existing_cols, canonical)))
      }
    }
  }
  
   df
}

safe_folder_process <- purrr::safely(function(folder_path) {
  
  csv_files <- list.files(
    folder_path,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(csv_files) == 0) return(NULL)
  
  read_results <- purrr::map(csv_files, safe_read_csv)
  dfs <- purrr::map(read_results, "result")
  dfs <- purrr::compact(dfs)
  
  if (length(dfs) == 0) return(NULL)
  
  dfs_harmonized <- purrr::map(dfs, ~ harmonize_columns(.x, alias_map))
  
  # FORCE numeric for most columns, firmware to be character and 
  
  numeric_cols   <- c("gas","current_temp_f","current_humidity",
                      "current_dewpoint_f","pressure","adc","mem",
                      "rssi", "uptime", "pm1_0_cf_1", "pm2_5_cf_1", 
                      "pm10_0_cf_1", "pm1_0_atm","pm2_5_atm_a", "pm10_0_atm",
                      "pm2.5_aqi_cf_1","pm2.5_aqi_atm",	"p_0_3_um",	"p_0_5_um",	
                      "p_1_0_um","p_2_5_um","p_5_0_um",	"p_10_0_um","pm1_0_cf_1_b",
                      "pm2_5_cf_1_b",	"pm10_0_cf_1_b","pm1_0_atm_b","pm2_5_atm_b",
                      "pm10_0_atm_b","pm2.5_aqi_cf_1_b","pm2.5_aqi_atm_b",	
                      "p_0_3_um_b","p_0_5_um_b","p_1_0_um_b",	"p_2_5_um_b",	
                      "p_5_0_um_b",	"p_10_0_um_b")
  character_cols <- c("UTCDateTime","firmware_ver", "mac_address","hardware")
  
  dfs_typed <- purrr::map(dfs_harmonized, ~{
    for (col in intersect(numeric_cols, names(.x))) {
      .x[[col]] <- as.numeric(.x[[col]])
    }
    for (col in intersect(character_cols, names(.x))) {
      .x[[col]] <- as.character(.x[[col]])
    }
    .x
  })
  
  
  all_cols <- unique(unlist(purrr::map(dfs_typed, names)))
  
  dfs_aligned <- purrr::map(dfs_typed, ~{
    miss <- setdiff(all_cols, names(.x))
    if (length(miss)) .x[miss] <- NA
    .x[all_cols]
  })
  
  combined <- dplyr::bind_rows(dfs_aligned)
  
  present_cols <- intersect(cols_of_interest, names(combined))
  if (length(present_cols) == 0) return(NULL)
  
  combined_selected <- combined |>
    dplyr::select(dplyr::all_of(present_cols), .source_file)
  combined_selected$id <- basename(folder_path)
  out_file <- file.path(output_dir, paste0(basename(folder_path), output_suffix))
  readr::write_csv(combined_selected, out_file)
  
  combined_selected
})

# ---- Get immediate subfolders (excluding files) ----
folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
folders <- folders[grep("data/PurpleAir/PurpleAirSDCardDownload",folders)]

subfolders <- unlist(
  lapply(folders, function(x) {
    list.dirs(x, recursive = FALSE, full.names = TRUE)
  })
)


#for october 2025 folder, Subfolders 1 & 4 are causing problems so remove for now
#probsubfolds<-c(1,4)
#subfolders<-subfolders[-probsubfolds]

# if (length(subfolders) == 0) {
#   stop("No subfolders found in base_dir. Check path or structure.")
# }

# ---- Process each subfolder----

#### old process safer process for results:
# 
# results <- purrr::map(subfolders, function(folder_path) {
#   # Find CSVs in this folder (non-recursive)
#   csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
# 
#   if (length(csv_files) == 0) {
#     message(sprintf("No CSV files in: %s", folder_path))
#     return(NULL)
#   }
# 
# 
#   # Read all CSVs, skipping failures
#   dfs <- purrr::map(csv_files, safe_read_csv)
#   dfs <- dfs[!purrr::map_lgl(dfs, is.null)]
# 
#   if (length(dfs) == 0) {
#     message(sprintf("All CSV reads failed in: %s", folder_path))
#     return(NULL)
#   }
# 
#   # Harmonize each DF with alias map
#   dfs_harmonized <- map(dfs, ~ harmonize_columns(.x, alias_map))
# 
#   #coerce "Gas" to  be numeric in all cases
#   dfs_numeric <- purrr::map(dfs_harmonized, ~{
#     if ("gas" %in% names(.x)) {
#       .x$gas <- as.numeric(.x$gas)
#     }
#     .x
#   })
# 
#   # Align columns across files (in case some canonical columns are missing)
#   all_cols <- unique(unlist(map(dfs_numeric, names)))
#   dfs_aligned <- map(dfs_numeric, ~{
#     miss <- setdiff(all_cols, names(.x))
#     if (length(miss)) .x[miss] <- NA
#     .x[all_cols]
#   })
# 
#   # Row-bind; use bind_rows to align columns
#   combined <- dplyr::bind_rows(dfs_aligned)
# 
#   # Select only columns of interest that actually exist
#   present_cols <- intersect(cols_of_interest, colnames(combined))
#   missing_cols <- setdiff(cols_of_interest, present_cols)
# 
#   if (length(present_cols) == 0) {
#     warning(sprintf(
#       "None of the requested columns found in folder: %s. Available columns: %s",
#       basename(folder_path), paste(colnames(combined), collapse = ", ")
#     ))
#     # Return the combined dataset with only .source_file if desired
#     return(dplyr::select(combined, .source_file))
#   }
# 
#   if (length(missing_cols) > 0) {
#     message(sprintf(
#       "In folder %s, missing columns: %s",
#       basename(folder_path), paste(missing_cols, collapse = ", ")
#     ))
#   }
# 
#   
#   # Keep the requested columns (present) + optional metadata column
#   combined_selected <- dplyr::select(combined, dplyr::all_of(present_cols))
# 
# 
#   # Write output CSV to the same folder (optional)
#   out_file <- file.path(output_dir, paste0(basename(folder_path),output_suffix))
#   readr::write_csv(combined_selected, out_file)
# 
#   message(sprintf("Wrote: %s (%s rows)", out_file, nrow(combined_selected)))
# 
#   #add id
#   combined_selected$id <- basename(folder_path)
#   # Return in case you want it programmatically
#   combined_selected
#   
# })

# ---- Process each subfolder----
#### new safer process for results:
results <- purrr::map(subfolders, safe_folder_process)

# Extract successful results only
results <- purrr::map(results, "result")
results <- purrr::compact(results)

# Optionally: combine all folders into one grand table
all_combined <- purrr::compact(results) |> dplyr::bind_rows(.id = "folder_index")
# If you want to save globally:
readr::write_csv(all_combined, file.path("analyses/output", "ALLPASDCARD_combined_2026March.csv"))

