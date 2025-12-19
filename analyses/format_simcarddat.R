##Code to get PurpleAir AQdata from sd card 
##into same format as what is downloaded 
##from PurpleAir Data Download Tool
##Started December 3, 2025
##By Ailene, ailene.ettinger@tnc.org
##########################################
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
#sd1df <- read_csv("data/PurpleAir/PurpleAirSimCardDowload2025Oct13/GRIT07/20250801.csv",
#                   col_select = 1:40)
#colnames(sd1df)
#head(sd1df$uptime)
#which are one per day and have ## columns
#to downloaded file format, which have
#9 columns:
#time_stamp	humidity_a	humidity_b	temperature_a	temperature_b	pressure_a	pressure_b	pm2.5_atm_a	pm2.5_atm_b
#convert time from UTC to Pacific

# ---- PARAMETERS ----
base_dir      <- "data/PurpleAir/PurpleAirSDCardDowload2025Oct13"  
cols_of_interest <- c("UTCDateTime","mac_address","current_temp_f","current_humidity","pm2_5_atm_a", "pm2_5_atm_b" )
output_dir <- "analyses/output"                      # folder to save combined csvfiles in
output_suffix <- "_PurpleAirSDCardDowload2025Oct13_purpleairsdcard_combined.csv"                      # name suffix for per-folder output


# --- alias map (for columns named something different) ---
alias_map <- list(
  pm2_5_atm_a    = c("pm2_5_atm")
)


# Safe reader that adds file and folder names
safe_read_csv <- function(file) {
  tryCatch({
    df <- readr::read_csv(
      file,
      show_col_types = FALSE,
      na = c("", "NA"),
      skip_empty_rows = TRUE,
      locale = readr::locale(encoding = "UTF-8")
    )
    df %>%
      mutate(
        file_name   = basename(file),
        folder_name = basename(dirname(file))
      )
  }, error = function(e) {
    warning(sprintf("Failed to read: %s\nReason: %s", file, e$message))
    NULL
  })
}

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
# ---- Get immediate subfolders (excluding files) ----
subfolders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

#Subfolders 1 & 4 are causing problems so remove for now
probsubfolds<-c(1,4)

subfolders<-subfolders[-probsubfolds]

if (length(subfolders) == 0) {
  stop("No subfolders found in base_dir. Check path or structure.")
}

# ---- Process each subfolder ----
results <- purrr::map(subfolders, function(folder_path) {
  # Find CSVs in this folder (non-recursive)
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(csv_files) == 0) {
    message(sprintf("No CSV files in: %s", folder_path))
    return(NULL)
  }
  
  # Read all CSVs, skipping failures
  dfs <- purrr::map(csv_files, safe_read_csv)
  dfs <- dfs[!purrr::map_lgl(dfs, is.null)]
  
  if (length(dfs) == 0) {
    message(sprintf("All CSV reads failed in: %s", folder_path))
    return(NULL)
  }
  
  # Harmonize each DF with alias map
  dfs_harmonized <- map(dfs, ~ harmonize_columns(.x, alias_map))
  
  # Align columns across files (in case some canonical columns are missing)
  all_cols <- unique(unlist(map(dfs_harmonized, names)))
  dfs_aligned <- map(dfs_harmonized, ~{
    miss <- setdiff(all_cols, names(.x))
    if (length(miss)) .x[miss] <- NA
    .x[all_cols]
  })

  # Row-bind; use bind_rows to align columns
  combined <- dplyr::bind_rows(dfs_aligned)
  
  # Select only columns of interest that actually exist
  present_cols <- intersect(cols_of_interest, colnames(combined))
  missing_cols <- setdiff(cols_of_interest, present_cols)
  
  if (length(present_cols) == 0) {
    warning(sprintf(
      "None of the requested columns found in folder: %s. Available columns: %s",
      basename(folder_path), paste(colnames(combined), collapse = ", ")
    ))
    # Return the combined dataset with only .source_file if desired
    return(dplyr::select(combined, .source_file))
  }
  
  if (length(missing_cols) > 0) {
    message(sprintf(
      "In folder %s, missing columns: %s",
      basename(folder_path), paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Keep the requested columns (present) + optional metadata column
  combined_selected <- dplyr::select(combined, dplyr::all_of(present_cols))
  
  
  # Write output CSV to the same folder (optional)
  out_file <- file.path(output_dir, paste0(basename(folder_path),output_suffix))
  readr::write_csv(combined_selected, out_file)
  
  message(sprintf("Wrote: %s (%s rows)", out_file, nrow(combined_selected)))
  
  # Return in case you want it programmatically
  combined_selected
})




# Optionally: combine all folders into one grand table
all_combined <- purrr::compact(results) |> dplyr::bind_rows(.id = "folder_index")
# If you want to save globally:
# readr::write_csv(all_combined, file.path(base_dir, "ALL_FOLDERS_combined.csv"))
``
