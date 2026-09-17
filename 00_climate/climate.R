# Disclaimer: Code got relatively complex at one point. Used AI to restructure and organize a bit. Formatting is therefore a bit detailed and "ai-like". 


# ============================================================
# Copernicus / ECDE NUTS-2 climate data — import & cleaning
# ============================================================


library(dplyr)
library(readr)
library(lubridate)
library(giscoR)


# ------------------------------------------------------------
# 1. Import the CSV
# ------------------------------------------------------------
climate_data <- read_csv(
  "00_climate/data/copernicus_climate_data_nuts2.csv",
  col_types = cols(
    nuts = col_character(),
    time = col_date(format = "%Y-%m-%d") #make sure nuts2 and date get parsed correctly
  )
)

attach(climate_data)

# ------------------------------------------------------------
# 2. Remove the empty regions
# ------------------------------------------------------------
#There is no data for regions like French Guiana. Removing all rows that are completely empty will solve this. Arounf 6000 rows removed 

indicator_cols <- c("mean_temperature", "hot_days", "total_precipitation",
                    "extreme_precipitation_days", "consecutive_dry_days",
                    "mean_wind_speed", "fwi")

climate_data <- climate_data[rowSums(!is.na(climate_data[indicator_cols])) > 0, ]


# ------------------------------------------------------------
# 4. Add a column with the NUTS-2 region name
# ------------------------------------------------------------

nuts2_lookup <- gisco_get_nuts(nuts_level = 2, year = "2021") %>%
  as.data.frame() %>%
  select(nuts = NUTS_ID, region_name = NAME_LATN) %>%
  distinct(nuts, .keep_all = TRUE)

climate_data <- climate_data %>%
  select(-any_of("region_name")) %>%   # drop old column if it exists
  left_join(nuts2_lookup, by = "nuts")

# BA01, BA02, BA03, NL35, NL36, PT19, PT1A, PT1B, PT1C, PT1D, XK00 will be missing. 
# NUTS2 regions in the Netherlands got redrawn recently and Bosnia (BA) is a missmatch. 
# It is only available as a "statistical region" in the NUTS classification. 

# ------------------------------------------------------------
# 5. Remove 2025 entirely (fwi has no data for that year)
# ------------------------------------------------------------
climate_data <- climate_data %>%
  filter(year(time) != 2025)

# ---------
# Detach & Reattach to account for new column. 
# ---------
detach(climate_data)
attach(climate_data)

colnames(climate_data) #check 

# Quick check of the result
cat("Final dimensions:", nrow(climate_data), "rows x", ncol(climate_data), "columns\n")
cat("Regions remaining:", n_distinct(climate_data$nuts), "\n")
cat("Year range:", min(year(climate_data$time)), "-", max(year(climate_data$time)), "\n")
cat("Any remaining NAs in fwi?", any(is.na(climate_data$fwi)), "\n") 

# 7 regions (e.g. Ceuta, Svalbard, Ionian Island) lack the fire indicator entirely. 
# I guess there is just no data for these regions. Since this is only ~1% of regions i'll neglect this for now.

# End of Cleaning


