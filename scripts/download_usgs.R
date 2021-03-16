## download USGS streamflow data


library(readr)
library(dplyr)
library(purrr)
start_date <- "2000-01-01"
end_date <- "2021-03-07"


## USGS 08110100 Davidson Ck nr Lyons, TX

## USGS 08109800 E Yegua Ck nr Dime Box, TX

## USGS 08065800 Bedias Ck nr Madisonville, TX


download_usgs <- function(site_no) {
  start_date <- "2000-01-01"
  end_date <- "2021-03-07"
  
  site_no %>%
    purrr::map(~{
      url <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=",
                    .x,
                    "&parameterCd=00060&startDT=",
                    start_date,
                    "&endDT=",
                    end_date,
                    "&statCd=00003")
      df <- read_delim(url,
                       delim = "\t",
                       comment = "#") %>%
        filter(row_number() != 1)
      write_csv(df, here::here(paste0("Data/USGS_Streamflow/", .x, ".csv")))
    })
}



download_usgs(c("08110100", "08109800", "08065800"))