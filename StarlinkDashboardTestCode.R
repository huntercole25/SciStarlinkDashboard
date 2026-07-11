library(httr2)
library(data.table)
library(ggplot2)
library(tidyr)


STARLINK_CLIENT_ID <- "e77634c7-9f28-481f-8546-777ce3a9de9c"
STARLINK_CLIENT_SECRET <- "BirdsArentReal25!BirdsArentReal25!"

# ---- Get Starlink bearer token ----

get_starlink_token <- function() {
  
  resp <- request("https://api.starlink.com/auth/connect/token") |>
    req_auth_basic(
      STARLINK_CLIENT_ID,
      STARLINK_CLIENT_SECRET
    ) |>
    req_body_form(
      grant_type = "client_credentials"
    ) |>
    req_perform()
  
  token <- resp_body_json(resp)$access_token
  
  return(token)
}

# get_starlink_token <- function() {
#   
#   resp <- request("https://api.starlink.com/auth/connect/token") |>
#     req_auth_basic(
#       Sys.getenv("STARLINK_CLIENT_ID"),
#       Sys.getenv("STARLINK_CLIENT_SECRET")
#     ) |>
#     req_body_form(
#       grant_type = "client_credentials"
#     ) |>
#     req_perform()
#   
#   token <- resp_body_json(resp)$access_token
#   
#   return(token)
# }


# ---- Query Starlink V2 daily usage ----

get_starlink_daily_usage <- function(token,
                                     start_date,
                                     end_date) {
  
  resp <- request("https://starlink.com/api/public/v2/data-usage/query") |>
    req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(
      list(
        startDate = format(start_date, "%Y-%m-%dT00:00:00Z"),
        endDate = format(end_date, "%Y-%m-%dT00:00:00Z")
      )
    ) |>
    req_method("POST") |>
    req_perform()
  
  resp_body_json(resp)
}


# ---- Flatten response to data.table ----

extract_today_usage <- function(usage_raw) {
  
  library(data.table)
  
  results <- usage_raw$content$results[[1]]
  
  # Extract every daily record from every billing cycle
  daily_usage <- rbindlist(
    lapply(results$billingCycles, function(cycle) {
      
      rbindlist(
        lapply(cycle$dailyDataUsage, function(day) {
          
          data.table(
            date = as.Date(substr(day$date, 1, 10)),
            priorityGB = as.numeric(day$priorityGB),
            optInPriorityGB = as.numeric(day$optInPriorityGB),
            standardGB = as.numeric(day$standardGB),
            nonBillableGB = as.numeric(day$nonBillableGB)
          )
          
        }),
        fill = TRUE
      )
      
    }),
    fill = TRUE
  )
  
  # Show available dates (debug)
  print(range(daily_usage$date))
  print(max(daily_usage$date))
  
  # Use the latest date returned by Starlink instead of Sys.Date()
  latest_date <- max(daily_usage$date)
  
  daily_usage <- daily_usage[date == latest_date]
  
  return(daily_usage)
}

extract_current_billing_cycle <- function(usage_raw) {
  
  results <- usage_raw$content$results[[1]]
  
  # Find the most recent billing cycle start date
  current_cycle <- results$billingCycles[[
    which.max(
      sapply(
        results$billingCycles,
        function(x) as.Date(substr(x$startDate, 1, 10))
      )
    )
  ]]
  
  data.table::data.table(
    billing_cycle_start = as.Date(substr(current_cycle$startDate, 1, 10)),
    billing_cycle_end = as.Date(substr(current_cycle$endDate, 1, 10))
  )
}


# ---- Run workflow ----

token <- get_starlink_token()

usage_raw <- get_starlink_daily_usage(
  token,
  start_date = Sys.Date(),
  end_date = Sys.Date()+1
)

usage_dt <- extract_today_usage(usage_raw)

billing_cycle <- extract_current_billing_cycle(usage_raw)

DateFiller <- data.table(date = seq.Date(billing_cycle$billing_cycle_start, billing_cycle$billing_cycle_end, 1))

FilledUse <- merge(DateFiller, usage_dt, all.x = T)

FilledUse[is.na(priorityGB), priorityGB := 0]
FilledUse[is.na(optInPriorityGB), optInPriorityGB := 0]
FilledUse[is.na(standardGB), standardGB := 0]
FilledUse[is.na(nonBillableGB), nonBillableGB := 0]

LongUse <- as.data.table(pivot_longer(FilledUse, c("priorityGB", "optInPriorityGB", "standardGB", "nonBillableGB"), names_to = "DataCategory", values_to = "GB"))

ggplot(LongUse, aes(date, GB, fill = DataCategory))+
  geom_col()

csv <- "starlink_daily_usage.csv"

if (file.exists(csv)) {
  old <- fread(csv)
  
  if (!today_usage$date %in% old$date) {
    fwrite(today_usage, csv, append = TRUE)
  }
  
} else {
  fwrite(today_usage, csv)
}
