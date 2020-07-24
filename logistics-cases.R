## ------------------
## Script to map resource allocation vs. disease burden over time
##
## How do we measure resource allocation (up to time = t)?: 
##      total deliveries
##      new deliveries in window
##
## How do we measure disease burden (up to time = t-lag)?:
##      cumulative cases
##      new cases in window
##      case rate (normalize by population)
##      case rate in window (normalize by population)
## ------------------

library(ggplot2)
library(usmap)

data_directory <- "/Users/jeffreygleason 1/Desktop/Mo Collaborations/CA-Covid-Analysis"
setwd(data_directory)

PPE_LAG <- 7 # e.g. compare ppe deliveries up to June 21st to cumulative cases up to June 14th
MIN_CASES <- 5 # maximum number of cases in county for which to include its ratio
DAY_INTERVAL <- 4 # interval, in days, of printed plots
PAUSE_PER_PLOT <- 1 # amount of time, in seconds, to pause on each plot

# Load data
ppe <- read.csv("logistics_ppe.csv")
cases <- read.csv('statewide_cases.csv')

# convert to Date objects
dates <- ppe$as_of_date
ppe$as_of_date <- as.Date(ppe$as_of_date)
cases$date <- as.Date(cases$date)

all_data <- NULL
for (i in seq(2, length(levels(dates)), DAY_INTERVAL)) {

        # date range
        start_date <- as.Date(levels(dates)[i-1])
        stop_date <- as.Date(levels(dates)[i])
        
        # column names
        delivery_str <- paste("deliveries", stop_date)
        case_str <- paste("cases", stop_date)
        ratio_str <- paste("ratio", stop_date)
        
        # create ppe df

        ppe_t_slice <- ppe[ppe$as_of_date < stop_date,]
        #         ppe$as_of_date >= start_date &
        #         ppe$as_of_date < stop_date,
        # ]

        num_deliveries <- as.data.frame(table(ppe_t_slice$county))
        colnames(num_deliveries) <- c("county", delivery_str)

        # create case df
        case_stop_date <- stop_date - PPE_LAG
        case_t_slice <- cases[
                cases$date == case_stop_date &
                cases$totalcountconfirmed >= MIN_CASES,
        ]
        num_cases <- case_t_slice[c("county", "totalcountconfirmed")]
        colnames(num_cases) <- c("county", case_str)

        # merge
        data <- merge(num_deliveries, num_cases)
        data[ratio_str] <- data[delivery_str]/data[case_str]

        # convert INF/NA to 0
        # data[ratio_str][sapply(data[ratio_str], is.infinite)] <- 0
        # data[ratio_str][is.na(data[ratio_str])] <- 0

        # horizontal concat
        if (is.null(all_data)) {
                all_data <- data
        }
        else {
                all_data <- merge(all_data, data, by='county')
        }
}

# convert county names to FIPS codes
fips_codes <- c()
for (i in 1:nrow(all_data)) {
        code <- tryCatch(
                {fips("CA",county=all_data$county[i])},
                error = function(e) {NA}
        )
        fips_codes <- c(fips_codes, code)
}
all_data$fips <- fips_codes

# plot maps over time
for (i in seq(2, length(levels(dates)), DAY_INTERVAL)) {

        # date range
        stop_date <- as.Date(levels(dates)[i])

        # column/title names
        ratio_str <- paste("ratio", stop_date)
        delivery_str <- unlist(
                strsplit(as.character(stop_date), '-')
        )
        case_str <- unlist(
                strsplit(as.character(stop_date - PPE_LAG), '-')
        )

        # make plot object
        plot_obj <- plot_usmap(
                "counties",
                include = "CA",
                data = all_data,
                values = ratio_str,
        ) +
        scale_fill_continuous(name = "Number of Deliveries Per Case") +
        theme(legend.position = "right") +
        ggtitle(paste(
                "Ratio of PPE Deliveries Through",
                paste(delivery_str[2], "/", delivery_str[3], sep = ""),
                "\nto Cumulative Cases",
                paste("(min = ", MIN_CASES, ")", sep = ""),
                "Through",
                paste(case_str[2], "/", case_str[3], sep = "")
        ))

        # show plot object
        print(plot_obj)
        Sys.sleep(PAUSE_PER_PLOT)
}
