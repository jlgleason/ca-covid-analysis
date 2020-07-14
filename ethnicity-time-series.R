## ------------------
## Script to generate time series plot of ethnicity demographics data 
##
## Plotting difference between case percentage and population percentage
##
## Referenced these tutorials:
##
## https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
## ------------------

library(ggplot2)
library(dplyr)

# Load Data
data_directory <- "/Users/jeffreygleason 1/Desktop/Mo Collaborations/CA-Covid-Analysis"
setwd(data_directory)
ethnicity_data <- read.csv('case_demographics_ethnicity.csv')

# Add new feature: 1) cases / total population per demographic, 2) % of cases - % of population 
ca_pop <- 39512223 # from https://www.census.gov/quickfacts/CA
ethnicity_data$percent_pop_covid <- ethnicity_data$cases / (ethnicity_data$percent_ca_population * ca_pop) * 100
ethnicity_data$case_difference <- ethnicity_data$case_percentage / ethnicity_data$percent_ca_population

# Combine some race_ethnicity labels that were updated in June, remove 'Other' label (always 0)
ethnicity_data <- mutate(
        ethnicity_data, race_ethnicity = replace(
                race_ethnicity, 
                race_ethnicity == "Native Hawaiian or Pacific Islander", 
                "Native Hawaiian and other Pacific Islander"
        )
)
ethnicity_data <- mutate(
        ethnicity_data, race_ethnicity = replace(
                race_ethnicity, 
                race_ethnicity == "Multiracial", 
                "Multi-Race"
        )
)
ethnicity_data <- subset(ethnicity_data, race_ethnicity != 'Other')
        
# Plot Time Series
ethnicity_data$date <- as.Date(ethnicity_data$date)
plot <- ggplot(
        ethnicity_data,
        aes(x=date, y=case_difference, color=race_ethnicity, group=race_ethnicity)
) +
geom_line() + 
ggtitle('Disproportionate COVID-19 Burden in California') + 
ylab('Ratio Betweeen % of Cases and % of Population') + 
xlab('Month') + 
scale_x_date(date_labels = "%b") + 
labs(color="Race-Ethnicity")
print(plot)

