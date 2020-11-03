library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# Import dataset
ccd <- as.data.frame(read_excel('MasterCCD.xlsx'))
ccd$Lat <- as.numeric(ccd$Lat)
ccd$Long <- as.numeric(ccd$Long)

# What percent of all school districts are current customers with us (at least one purchase in the last two years)
mean(ccd$IsCustomer) # 13.5%
sum(ccd$IsCustomer) # 2508 customers

# Missing enrollment data for some school districts
sum(is.na(ccd$TotalEnroll)) # 1200 rows are missing enrollment data
sum(is.na(ccd$TotalEnroll)) / nrow(ccd) # 6.5% of rows are missing enrollment data out of 18,461 total rows

# Create a dataframe to check for any current customers with missing enrollment data
cust_missing <- ccd %>% filter(is.na(ccd$TotalEnroll) & IsCustomer == 1)
# Only 9 of our current customers have missing data out of the 2508 current customers.

ccd$IsCustomer <- as.factor(ccd$IsCustomer)

ccd %>% filter(!is.na(Locale)) %>%
  ggplot(aes(Locale)) + geom_bar(aes(group = IsCustomer, fill = IsCustomer), stat = 'count', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + ggtitle('Customers by Locale Category')

# Interactive Mapping
library(leaflet)
library(htmltools)
library(inlmisc)

pal = colorFactor(palette = c('#FE9882','#00BD28'), levels = c(0,1))
labels <- sprintf(
  '<strong>%s</strong>',
  cont_ccd$AgencyName
) %>% lapply(htmltools::HTML)
leaflet(data = cont_ccd) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(col = ~pal(IsCustomer), opacity = 0.8) %>%
  addMarkers(~Long, ~Lat, popup = paste('<strong>District:</strong>', cont_ccd$AgencyName, '<br>',
                                        '<strong>Address:</strong>','<br>',
                                        cont_ccd$Address, '<br>',
                                        cont_ccd$City, cont_ccd$State, cont_ccd$ZIP, '<br><br>',
                                        '<strong>Locale:</strong>', cont_ccd$Locale, '<br><br>',
                                        '<strong>Total Enrollment:</strong>', format(cont_ccd$TotalEnroll, big.mark = ',', scientific = F), '<br>',
                                        '<strong>Percent ELL:</strong>', round(cont_ccd$ELL / cont_ccd$TotalEnroll * 100, digits = 2), '%<br>',
                                        '<strong>FTE:</strong>', format(cont_ccd$FTE, big.mark = ',', scientific = F), '<br>',
                                        '<strong>Pupil to Teacher Ratio:</strong>', cont_ccd$PTRatio, '<br><br>',
                                        '<strong>Revenue per Pupil:</strong> $', format(cont_ccd$RevPerPupil, big.mark = ',', scientific = F), '<br>',
                                        '<strong>Expenditure per Pupil:</strong> $', format(cont_ccd$ExpPerPupil, big.mark = ',', scientific = F), '<br>',
                                        '<strong>Title I Revenue:</strong> $', format(cont_ccd$RevTitleI, big.mark = ',', scientific = F), '<br><br>',
                                        '<strong>2020 Sales:</strong> $', format(cont_ccd$YTDSales, big.mark = ',', scientific = F), '<br>',
                                        '<strong>2019 Sales:</strong> $', format(cont_ccd$PYSales, big.mark = ',', scientific = F), '<br><br>',
                                        '<i>The last complete year for enrollment and finance data is from 2017-18 (some schools do not have data reported). Our sales data is current as of 10/30/2020.</i>'),
             clusterOptions = markerClusterOptions(), label = labels)
