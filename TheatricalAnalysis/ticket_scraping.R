library(xml2)
library(stringr)
library(rvest)
library(parallel)

site <- read_html("https://www.natoonline.org/data/ticket-price/")
dat <- html_nodes(site,"[id=tablepress-6]")
#Columns to be selected
tickets <- as.data.frame(html_table(dat))

tickets$Price <- as.numeric(substr(tickets$Price,2,length(tickets$Price)))

tickets$Year <- as.numeric(tickets$Year)
tickets <- tickets[tickets$Year>1999,]

tickets <- na.omit(tickets)

#inflation data by year
inflationlink <- "https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/"
inflation_site <- read_html(inflationlink)

cpi_table <- html_nodes(inflation_site,"table")
cpi_table <- html_table(cpi_table[1], header = TRUE)[[1]]

names(cpi_table) <- cpi_table[1,]
cpi_table <- cpi_table[-1,]
cpi_table <- as.data.frame(apply(cpi_table,2, as.numeric))

cpi_table <- data.frame(Year = as.numeric(cpi_table$Year),
                        Avg = round(rowMeans(cpi_table[,c(2:13)], na.rm = TRUE),2))
cpi_table <- cpi_table[cpi_table$Year>1999,]

inflation <- data.frame(Year = cpi_table$Year, rate = (cpi_table$Avg[21]-cpi_table$Avg)/cpi_table$Avg)



for (i in 1:nrow(tickets)){
  rate <- inflation$rate[tickets$Year[i]==inflation$Year]
  tickets$Price[i] <- (1+rate)*tickets$Price[i]
}



write.csv(x = tickets, file = "movietickets.csv")