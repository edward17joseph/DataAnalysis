library(xml2)
library(stringr)
library(rvest)
library(parallel)

#Create boxoffice data frame
boxofficedata <- data.frame(Release = character(), `Total Gross` = character(), Theaters = character(), `Release Date`= character(), Year = integer(), link = character())

#Scrape BoxOfficeMojo for theatrical releases from 1977 to 2020.
for (year in 2020:1977){
  site <- read_html(paste0("https://www.boxofficemojo.com/year/", year))
  dat <- html_node(site,"table")
  #Columns to be selected
  table <- subset(html_table(dat) ,select = c("Release", "Total Gross", "Theaters", "Release Date", "Distributor"))
  table$Year <- year
  
  sublink <- html_attr(html_nodes(site,"[class=a-link-normal]"),"href")
  sublink <- sublink[str_starts(sublink,"/release")]
  link <- paste0("https://www.boxofficemojo.com/", sublink)
  table <- cbind(table, link)
  
  boxofficedata <- rbind(boxofficedata,table)
}

#Covert Total Gross to Numerical Value
boxofficedata$`Total Gross` <- gsub("[^0-9A-Za-z///' ]","",boxofficedata$`Total Gross`)
boxofficedata$`Total Gross` <- as.numeric(boxofficedata$`Total Gross`)

#Covert Number of Theaters to Numerical Value
boxofficedata$Theaters <- gsub("[^0-9A-Za-z///' ]","",boxofficedata$Theaters)
boxofficedata$Theaters <- as.numeric(boxofficedata$Theaters)

#Simplify Release Date to Month of Release
boxofficedata$`Release Date` <- substr(boxofficedata$`Release Date`,0,3)

#Remove rows with movies released across multiple years.
boxofficedata <- boxofficedata[!duplicated(gsub("\\/\\?.+", "", boxofficedata$link)),]

#Extract wide release films since 2000 (excluding theatrical events)
widereleases <- boxofficedata[boxofficedata$Theaters>600,]
widereleases <- widereleases[widereleases$Distributor!="Fathom Events",]
widereleases <- widereleases[widereleases$Year>1999,]

#Create data frame containing Metacritic/IMDB Ratings
s <- 1:nrow(widereleases)
fx <- function(i){
  tryCatch({
    link <- widereleases$link[i]
    site <- read_html(link)
    link <- html_attr(html_nodes(site,"[class=a-link-normal]"), "href")
    link <- link[str_starts(link, "https://pro.imdb.com/title/")][1]
    site <- read_html(link)
    link <- html_attr(html_nodes(site,"[class='a-size- a-align- a-link- visit clickable_share_link']"), "href")
    site <- read_html(link)
    
    imdbscore <<- as.numeric(html_text(html_nodes(site,"[itemprop=ratingValue]")))
    imdbreviews <<- html_text(html_nodes(site,"[itemprop=ratingCount]"))
    imdbreviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", imdbreviews))
    
    if (length(imdbscore) == 0){
      imdbscore <<- NA
    }
    if (length(imdbreviews) == 0){
      imdbreviews <<- NA
    }
  }, 
  error = function(e){
    imdbreviews <<- NA
    imdbscore <<- NA
  })
  
  tryCatch({
    sublink <- html_attr(html_nodes(html_nodes(site,"[class=titleReviewBarItem]"),"[href]")[1], "href")
    link <- paste0(gsub("\\?.+","",link), sublink)
    site <- read_html(link)
    mclink <- html_attr(html_nodes(html_nodes(site,"[class=see-more]"),"[href]")[1], "href")
    mcsite <- read_html(mclink)
    mcscore <- html_text(html_nodes(mcsite,"[class=metascore_anchor]"))
    mcreviews <- html_text(html_nodes(mcsite,"[class=based_on]"))
    
    mccritic <<- as.numeric(gsub("([0-9]+).*$", "\\1", mcscore[1]))
    mcaudience <<- as.numeric(gsub("[ ]|[\n]", "", mcscore[2]))
    
    mccriticreviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", mcreviews[1]))
    mcaudiencereviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", mcreviews[2]))
  }, 
  error = function(e){
    mccritic <<- NA
    mcaudience <<- NA
    
    mccriticreviews <<- NA
    mcaudiencereviews <<- NA
  })
  closeAllConnections()  
  return (data.frame(mc_critic_score = mccritic, mc_critic_num = mccriticreviews, 
                     mc_audience_score = mcaudience, mc_audience_num = mcaudiencereviews, 
                     imdb_score = imdbscore, imdb_reviews = imdbreviews))
}

results <- mclapply(s, fx, mc.cores = 8)

mc_imdb <- data.frame(mc_critic_score = integer(), mc_critic_num = integer(), 
                      mc_audience_score = double(), mc_audience_num = integer(),
                      imdb_score = double(), imdb_reviews = integer())
for (i in 1:length(results)){
  mc_imdb <- rbind(mc_imdb, results[[i]])
}

#Create data frame containing Rotten Tomatoes Ratings
rt <- data.frame(rt_critic_score = integer(), rt_critic_num = integer(), 
                 rt_audience_score = double(), rt_audience_num = integer())

rtsublink <- tolower(gsub("[ ]+","_",gsub("[[:punct:]]","",widereleases$Release)))

for (i in s){
  rtlink <- paste0("https://www.rottentomatoes.com/m/",rtsublink[i])
  tryCatch({
    rtsite <- read_html(paste0(rtlink,as.character("_"),as.character(widereleases$Year[i]-1)))
    rtscore <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__percentage]"))
    rtreviews <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__text--small]"))
    
    rtcritic <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[1]))
    rtaudience <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[2]))
    
    rtcriticreviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[2]))
    rtaudiencereviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[3]))
  },
  error = function(e){
    rtsite <- read_html(paste0(rtlink,as.character("_"),as.character(widereleases$Year[i])))
    rtscore <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__percentage]"))
    rtreviews <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__text--small]"))
    
    rtcritic <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[1]))
    rtaudience <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[2]))
    
    rtcriticreviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[2]))
    rtaudiencereviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[3]))
  },
  error = function(e){
    rtsite <- read_html(rtlink)
    rtscore <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__percentage]"))
    rtreviews <- html_text(html_nodes(rtsite,"[class=mop-ratings-wrap__text--small]"))
    
    rtcritic <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[1]))
    rtaudience <<- as.numeric(gsub("([0-9]+).*$", "\\1", rtscore[2]))
    
    rtcriticreviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[2]))
    rtaudiencereviews <<- as.numeric(gsub("([^[:digit:]]+)+", "", rtreviews[3]))
  }, 
  error = function(e){
    rtcritic <<- NA
    rtaudience <<- NA
    
    rtcriticreviews <<- NA
    rtaudiencereviews <<- NA
  })
  rt <- rbind(rt, data.frame(rt_critic_score = rtcritic, 
                             rt_critic_num = rtcriticreviews, 
                             rt_audience_score = rtaudience, 
                             rt_audience_num = rtaudiencereviews))
  print(rt[i,])
  closeAllConnections()
}

#Create data frame containing film attributes
filmatt <- data.frame(MPAA = character(), genres = character(), runtime = integer(), OpeningBO=integer(), Budget = integer())
for (i in 1:nrow(widereleases)){
  link <- widereleases$link[i]
  if (is.na(link)){
    filmdat <- NA
  }else{
    site <- read_html(link)
    filmdat <- html_text(html_nodes(site,"[class='a-section a-spacing-none']"))
  }
  
  MPAA <- substr(filmdat[str_starts(filmdat,"MPAA")],5,9)
  if (length(MPAA)==0){
    MPAA <- 'Unrated'
  }
  
  genres <- strsplit(gsub(" ","",substr(filmdat[str_starts(filmdat,"Genres")], 7,
                                        nchar(filmdat[str_starts(filmdat,"Genres")]))), "\n")
  if (length(genres)==0){
    genres <- NA
  }else{
    genres <- genres[[1]]
  }
  genres <- paste(genres[genres!=""], collapse = "/")
  
  runtime <- strsplit(gsub("\\D", " ", filmdat[str_starts(filmdat,"Running Time")])," ")
  if (length(runtime)==0){
    runtime <- NA
  }else{
    runtime <- runtime[[1]]
    runtime <- runtime[runtime!=""]
    runtime <- as.numeric(runtime[1])*60+as.numeric(runtime[2])
  }
  
  OpeningBO <- strsplit(gsub("\\$", " ", filmdat[str_starts(filmdat,"Opening")])," ")
  if (length(OpeningBO)==0){
    OpeningBO <- NA
  }else{
    OpeningBO <- gsub(",","",OpeningBO[[1]][2])
    OpeningBO <- as.numeric(gsub("\n","",OpeningBO))
  }
  
  Budget <- strsplit(gsub("\\$", " ", filmdat[str_starts(filmdat,"Budget")])," ")
  if (length(Budget)==0){
    Budget <- NA
  }else{
    Budget <- gsub(",","",Budget[[1]][2])
    Budget <- as.numeric(Budget)
  }
  
  filmatt <- rbind(filmatt,data.frame(MPAA,genres=I(list(genres)),runtime,OpeningBO, Budget))
  print(c(widereleases$Year[i],i))
}

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

#Write file
filmdata <- cbind(widereleases, filmatt, mc_imdb, rt)
filmdata$genres <- as.character(filmdata$genres)

write.csv(x = filmdata, file = "WideReleaseData.csv")

#adjust for inflation
for (i in 1:nrow(filmdata)){
  rate <- inflation$rate[filmdata$Year[i] == inflation$Year]
  filmdata[c("Total Gross", "OpeningBO", "Budget")][i,] <- filmdata[c("Total Gross", "OpeningBO", "Budget")][i,]*(1+rate)
}

write.csv(x = filmdata, file = "WideReleaseData_inflation.csv")