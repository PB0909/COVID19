install.packages("httr")
install.packages("rvest")
install.packages("stringi")

require("httr")
require("rvest")

library(httr)
library(rvest)

# Task 1: Get COVID-19 pandemic Wiki page
get_wiki_covid19_page <- function(url, prm) {
  wiki_url <- url
  query_prm <- list(title = prm)
  response <- GET(wiki_url, query = query_prm)
  return(response)
}

# Call the get_wiki_covid19_page function and print the response
response <- get_wiki_covid19_page("https://en.wikipedia.org/wiki/Template:COVID-19_testing_by_country", "Template:COVID-19_testing_by_country")
page_content <- content(response, as = "text")

# Task 2: Extract COVID-19 testing data table
root_node <- read_html(page_content)
table_node <- html_node(root_node, xpath = "//table[contains(@class, 'wikitable')]")  # Adjust the XPath if needed
covid_data <- html_table(table_node, fill = TRUE)

# Print the extracted data frame
head(covid_data)

# Print the summary of the data frame
summary(covid_data)
preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  date <- as.factor(data_frame$date)
  tested <- as.numeric(gsub(",", "", data_frame$tested))
  confirmed <- as.numeric(gsub(",", "", data_frame$confirmed))
  confirmed.tested.ratio <- as.numeric(gsub(",", "", data_frame$`confirmed.tested.ratio`))
  data_frame$tested.population.ratio <- as.numeric(gsub(",", "", data_frame$`tested.population.ratio`))
  data_frame$confirmed.population.ratio <- as.numeric(gsub(",", "", data_frame$`confirmed.population.ratio`))
    return(data_frame)
}


  
# call `preprocess_covid_data_frame` function and assign it to a new data frame
covid_data_v2 <- preprocess_covid_data_frame(covid_data)

# Print the summary of the processed data frame again
summary(covid_data_v2)

# Export the data frame to a csv file
write.csv(covid_data_v2, file='Covid.csv')


# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv")
# File path
print(file_path)
file.exists(file_path)


## Download a sample csv file
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Read covid_data_frame_csv from the csv file
covid_subset <- read.csv('covid.csv')

# Get the 5th to 10th rows, with two "country" "confirmed" columns
covid_subset[5:10,c('country','confirmed')]
# Get the total confirmed cases worldwide
sum(covid_subset$confirmed)

# Get the total tested cases worldwide
sum(covid_subset$tested)

# Get the positive ratio (confirmed / tested)
positive_ratio <- sum(covid_subset$confirmed)/sum(covid_subset$tested)
positive_ratio


# Get the `country` column
countries <- covid_subset$country

# Check its class (should be Factor)
class(countries)

# Conver the country column into character so that you can easily sort them


# Sort the countries AtoZ
AZ <- sort(countries)

# Sort the countries ZtoA
ZA <- sort(countries, decreasing = TRUE)

# Print the sorted ZtoA list
print(ZA)

# Use a regular expression `United.+` to find matches
utd <- grep('United.+', countries)


# Print the matched country names
countries[utd]

# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_subset[covid_subset$country == 'United Kingdom', c('confirmed','country','confirmed.population.ratio')]

# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_subset[covid_subset$country == 'Germany', c('confirmed','country','confirmed.population.ratio')]

# Use if-else statement
# if (check which confirmed.population value is greater) {
#    print()
# } else {
#    print()
# }
if (covid_subset[165,'confirmed.population.ratio'] > covid_subset[60, 'confirmed.population.ratio']) {
  print('United Kingdom')
} else {
  print("Germany")
}

# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
low_ratio_countries <- subset(covid_subset, covid_subset$confirmed.population.ratio < 0.01)
low_ratio_countries[,2]

