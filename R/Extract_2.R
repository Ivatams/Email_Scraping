library(pdftools)
library(rvest)

# Read the PDF file into R
pdf_file <- "Remote Friendly Companies.pdf"
pdf_table <- pdf_text(pdf_file)

# Extract the table data from the PDF into a data frame
pdf_table_df <- read.table(text = pdf_table, sep = "\t", header = TRUE)

# Use rvest to extract the website URLs from the data frame
websites <- list()
for (i in 1:nrow(pdf_table_df)) {
  company_name <- pdf_table_df[i, "Name"]
  website_url <- pdf_table_df[i, "Website"]
  region <- pdf_table_df[i, "Region"]
  
  # Use rvest to extract the website URL from the HTML code
  print(paste("Processing row", i, "with URL", website_url))
  if (is.null(website_url) || website_url == "") {
    websites[[i]] <- NA
  } else {
    website_html <- read_html(website_url)
    website <- website_html %>% html_nodes("a") %>% html_attr("href") %>% head(1)
    websites[[i]] <- website
  }
  
  
}

# Use httr to send requests to each website URL and retrieve the HTML code
html_codes <- list()
for (i in 1:length(websites)) {
  website <- websites[[i]]
  if (!is.na(website)) {
    response <- GET(website)
    html_code <- content(response, "text")
    html_codes[[i]] <- html_code
  } else {
    html_codes[[i]] <- NA
  }
}

# Use rvest to extract the email addresses from the HTML code
email_addresses <- list()
for (i in 1:length(html_codes)) {
  html_code <- html_codes[[i]]
  if (!is.na(html_code)) {
    email_address <- html_code %>% html_nodes("a[href^='mailto:']") %>% html_text()
    email_addresses[[i]] <- email_address
  } else {
    email_addresses[[i]] <- NA
  }
}

# Combine the data into a single data frame
result <- data.frame(Name = pdf_table_df$Name,
                     Website = unlist(websites),
                     Email = unlist(email_addresses))

# View the results
View(result)
