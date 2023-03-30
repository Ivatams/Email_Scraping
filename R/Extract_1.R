library(rvest)
library(stringr)

# Read PDF table into R
pdf_file <- "Remote Friendly Companies.pdf"
pdf_table <- pdf_text(pdf_file)
pdf_table_df <- read.table(text = pdf_table, sep = "\t", header = TRUE)

# Initialize empty lists for websites and emails
websites <- vector("list", length = nrow(pdf_table_df))
emails <- vector("list", length = nrow(pdf_table_df))

websites
# Loop through rows of PDF table
for (i in 1:nrow(pdf_table_df)) {
  
  # Extract company name, website, and region
  company_name <- pdf_table_df[i, "Name"]
  website_url <- pdf_table_df[i, "Website"]
  region <- pdf_table_df[i, "Region"]
  
  # Use rvest to extract the website URL from the HTML code
  website_html <- tryCatch(
    {
      read_html(website_url)
    },
    error = function(e) {
      NULL
    }
  )
 
  # Extract the first website link from the HTML
  website <- NA
  if (!is.null(website_html)) {
    website <- website_html %>% html_nodes("a") %>% html_attr("href") %>% head(1)
  }
  websites[[i]] <- website
  
  # Use regular expressions to extract email addresses from the website HTML
  email <- NA
  if (!is.na(website)) {
    website_html_text <- html_text(website_html)
    email_pattern <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
    email_matches <- str_extract_all(website_html_text, email_pattern)
    if (length(email_matches[[1]]) > 0) {
      email <- email_matches[[1]][1]
    }
  }
  emails[[i]] <- email
}

# Combine results into data frame
result <- data.frame(Name = pdf_table_df$Name,
                     Website = unlist(websites),
                     Email = unlist(emails),
                     Region = pdf_table_df$Region)
