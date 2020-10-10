# Html-text-processing
Fetching and parsing financial reports on Edgar
---
title: "Text parsing and data preparation"
output:
  pdf_document: default
  html_notebook: default
editor_options:
  chunk_output_type: console
---


```{r libraries, warning=FALSE, message=FALSE}
library(edgar)
library(readtext)
library(docxtractr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(XML)
library(stringr)
library(RSQLite)
library(tm)
library(BatchGetSymbols)
```

# Data preparation
The list of companies included in the S&P 500 was provided in the word document of individual assignment description.
```{r listSP500, eval=FALSE}
# Extract table from docx 
dat_word_tbl <- docx_extract_tbl(read_docx("individual_assignment_companies.docx"))
dat_word_tbl$GICS.Sector <- as.factor(dat_word_tbl$GICS.Sector)

# Reformat CIK column as factor
dat_word_tbl$CIK <- as.numeric(dat_word_tbl$CIK)
# Replace dot with dash as stock symbols on Yahoo Finance do not include dots
dat_word_tbl$Y_symbol <- gsub("\\.", "-", dat_word_tbl$Symbol)
dat_word_tbl <- dat_word_tbl %>%
  plyr::rename(c("GICS.Sector" = "GICS_sector",
                 "GICS.Sub.Industry" = "GICS_sub_industry"))

# Save as rda file
save(dat_word_tbl, file = "company_full_list.rda")
```

Using edgar package, the loop function will download filings for companies in SP500 list. Here I choose to work with a smaller sample including 3 industry sectors: Industrials, Health Care, and Information Technology due to free disk space limitation. This sample comprises 201 companies, which is a half of the original full list. The total downloaded filings are 6,432 files.
```{r gettext, eval=FALSE}
# Working on a smaller sample due to free disk space limitation
company_set <- dat_word_tbl %>%
  filter(GICS.Sector %in% c("Industrials", "Health Care", "Information Technology"))

for (i in 1:nrow(company_set)) {
  this_cik <- company_set$CIK[i]
  getFilings(this_cik, form.type = c("10-K", "10-Q"),
                 2009:2019, quarter = c(1,2,3), downl.permit = "y")
  print(i)
}
```

# Text parsing

The parsing process will be conducted with txt files directly instead of html files as this would reduce the downloading time (the getFilingsHTML function will download text files then parse to html format, doubling the processing time as well as storing spaces). This approach was introduced by Hering, 2016.
(Hering, J., 2016. The annual report algorithm: Retrieval of financial statements and extraction of textual information. Available at SSRN 2870309.)
```{r function}
# As there is no ready function to extract text from 10-Q form, the following function
# is defined to break down and remove mark up html language.
text_10k_clean <- function(x) {
  readLines(x) %>%
  str_c(collapse = " ") %>% # Step 1
  str_extract(pattern = "(?s)(?m)<TYPE>10-K.*?(</TEXT>)") %>% # Step 2
  str_replace(pattern = "((?i)<TYPE>).*?(?=<)", replacement = "") %>% # Step 3
  str_replace(pattern = "((?i)<SEQUENCE>).*?(?=<)", replacement = "") %>% 
  str_replace(pattern = "((?i)<FILENAME>).*?(?=<)", replacement = "") %>% 
  str_replace(pattern = "((?i)<DESCRIPTION>).*?(?=<)", replacement = "") %>%
  str_replace(pattern = "(?s)<HEAD>.*?</HEAD>", replacement = "") %>% # Step 4
  str_replace_all(pattern = "(?s)<[^>]*>", replacement = " ") %>% # Step 5
  str_replace_all(pattern = "&(.{2,6});", replacement = " ") %>% # Step 6
  str_replace_all(pattern = "(?s) +", replacement = " ") # Step 7
}

text_10q_clean <- function(x) {
  readLines(x) %>%
  str_c(collapse = " ") %>% # Step 1
  str_extract(pattern = "(?s)(?m)<TYPE>10-Q.*?(</TEXT>)") %>% # Step 2
  str_replace(pattern = "((?i)<TYPE>).*?(?=<)", replacement = "") %>% # Step 3
  str_replace(pattern = "((?i)<SEQUENCE>).*?(?=<)", replacement = "") %>% 
  str_replace(pattern = "((?i)<FILENAME>).*?(?=<)", replacement = "") %>%
  str_replace(pattern = "((?i)<DESCRIPTION>).*?(?=<)", replacement = "") %>%
  str_replace(pattern = "(?s)<HEAD>.*?</HEAD>", replacement = "") %>% # Step 4
  str_replace_all(pattern = "(?s)<[^>]*>", replacement = " ") %>% # Step 5
  str_replace_all(pattern = "&(.{2,6});", replacement = " ") %>% # Step 6
  str_replace_all(pattern = "(?s) +", replacement = " ") # Step 7
}
```
Step 1: readLines() will read the text file line by line then the str_c() function will combine all string vectors into one-element vector.
Step 2: The str_extract() function takes in the one-element vector of text, and returns only the text corresponding to the beginning and end of the document, specified by 10-K and 10-Q, respectively.
Step 3: These steps will delete the elements that are used to format the document, including document type, sequence information, file name and description, as they are not relevant data.
Step 4: Removal of head section (including document title).
Step 5: Removal of HTML tags and attributes.
Step 6: Replacement of UNICODE strings with spaces.
Step 7: Replacement of multiple spaces with one single space.

```{r parsing10K, eval=FALSE}

list_10k <- list.files(path = "Edgar filings_full text/Form 10-K/",
                       full.names = TRUE, recursive = TRUE)

for (l in 1:length(list_10k)) {
  this_file_text <- text_10k_clean(list_10k[l])
  # Remove digits as well as punctuations
  this_file_text_clean <- gsub("[^a-zA-Z ]", "", this_file_text) %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
    tolower() %>%
    stripWhitespace()
  # Keep file name
  file <- basename(list_10k[l])
  
  text_file <- data.frame(this_file_text_clean, file)
 # Separate file name to identifiers
  text_file <- text_file %>% separate(file, c("CIK", "form_type", "date_filed", "doc_id"),
                                      sep = "_", remove = FALSE)
  print(l)
  dbWriteTable(conn, name = "edgars_full_text", value = text_file, append = TRUE)
}
```

```{r parsing10q, eval=FALSE}
# Listing 10-Q files
list_10q <- list.files(path = "Edgar filings_full text/Form 10-Q/",
                       full.names = TRUE, recursive = TRUE)
# Looping through 10q files
for (m in 1:length(list_10q)) {
  this_file_text <- text_10q_clean(list_10q[m])
  # Remove digits as well as punctuations
  this_file_text_clean <- gsub("[^a-zA-Z ]", "", this_file_text) %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
    tolower() %>%
    stripWhitespace()
  # Keep file name
  file <- basename(list_10q[m])
  
  text_file <- data.frame(this_file_text_clean, file)
  # Separate file name to identifiers
  text_file <- text_file %>% separate(file, c("CIK", "form_type", "date_filed", "doc_id"),
                                      sep = "_", remove = FALSE)
  print(m)
  dbWriteTable(conn, name = "edgars_full_text", value = text_file, append = TRUE)
}
# write(this_file_text_clean, file = "cleaned_example.txt")
```

Now as all the processed text are saved in the database, this will be reserved as backup for all analysis. An example of the processed text is included in the submission file.
```{r fetch_text, eval=FALSE}
# Fetching all text 
company_all_text <- dbGetQuery(conn, "SELECT * FROM edgars_full_text")
company_all_text$CIK <- as.numeric(company_all_text$CIK)
company_all_text$date_filed <- ymd(company_all_text$date_filed)

# Joining with company list
company_all_text <- company_all_text %>%
  left_join(company_full_list, by = "CIK")

# Saving the final file in rda format
save(company_all_text, file = "company_all_text.rda")
```


Next, the stock prices of all companies within the analysing timeframe will be collected using BatchGetSymbols package. The timeframe is defined by one week before and one week after the filing dates of both 10-K and 10-Q forms. This period would ensure sufficient stock price input, as the days right before or after the filing dates could be non-trading weekends or national holidays.
```{r stockprice, eval=FALSE}
load("company_all_text.rda")

company_stock_price <- data.frame()

# Getting stock prices nrow(company_all_text)
for (s in 1:nrow(company_all_text)) {
  # Setting up timeframe 
  this_stock <- company_all_text$Y_symbol[s]
  left_bound <- company_all_text$date_filed[s] - 7
  right_bound <- company_all_text$date_filed[s] + 7
  # Downloading stock prices
  this_stock_price <- BatchGetSymbols(this_stock, first.date = left_bound,
                                      last.date = right_bound, be.quiet = TRUE)
  
  # Binding all stocks to one dataframe
  company_stock_price <- bind_rows(company_stock_price, this_stock_price)
}
company_stock_price <- company_stock_price %>%
  select(ticker, price.open:ret.closing.prices) %>%
  na.omit()

save(company_stock_price, file = "company_stock_price.rda")
```


# Customise stop-word list

The DateandTime as well as Geographic files are context-specific stopword lists obtained via the University of Notre Dame website. (https://sraf.nd.edu/textual-analysis/resources/#StopWords)
```{r stopword, eval=FALSE}
# Getting company names
company_name <- company_all_text %>%
  select(Security) %>%
  unique(.) %>%
  mutate(full_name = gsub("[[:punct:]]+", " ", Security)) %>%
  select(-Security)

company_name <- company_name %>%
  unnest_tokens(word, full_name) %>%
  mutate(lexicon = "CUSTOM")

# Geographic stopwords
geographic_stopword <- as.data.frame(readLines("StopWords_Geographic.txt")) %>%
  setNames("word") %>%
  mutate(lexicon = "CUSTOM")

# Date and time stopwords
datetime_stopword <- as.data.frame(readLines("StopWords_DatesandNumbers.txt")) %>%
  setNames("word") %>%
  mutate(lexicon = "CUSTOM")

# Other words that are highly repeated in 10-k and 10-q forms
other_stopword <- data.frame(word = c("company", "companys", "consolidated", "financial",
                                      "finance", "percent", "fiscal", "statement", 
                                      "chapter", "item", "part", "section", "contents",
                                      "segment", "note", "accounting", "statements",
                                      "condensed", "gaap", "nongaap", "pension"),
                             lexicon = "CUSTOM")

# Combine with current tidytext stop_words dataframe
stop_words_n <- tidytext::stop_words %>% rbind(company_name, other_stopword,
                                               geographic_stopword, datetime_stopword)

# save(stop_words_n, file = "stop_word_n.rda")
```

# Tokenise the text by year

In this step, the full text of each filing will be tokenised then filtered by the customised stop word list. Also, too long or too short tokens will also be filtered, keeping only tokens with length between 3 and 13 characters. This additional cleaning process is necessary as too long or too short words are often outliers and/or abbreviations that contain little meaning.
```{r tokenise, eval=FALSE}
# Constructing tokenised data for each year from 2009 to 2019
# so that R memory is not exhausted
# load("stop_word_n.rda")
for(y in 2009:2019) {
this_year_token <- company_all_text %>%
  select(this_file_text_clean, doc_id, GICS_sector, Y_symbol,
         date_filed, CIK, form_type) %>%
  mutate(this_year = year(date_filed)) %>%
  filter(this_year == y) %>%
  unnest_tokens(word, this_file_text_clean) %>%
  mutate(token_length = nchar(word)) %>%
  filter(between(token_length, 3, 13)) %>%  # Filter too long/too short tokens
  anti_join(stop_words_n)

# Save the tokenised file for further analysis
save(this_year_token, file = paste0("year_",y,"_token.rda"))
print(y)
rm(this_year_token) # clear up memory
}
```
