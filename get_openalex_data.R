library('dplyr')
library('httr')
library('jsonlite')
library('glue')
library('arrow')

# Load Web of Science dataset
dataset <- read.csv('dataset.txt')

# Create a dataset retaining records having a DOI
dataset_doi <- dataset |> 
  filter(DI != "") |>
  select(DI, UT, TI, PY)

# Input fields
endpoint <- 'https://api.openalex.org/works/'
doi_prefix <- 'https://doi.org/'
fields <- '?select=id,doi,display_name,publication_year,open_access'
auth_suffix <- '&mailto=xxxx@gmail.com' # Use your email.

# Init lists
errors <- list()
oa <- list()

# Get data from OpenAlex
for (x in c(1:nrow(dataset_doi))) {
  print(x)
  article_url <- glue("{endpoint}{doi_prefix}{dataset_doi$DI[x]}{fields}{auth_suffix}")
  response <- GET(article_url)
  if (response$status_code == 200) {
    response <- fromJSON(rawToChar(response$content))
    print(response$display_name)
    oa <- c(oa, list(response))
  } else {
    print(glue('error: {x}'))
    errors <- c(errors, list(idx=x, doi=dataset_doi$DI[x]))
  }
  Sys.sleep(0.15)
}

# Save responses and errors
save(oa, errors, file="dataset_openalex_sustainable_food.rdata")

# Format as data frame
df <- as.data.frame(do.call(rbind, oa)) |>
  mutate(
    doi = unlist(doi) |> as.character(),
    display_name = as.character(display_name),#unlist(display_name) |> as.character(),
    publication_year = unlist(publication_year) |> as.character()
  )

# Find the missing data due to DOI errors
errors_idx <- errors[names(errors) == 'idx'] |> 
  unlist() |> 
  unname()

missing_records_errors <- dataset_doi |>
  slice(errors_idx) |>
  select(DI, TI, PY) |>
  rename(
    doi = DI,
    display_name = TI,
    publication_year = PY
  )


# Find the missing data due to DOI missing
missing_records_blank_doi <- dataset |>
  filter(! UT %in% dataset_doi$UT)  |>
  select(DI, TI, PY)  |>
  rename(
    doi = DI,
    display_name = TI,
    publication_year = PY
  )

# Bind dfs
df_complete <- bind_rows(
  df,
  missing_records_errors,
  missing_records_blank_doi
)

# Save object as `.Rdata`
save(df_complete, file="dataset_openalex_sustainable_food.rdata")
