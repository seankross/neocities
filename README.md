# Neocities API in R

Make [neocities.org](https://neocities.org/) part of your publishing workflow! Neocities allows users to have their own webpages with 20MB of [basically](https://neocities.org/site_files/allowed_types) whatever they want to upload! This library provides access to Neocities' REST API according to Hadley Wickham's [best practices](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd) for wrapping an API.

### Installation

```ruby
# install.packages("devtools")
devtools::install_github("seankross/neocities")
```

### Getting started

```ruby
# Sign up for an account at neocities.org
library(neocities)

# upload a file
neocities_upload("~/Desktop/report.html")

# delete a file
neocities_delete("report.html")

# get info on a user
neocities_info(user="username")
```