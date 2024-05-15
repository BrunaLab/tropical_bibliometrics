# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
# library(tidystringdist)

# Load journals raw data  -----------------
journals<-read_csv(here("data","data_raw","jrnls.csv")) %>%
  mutate_all(as_factor)

# save to the data_archive folder

write_csv(journals,here("data","data_archive","jrnls.csv"))


