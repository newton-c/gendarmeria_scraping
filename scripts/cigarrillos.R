
prs <- read_csv("data/gendarmeria_2023-09-25.csv")

# find press releases talking about contraband cigarettes
cigarrillos <- prs %>%
  mutate(title = sub("[.]", "", title),
         date = as.POSIXlt(date),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         year_month = as.numeric(format(date, format = "%Y.%m")),
         seized = str_extract(title, "[0-9]+ [a-z]+ de cigarrillos")) %>%
  filter(grepl("cigarrillos", text) == TRUE) 

