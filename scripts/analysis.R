library(tidyverse)
library(ICplots)
library(rvest)


# Search for Provinces
prs <- read_csv("data/gendameria_2023-06-07.csv")
prs$prov <- NA

prov <- read_html("https://es.wikipedia.org/wiki/Provincias_de_Argentina") %>%
  html_elements("td") %>%
  html_elements("b") %>%
  html_elements("a") %>%
  html_attr("title") %>%
  sub("Provincia de ", "", .) %>%
  as.list


for (i in seq_along(prov)) {
  for (j in seq_len(nrow(prs))) {
    if (is.na(prs$prov[[j]])) {
      prs$prov[[j]] <- str_extract(prs$text[[j]], prov[[i]])
  } else if (!is.na(prs$prov[[j]])) {
    match <- str_extract(prs$text[[j]], prov[[i]]) %>%
      unique
    prs$prov[[j]] <- paste(prs$prov[[j]],
                           match,
                           sep = " ")
    }
  }
}

# clean up extra characters fror the poorly written loop above
prs$prov <- gsub("NA", "", prs$prov)
prs$prov <- gsub("\\s+", " ", prs$prov, perl = TRUE)


# find press releases that mention marijuana, add the amount seized if possible
marijuana <- prs %>%
  mutate(date = as.POSIXlt(date),
         title = gsub("[.]", "", title),
         incautaciones = str_extract(title, "([0-9]+ kilos de marihuana)"),
         #incautaciones = str_extract(title, "([0-9]+ [a-z]+ de cocaína)"),
         mari_kilos = as.numeric(str_extract(incautaciones, "[0-9]+")),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         year_month = as.numeric(format(date, format = "%Y.%m")),
         filter_col = grepl("marihuana", text)) %>%
  filter(filter_col == TRUE) %>%
  select(-filter_col)

write_excel_csv(marijuana, "data/gendameria_marijuana_2023-06-07.csv") 
  

# find press releases that mention cocaine, add the amount seized if possible
cocaine <- prs %>%
  mutate(date = as.POSIXlt(date),
         title = gsub("[.]", "", title),
         incautaciones = str_extract(title, "([0-9]+ kilos de cocaína)"),
         #incautaciones = str_extract(title, "([0-9]+ [a-z]+ de cocaína)"),
         coke_kilos = as.numeric(str_extract(incautaciones, "[0-9]+")),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         year_month = as.numeric(format(date, format = "%Y.%m")),
         filter_col = grepl("cocaína", text)) %>%
  filter(filter_col == TRUE) %>%
  select(-filter_col)

write_excel_csv(cocaine, "data/gendameria_cocaine_2023-06-07.csv") 

entre_rios <- prs %>%
  mutate(date = as.POSIXlt(date),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         year_month = as.numeric(format(date, format = "%Y.%m")),
         filter_col = grepl("Entre Ríos", text)) %>%
  filter(filter_col == TRUE) %>%
  select(-filter_col)

write_excel_csv(entre_rios, "data/gendameria_entre_rios_2023-06-07.csv") 

# find press releases that mention tires, add the amount seized if possible
tires <- prs %>%
  mutate(date = as.POSIXlt(date),
         title = gsub("[.]", "", title),
         incautaciones = str_extract(title, "([0-9]+ neumáticos)|([0-9]+ cubiertas)"),
         tires = as.numeric(str_extract(incautaciones, "[0-9]+")),
         year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         year_month = as.numeric(format(date, format = "%Y.%m")),
         filter_col = grepl("neumáticos|cubiertas", text)) %>%
  filter(filter_col == TRUE) %>%
  select(-filter_col)
write_excel_csv(tires, "data/gendameria_tires_2023-06-07.csv") 

time_plot <- ggplot(tires, aes(x = tires)) +
  geom_histogram() +
  theme_ic()
time_plot

# yearly seizures
yearly <- tires %>%
  select(year, tires) %>%
  group_by(year) %>%
  summarize(tires = sum(tires, na.rm = TRUE))

year_plot <- ggplot(yearly, aes(x = year, y = tires)) +
  geom_col(fill = "#B70039") +
  labs(title = "Illegal Tire Seizures") +
  ylab("# of Tires Seized") +
  geom_hline(yintercept = 0, color = "#3B3B3B") +
  theme_ic()
year_plot
finalise_plot(year_plot,
              source_name = "Gendarmería Nacional Argentina",
              save_filepath = "tires_seized.png")

# monthly seizures
monthly <- cocaine %>%
  select(date, year_month, coke_kilos) %>%
  group_by(year_month) %>%
  summarize(coke_kilos = sum(coke_kilos),
            date = min(date))

month_plot <- ggplot(monthly, aes(x = date, y = coke_kilos)) +
  geom_line(group = 1) +
  theme_ic() +
    theme(axis.ticks.x = element_line())
month_plot
