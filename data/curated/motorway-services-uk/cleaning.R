# Paste code used to create the curated dataset here. Include comments as
# necessary. If you did not need to clean the data, use a comment like the one
# below, but also load the data with readr::read_csv() to ensure the data can be
# loaded, and to use with `saving.R`. Delete this block of comments.


library("tidyverse")
library("rvest")
library("readxl")

read_motorway_services_info <- function(file_path){
  name_service_station <- str_remove(basename(file_path), " - Motorway Services Information.html")
  
  read_html(file_path) %>% 
    html_nodes(".infotext") %>% 
    html_text() %>% 
    tibble(
      info = .
    ) %>%
    separate_wider_delim(info, ":", names = c("property", "value"), too_many = "merge") %>%
    mutate(value = str_trim(value)) %>%
    mutate(service_station = name_service_station) %>% 
    identity()
}

# I've manually downloaded the webpage for 106 services listed here
# https://www.motorwayservices.info/list/name
# Data can't be scraped with {rvest} unfortunately
# Data is licensed under a Creative Commons ANCS 2.0 License as detailed here
# https://www.motorwayservices.info/site/about

data_raw_services <- list.files("data/curated/motorway-services-uk/service-stations/", "[.]html", full.names = TRUE) %>% 
  map_dfr(~read_motorway_services_info(.x))

# Identify paired services via the Eat-In Food property
vec_eat_in_pairs <- data_raw_services %>% 
  filter(property == "Eat-In Food",
         str_detect(value, "Northbound|Eastbound")) %>% 
  pull(service_station)

# I've manually categorised the retailer types
data_type_of_retailer <- read_excel("data/curated/motorway-services-uk/retailer_types.xlsx")

# I've obtained the long, lat coords for the service stations from Google 
# manually because it gets complicated with paired/twinned services
data_raw_service_locations <- read_excel("data/curated/motorway-services-uk/services-locations.xlsx")


# Manual manipulation -----------------------------------------------------

# For confusing reasons, Westmorland data is inconsistently collected. 
# Happendon, Tebay, and Gloucester all have the same food retailers but only
# Gloucester has the data recorded. So let's add ot in manually.

data_manual_westermorland <- data_raw_services %>% 
  filter(service_station == "Gloucester Services Northbound M5",
         property %in% c("Eat-In Food", "Takeaway Food / General")) %>% 
  mutate(service_station = c("Happendon Services M74|Tebay Services Northbound M6|Tebay Services Southbound M6")) %>% 
  separate_longer_delim(service_station,
                        delim= "|")

data_raw_services <- data_raw_services %>% 
  bind_rows(data_manual_westermorland)


# J38 Truckstop isn't really a service station!
data_raw_services <- data_raw_services %>% 
  filter(service_station != "J38 Truckstop M6")

# The Costas at Thurrock Services M25 and Trowell Services M1 have been accidentally ommitted.

data_raw_services <- data_raw_services %>% 
  bind_rows(tibble(
    property = "Takeaway Food / General",
    value = "Costa",
    service_station = c("Thurrock Services M25", "Trowell Services M1")
  ))

# Retail info -------------------------------------------------------------

data_raw_eat_in <- data_raw_services %>% 
  filter(property == "Eat-In Food") %>% 
  mutate(directional = str_detect(value,
                                  "Northbound|Eastbound"))

data_raw_directional_eat <- data_raw_eat_in %>% 
  filter(directional == TRUE) %>% 
  mutate(direction = case_when(
    str_detect(value, "Northbound") ~ "Northbound|Southbound",
    str_detect(value, "Eastbound") ~ "Eastbound|Westbound"
  )) %>% 
  separate_longer_delim(direction,
                        delim = "|") %>% 
  mutate(value = case_when(
    direction == "Northbound" ~ str_extract(value,
                                            "(?<=Northbound: ).*(?=Southbound)"),
    direction == "Southbound" ~ str_extract(value, "(?<=Southbound).*"),
    direction == "Eastbound" ~ str_extract(value,
                                           "(?<=Eastbound: ).*(?=Westbound)"),
    direction == "Westbound" ~ str_extract(value,
                                           "(?<=Westbound: ).*")
  )) 

data_raw_directional_eat <- data_raw_directional_eat %>% 
  mutate(value = str_remove(value, ":"),
         value = str_remove(value, " Northbound.*"),
         value = str_trim(value))

data_raw_directionless_eat <- data_raw_eat_in %>% 
  filter(directional == FALSE) %>% 
  mutate(value = str_remove(value, ":|;"),
         value = str_remove(value, "(Westbound)"),
         value = str_trim(value),
         direction = "Directionless")

data_clean_eat_in <- data_raw_directionless_eat %>% 
  bind_rows(data_raw_directional_eat) %>% 
  select(-directional)

fn_fix_value_columns <- function(data){
  data %>% 
    mutate(value = case_when(
      str_detect(tolower(value), "arlo") ~ "Arlo's", 
      str_detect(tolower(value), "^bk$") ~ "Burger King",
      str_detect(tolower(value), "cotton") ~ "Cotton Traders", 
      str_detect(tolower(value), "chozen") ~ "Chozen Noodles", 
      str_detect(tolower(value), "cornwall") ~ "West Cornwall Pasty Company", 
      str_detect(tolower(value), "costa") ~ "Costa", 
      str_detect(tolower(value), "eat & drink co") ~ "Eat & Drink Co", 
      str_detect(tolower(value), "edc") ~ "Eat & Drink Co", 
      str_detect(tolower(value), "fone") ~ "FoneBiz", 
      str_detect(tolower(value), "Food to Go") ~ "Food to Go - Farm Cafe",
      str_detect(tolower(value), "full house") ~ "Full House",
      str_detect(tolower(value), "greg") ~ "Greggs", 
      str_detect(tolower(value), "harry") ~ "Harry Ramsden's", 
      str_detect(tolower(value), "hot food co") ~ "Hot Food Co",
      str_detect(tolower(value), "krispy") ~ "Krispy Kreme", 
      str_detect(tolower(value), "le petit") ~ "Le Petit Four", 
      str_detect(tolower(value), "lp4") ~ "Le Petit Four", 
      str_detect(tolower(value), "lucky coin") ~ "Lucky Coin", 
      str_detect(tolower(value), "m&s") ~ "M&S", 
      str_detect(tolower(value), "marks") ~ "M&S", 
      str_detect(tolower(value), "mcdona") ~ "McDonald's", 
      str_detect(tolower(value), "papa john") ~ "Papa John's", 
      str_detect(tolower(value), "pizza hut") ~ "Pizza Hut", 
      str_detect(tolower(value), "quicksilver") ~ "Quicksilver", 
      str_detect(tolower(value), "regus") ~ "Regus Business Lounge", 
      str_detect(tolower(value), "restbite") ~ "Restbite", 
      str_detect(tolower(value), "soho") ~ "SOHO Coffee Co", 
      str_detect(tolower(value), "spar") ~ "SPAR", 
      str_detect(tolower(value), "starbucks") ~ "Starbucks", 
      str_detect(tolower(value), "the burger") ~ "The Burger Company", 
      str_detect(tolower(value), "top gift") ~ "Top Gift", 
      str_detect(tolower(value), "tourist information") ~ "Tourist Information", 
      str_detect(tolower(value), "upper") ~ "Upper Crust", 
      str_detect(tolower(value), "whs") ~ "WHSmiths", 
      str_detect(tolower(value), "wild") ~ "Wild Bean Cafe", 
      tolower(value) %in% tolower(c("WH Smith", "WHSMiths", "Whsmith","W H Smiths", "W.H.Smiths", "W H Smith", "WH Smiths", "Wh Smith", "WH smith")) ~ "WHSmiths", 
      value == "Buger King" ~ "Burger King", 
      value == "M & S Simply food" ~ "M&S",
      TRUE ~ value
    ))
}

data_long_eat_in <- data_clean_eat_in %>% 
  separate_longer_delim(value,
                        delim = ",") %>% 
  mutate(value = str_trim(value)) %>% 
  filter(value != "") %>% 
  fn_fix_value_columns() %>% 
  select(retailer = value,
         service_station,
         direction)

data_long_other_shops_directionless <- data_raw_services %>% 
  filter(!service_station %in% vec_eat_in_pairs) %>% 
  filter(property %in% c("Takeaway Food / General", "Other Non-Food Shops")) %>% 
  select(value, service_station) %>% 
  filter(value != "01823680370") %>% 
  separate_longer_delim(value,
                        delim = ",") %>% 
  mutate(value = str_trim(value, side = "both")) %>% 
  fn_fix_value_columns() %>% 
  mutate(value = str_remove(value, "[(].*y[)]"),
         value = str_trim(value)) %>% 
  reframe(retailer = value,
          service_station = service_station,
          direction = "Directionless")

## There's one bad record
data_long_other_shops_directionless <- tibble(
  retailer = c("Gamezone", "WHSmiths", "Waitrose"),
  service_station = "Newport Pagnell Services M1",
  direction = "Directionless"
) %>%
  bind_rows(filter(
    data_long_other_shops_directionless,!str_detect(retailer, "24hr Gamezone WHSmith & Waitrose")
  ))

## Expand out the twins
data_long_other_shops_w_direction <- data_raw_services %>% 
  filter(service_station %in% vec_eat_in_pairs) %>% 
  filter(property %in% c("Other Non-Food Shops", "Takeaway Food / General")) %>% 
  select(value, service_station) %>% 
  mutate(direction = case_when(
    service_station == "Rownhams Services M27" ~ "Eastbound;Westbound",
    TRUE ~ "Northbound;Southbound"
  )) %>% 
  separate_longer_delim(direction,
                        delim = ";") %>% 
  fn_fix_value_columns() %>% 
  rename(retailer = value)

data_long_retailers <- bind_rows(data_long_eat_in, data_long_other_shops_w_direction, data_long_other_shops_directionless)

data_services_retailers <- data_long_retailers %>% 
  left_join(data_type_of_retailer) %>% 
  mutate(across(starts_with("is"), ~ case_when(
    .x == "Y" ~ TRUE,
    .x == "N" ~ FALSE,
    TRUE ~ NA
  )))


# Non-retailer information ------------------------------------------------

data_wide_services <- data_raw_services %>% 
  filter(property %in% c("Motorway",
                         "Where",
                         "Postcode",
                         "Type",
                         "Operator",
                         "Parking Charges",
                         "LPG available",
                         "Electric Charge Point")) %>% 
  mutate(property = case_when(
    property == "LPG available" ~ "has_lpg",
    property == "Electric Charge Point" ~ "has_electric_charge",
    TRUE ~ property
  )) %>% 
  mutate(value = str_replace_all(value, "ï��[0-9]{1,}", "£"),
         value = str_remove_all(value, "Â")) %>% 
  pivot_wider(names_from = property,
              values_from = value) %>% 
  janitor::clean_names() %>% 
  mutate(is_single_site = str_detect(type, "Single site"),
         is_twin_station = str_detect(type, "Separate facilities"),
         has_walkway_between_twins = case_when(
           is_twin_station == TRUE & str_detect(type, "linked") ~ TRUE,
           is_twin_station == TRUE & str_detect(type, "no link") ~ FALSE,
           TRUE ~ NA),
         is_ireland = str_detect(service_station, "Ireland")) %>% 
  filter(is_ireland == FALSE) %>% 
  reframe(
    name = service_station,
    motorway,
    where,
    postcode,
    type,
    operator,
    p_charges = parking_charges,
    has_charge = has_electric_charge,
    is_single = is_single_site,
    is_twin = is_twin_station,
    has_walk = has_walkway_between_twins
  )

data_paired_services <- data_wide_services %>% 
  filter(is_single == FALSE) %>% 
  filter(str_detect(name, "North|South|East|West")) %>% 
  select(name) %>% 
  separate_wider_delim(name, delim = "Services",
                       names = c("name", "direction")) %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  separate_wider_delim(direction,
                       delim = " ",
                       names = c("direction",
                                 "motorway"),
                       too_few = "align_end") %>% 
  add_count(name, motorway) %>% 
  filter(n > 1) %>% 
  reframe(name = paste(name, "Services", direction, motorway),
          pair_name = paste(name, motorway))

data_services_info <- data_wide_services %>% 
  left_join(data_paired_services) %>% 
  mutate(is_pair = ifelse(is.na(pair_name), FALSE, TRUE))


# Locations ---------------------------------------------------------------

data_clean_long_lat <- data_raw_service_locations %>% 
  separate_wider_delim(google_pin,
                       delim = ",", 
                       names = c("lat", "long")) %>% 
  mutate(long = as.numeric(long),
         lat = as.numeric(lat)) %>% 
  select(name, long, lat)

data_service_locations <- data_services_info %>% 
  left_join(data_clean_long_lat) %>% 
  select(name, long, lat, everything())


# Operators ---------------------------------------------------------------

data_process_ops_single <- data_services_info %>% 
  select(name, operator, is_single) %>% 
  count(operator, is_single) %>% 
  filter(is_single == TRUE) %>% 
  reframe(operator,
          n_single_sites = n)

data_process_ops_twins <- data_services_info %>% 
  count(operator, is_twin) %>% 
  filter(is_twin == TRUE) %>% 
  reframe(operator,
          n_twins = n)

data_process_ops_pair <- data_services_info %>% 
  select(name, operator, is_pair) %>% 
  count(operator, is_pair) %>% 
  filter(is_pair == TRUE) %>% 
  reframe(operator,
          n_pairs = n)

data_process_ops_simple_all <- data_services_info %>% 
  count(operator, name = "n_named_sites_all") 

data_operators <- data_process_ops_simple_all %>% 
  left_join(data_process_ops_single) %>% 
  left_join(data_process_ops_twins) %>% 
  left_join(data_process_ops_pair) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  select(
    operator,

    everything())
