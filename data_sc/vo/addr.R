library(lubridate)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(plotly)
library(data.table)

drop_cols <- c("rn", "BaseId", "AddrID", "GeoPrecision", "GeoKind", "VerifyCode",
               "clientid", "AddrCode", "AddrWho", "AddrRemark", "AddrDTM",
               "EditorLogin", #"AddrCitySocr", "AddrStreetSocr", "AddrTownSocr",
               #"AddrSubTownSocr", "AddrAreaSocr", "AddrStateSocr",
               "Latitude", "Longitude", "Source", "UADId", "UADKind"
               #,"AddrStr.."
               )

addr_file <- '../../data/dataset_debts/addr_new1.csv'
addr_df <- read.csv(addr_file,
                    nrows = -1,
                    stringsAsFactors = FALSE,
                    encoding = 'cp1251',
                    fileEncoding = 'cp1251') %>%
  filter((contract_number != '') | (!is.na(clientid))) %>%
  rename(CONTRACT_REF = contract_number) %>%
  select(-one_of(drop_cols))



#pattern <- "(?<![^\\s\\z\\.])[\\s]*((Г[ОO]?[РP]?[OО]?Д?)|([CС]?[EЕ]?Л?[OО]?))[\\s\\z\\.]?(?![^\\s\\z\\.])[\\s\\z\\.]*"
pattern <- "(?<![^\\s\\z\\.])[\\s]*((Г[ОO]?[РP]?[OО]?Д?)|([CС][EЕ]?Л?[OО]?)|(Д[EЕ]?[РP]?[EЕ]?[BВ]?[HН]?Я?)|(П[ОO]?[CС]?[EЕ]?Л?[ОO]?[КK]?)|(ПГ[ТТ])|([PР]П))[\\s\\z\\.]?(?![^\\s\\z\\.])[\\s\\z\\.]*"

addr_df %<>%
  mutate(AddrCity2 = str_replace_all(toupper(AddrCity), pattern, '') %>%
                                    str_trim(),
         AddrTown2 = str_replace_all(toupper(AddrTown), pattern, '') %>%
                                    str_trim(),
         AddrState2 = str_replace_all(toupper(AddrState), pattern, '') %>%
                                    str_trim(),
         AddrStreet2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                            "\\1",
                            toupper(AddrStreet)) %>% str_trim(),
         AddrArea2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                            "\\1",
                            toupper(AddrArea)) %>% str_trim(),
         AddrSubTown2 = gsub(".*Г[ОO]?[РP]?[OО]?[Д]?[[:space:]\\.]+([^[:space:]]+).*",
                          "\\1",
                          toupper(AddrSubTown)) %>% str_trim())

addr_df %<>% 
  unite(FULL_ADDRESS, 
        AddrStateSocr, AddrState2, AddrAreaSocr, AddrArea2, 
        AddrCitySocr, AddrCity2, AddrTownSocr, AddrTown2, 
        AddrSubTownSocr, AddrSubTown2, AddrStreetSocr, AddrStreet2, 
        sep = " ", remove = FALSE)

## geoparser ----
library(jsonlite)

uq_loc <- addr_df %>%
  select(FULL_ADDRESS) %>%
  mutate(FULL_ADDRESS = str_trim(str_replace_all(FULL_ADDRESS, '\\s+', ' '))) %>%
  distinct()

uq_loc$GEO_NAME <- ""
uq_loc$LAT_LON <- ""
uq_loc$FOUND <- 0

for (i in 60001:nrow(uq_loc)) {
  if (mod(i, 200) == 0) {
    print(i)
  }
  full_addr <- str_split(uq_loc[i, 'FULL_ADDRESS'], ' ')[[1]]
  n_words <- length(full_addr)
  ## delete two words until we find the address
  for (j in 0:as.integer(n_words / 2)) {
    curr_addr <- full_addr[1:(n_words - 2 * j)]
    data <- try(fromJSON(paste0("https://geocode-maps.yandex.ru/1.x/?format=json&geocode=",
                                paste0(curr_addr, collapse = ' '))))
    if (class(data) == "try-error") next;
    n_r <- as.numeric(data$response$GeoObjectCollection$metaDataProperty$GeocoderResponseMetaData$found)#nrow(data)
    uq_loc[i, 'FOUND'] <- n_r
    if (n_r > 0) {
      data <- data$response$GeoObjectCollection$featureMember
      uq_loc[i, 'GEO_NAME'] <- data$GeoObject$metaDataProperty$GeocoderMetaData$Address$formatted[1]
      uq_loc[i, 'LAT_LON'] <- data$GeoObject$Point$pos[1]
      break;
    }
  }
}
 
addr_df %<>%
  mutate(FULL_ADDRESS = str_trim(str_replace_all(FULL_ADDRESS, '\\s+', ' '))) 

addr_df %>% dim

addr_df2 <- addr_df %>%
  left_join(uq_loc, by = 'FULL_ADDRESS')
addr_df2 %>% dim

saveTable(addr_df2, 'GeoAddr', './BBKK2', TRUE)
saveTable(addr_df2 %>%
            select(CONTRACT_REF, contacttype,
                   FULL_ADDRESS, GEO_NAME,
                   LAT_LON, FOUND), 'GEO_ADDR', './BBKK2', TRUE)

#   data <- try(fromJSON(paste0("https://geocode-maps.yandex.ru/1.x/?format=json&geocode=",
#                               uq_loc[i, 'FULL_ADDRESS'])))
#   if (class(data) == "try-error") next;
#   n_r <- as.numeric(data$response$GeoObjectCollection$metaDataProperty$GeocoderResponseMetaData$found)#nrow(data)
#   uq_loc[i, 'FOUND'] <- n_r
#   if (n_r > 0) {
#     data <- data$response$GeoObjectCollection$featureMember
#     uq_loc[i, 'GEO_NAME'] <- data$GeoObject$name[1]
#     uq_loc[i, 'LAT_LON'] <- data$GeoObject$Point$pos[1]
#   } else {
#     ## try without 'УЛ'
#     data <- try(fromJSON(paste0("https://geocode-maps.yandex.ru/1.x/?format=json&geocode=",
#                                 gsub("(.*)УЛ.*", "\\1", uq_loc[i, 'FULL_ADDRESS']))))
#     if (class(data) == "try-error") next;
#     n_r <- as.numeric(data$response$GeoObjectCollection$metaDataProperty$GeocoderResponseMetaData$found)#nrow(data)
#     uq_loc[i, 'FOUND'] <- n_r
#     if (n_r > 0) {
#       data <- data$response$GeoObjectCollection$featureMember
#       uq_loc[i, 'GEO_NAME'] <- data$GeoObject$metaDataProperty$GeocoderMetaData$Address$formatted[1]#data$GeoObject$name[1]
#       uq_loc[i, 'LAT_LON'] <- data$GeoObject$Point$pos[1]
#     }
#   }
# }

###


addr_df %<>%
  mutate(AddrCity2 = ifelse(AddrCity2 == '',
                            ifelse(AddrTown2 == '',
                                   ifelse(AddrState2 == '',
                                          ifelse(AddrStreet2 == '',
                                                 ifelse(AddrArea2 == '',
                                                        AddrSubTown2,
                                                        AddrArea2),
                                                 AddrStreet2),
                                          AddrState2),
                                   AddrTown2),
                            AddrCity2),
         AddrCity2 = str_replace_all(AddrCity2, "[[:punct:] ]", ""))

drop_cols <- c("rn", "BaseId", "AddrID", "GeoPrecision", "GeoKind", "VerifyCode",
               "CalipsoDT", "AddrCode", "AddrWho", "AddrRemark", "AddrDTM",
               "EditorLogin", "AddrCitySocr", "AddrStreetSocr", "AddrTownSocr",
               "AddrSubTownSocr", "AddrAreaSocr", "AddrStateSocr",
               "Latitude", "Longitude", "Source", "UADId", "UADKind",
               "AddrStr..")
source('helpers.R')

## assuming that the bigger CallipsoDT, the more recent the record
addr_df %<>% 
  select(contract_number, contacttype, AddrCity2, AddrCountry, CalipsoDT) %>%
  group_by(contract_number, contacttype) %>%
  arrange(desc(CalipsoDT)) %>%
  top_n(1) %>%
  distinct() %>%
  ungroup()

## will only consider types 11, 12, 13
addr_df %<>% filter(contacttype <= 13)
## creating features
createTopNCols(addr_df, 
               top_n_cols = list(c(name = 'AddrCity2', percentage = 0.55),
                                 c(name = 'AddrCountry', percentage = 0.99)), 
               list()) -> out_tmp
addr_df <- out_tmp[[1]]
code_maps <- out_tmp[[2]]
rm(out_tmp)

addr_df %<>%
  select(-CalipsoDT) %>%
  gather(property, value, -contract_number, -contacttype) %>%
  unite(COLUMN, contacttype, property) %>%
  spread(COLUMN, value)

write.table(addr_df, 'data/addr_df.csv', sep = '|', 
            row.names = FALSE, quote = FALSE)
save(code_maps, file = 'data/code_maps_addr.RData')
