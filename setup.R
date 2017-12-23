#### librarys ####
library(dplyr)

#### Falls die Daten aus einem Googlesheet kommen ####
# Wenn die Daten von woanders kommen variiert das selbstverständlich
# z.B. Social Science Survey (https://www.soscisurvey.de/) liefert den setup teil für R mit
# dann müsst ihr den Code erst ab Zeile 22 ausführen
library(googlesheets)

# get the right sheet
neo_ffi <- gs_title(x = "NEO-FFI")

# read the right sheet
neoffi <- gs_read(ss = neo_ffi)

# convert to a data frame
neo <- as.data.frame(neoffi)

names(neo) <- c("timestamp", "test_id", paste0("item", 1:60))


#### Umpolung ####
# Funktion schreiben
pol <- function(x) {
  x <- (x * -1 ) + 4
  return(x)
}

# und anwenden
neo <- neo %>% mutate(
  item1  = pol(item1), item3  = pol(item3),
  item8  = pol(item8), item9  = pol(item9),
  item12 = pol(item12), item14 = pol(item14),
  item15 = pol(item15), item16 = pol(item16),
  item18 = pol(item18), item23 = pol(item23),
  item24 = pol(item24), item27 = pol(item27),
  item29 = pol(item29), item30 = pol(item30),
  item31 = pol(item31), item33 = pol(item33),
  item38 = pol(item38), item39 = pol(item39),
  item42 = pol(item42), item44 = pol(item44),
  item45 = pol(item45), item46 = pol(item46),
  item48 = pol(item48), item54 = pol(item54),
  item55 = pol(item55), item57 = pol(item57),
  item59 = pol(item59)
  )

#### Rohtestwerte ####
neo <- neo %>% mutate(
  neuro_raw  = select(neo, c(ends_with("1"), ends_with("6"))) %>% rowSums(na.rm = T),
  extra_raw  = select(neo, c(ends_with("2"), ends_with("7"))) %>% rowSums(na.rm = T),
  offen_raw  = select(neo, c(ends_with("3"), ends_with("8"))) %>% rowSums(na.rm = T),
  vertr_raw  = select(neo, c(ends_with("4"), ends_with("9"))) %>% rowSums(na.rm = T),
  gewiss_raw = select(neo, c(ends_with("5"), ends_with("0"))) %>% rowSums(na.rm = T)
)


#### Anzahl der beantworteten Fragen ####
neo <- neo %>% mutate(
  quest_n = 12 - select(neo, c(ends_with("1"), ends_with("6"))) %>% is.na() %>% rowSums(),
  quest_e = 12 - select(neo, c(ends_with("2"), ends_with("7"))) %>% is.na() %>% rowSums(),
  quest_o = 12 - select(neo, c(ends_with("3"), ends_with("8"))) %>% is.na() %>% rowSums(),
  quest_v = 12 - select(neo, c(ends_with("4"), ends_with("9"))) %>% is.na() %>% rowSums(),
  quest_g = 12 - select(neo, c(ends_with("5"), ends_with("0"))) %>% is.na() %>% rowSums()
)


#### Mean ####
neo <- neo %>% mutate(
  neu = neuro_raw / quest_n,
  ext = extra_raw / quest_e,
  off = offen_raw / quest_o,
  ver = vertr_raw / quest_v,
  gew = gewiss_raw / quest_g
)

#### Testwert ####
neo <- neo %>% mutate(
  neuro = neu * 12,
  extra = ext * 12,
  offen = off * 12,
  vertr = ver * 12,
  gewiss = gew * 12
)

#### Speichern ####
# Mit Alles
saveRDS(object = neo, file = "NEOFFI.rds")

# Nur mit timestamp und fertigen Testwerten
saveRDS(object = neo[ , c(1, 73:77)], file = "neo_shiny.rds")
