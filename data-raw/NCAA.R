NCAAbb <- readr::read_csv("ncaa-bb.csv") %>%
  filter(!is.na(date)) %>%   # there were three blank lines in the data, this removes them
  rename(hscore = hs, ascore = as, notes = n) %>% 
  mutate(
    date = lubridate::mdy(date),
    season = derivedVariable(
      "2006-07" = between(date, lubridate::mdy("9/1/2006"), lubridate::mdy("5/1/2007")),
      "2007-08" = between(date, lubridate::mdy("9/1/2007"), lubridate::mdy("5/1/2008")),
      "2008-09" = between(date, lubridate::mdy("9/1/2008"), lubridate::mdy("5/1/2009")),
      "2009-10" = between(date, lubridate::mdy("9/1/2009"), lubridate::mdy("5/1/2010")),
      "2010-11" = between(date, lubridate::mdy("9/1/2010"), lubridate::mdy("5/1/2011")),
      "2011-12" = between(date, lubridate::mdy("9/1/2011"), lubridate::mdy("5/1/2012")),
      "2012-13" = between(date, lubridate::mdy("9/1/2012"), lubridate::mdy("5/1/2013")),
      "2013-14" = between(date, lubridate::mdy("9/1/2013"), lubridate::mdy("5/1/2014")),
      "2014-15" = between(date, lubridate::mdy("9/1/2014"), lubridate::mdy("5/1/2015")),
      "2015-16" = between(date, lubridate::mdy("9/1/2015"), lubridate::mdy("5/1/2016")),
      NA),
    postseason = grepl("p", notes)
  )

devtools::use_data(NCAAbb, overwrite = TRUE)