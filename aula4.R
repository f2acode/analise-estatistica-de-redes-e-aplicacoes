# http://dontpad.com/redescarvalho
# Burglaries = entrar na casa das pessoas sem permissão

# fazer o download no site dos datasets
# book: https://r4ds.had.co.nz/

# simples example
library(tidyverse)

tb <- tibble(id = gl(2, 3), x = rpois(6, 6))
tb <- tibble(id = gl(3, 2), x = rpois(6, 6))

tibble(x = 1:100)
mutate(tb, is_even = x %% 2 == 0)

# STARTING DATA SCIENCE
(all_crimes <- read_csv("./data/crimes_boston.csv"))

library(lubridate)

burglary_codes <- c(520, 521, 522) # residential burglaries
(burglaries <- all_crimes %>%
    filter(as.numeric(OFFENSE_CODE) %in% burglary_codes) %>%
    select(INCIDENT_NUMBER, OFFENSE_CODE, OCCURRED_ON_DATE, Lat, Long) %>%
    filter(!is.na(Lat) & !is.na(Long)) %>% # avoid occurrences without GPS record
    mutate(YEAR = year(OCCURRED_ON_DATE)) %>% # add YEAR field from date
    filter(between(YEAR, 2016, 2018)))

# poisson não deu um bom ajuste no modelo na primeira tentativa
# normalmente o fi é 1, mas deu 29.16 ...

# é necessário pensar em como remover os 0's e outliers

# st_buffer = defini uma borda ao redor de algum valor
# tomar cuidado para não ser contado duas vezes (na esquina)

# depois ele pega as coisas da rua e mapeia para as intersecções (esquinas)

# inflação por zeros - pesquisar, não foi feito em sala

# tentar fazer com dados de são paulo (talvez do meu bairro?)

# http://math.bu.edu/people/lecarval/ 
# hierarchical modeling and analysis for spatial data