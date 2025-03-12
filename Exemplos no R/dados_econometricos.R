
library(readr)
library(readxl)
url = 'https://www.econometrics.com/manuals/gujarati/data_6.3.shd'

dados <- read_csv(url, col_names = TRUE, show_col_types = T, )
dados

dados = readr::read_delim(url, delim = "", col_names = F)
dados

install.packages("WDI")

library(WDI)

df <- WDI( country = "all",
           #country=c("US","BR"), 
          indicator = c("NY.GDP.TOTL.KD.ZG", "NE.EXP.GNFS.ZS", "SI.POV.DDAY"), start = 2022, end = 2022, latest = 5)
df


?WDI



install.packages("eurostat")
library(eurostat)

remotes::install_github("rfsaldanha/microdatasus")



library(microdatasus)
dados <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "RJ", information_system = "SIM-DO")
dados <- process_sim(dados)

