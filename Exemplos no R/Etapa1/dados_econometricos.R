
library(readr)
library(readxl)
url = 'https://www.econometrics.com/manuals/gujarati/data_6.3.shd'

dados <- read_csv(url, col_names = TRUE, show_col_types = T, )
dados

dados = readr::read_delim(url, delim = "", col_names = F)
dados
library(dplyr)
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
dados <- fetch_datasus(year_start = 2022, year_end = 2022, uf = "PB", information_system = "SIM-DOMAT")
dados <- process_sim(dados)
glimplise(dados)

?microdatasus

library(sidrar)
dados = get_sidra( api = "/t/9542/n6/all/v/allxp/p/all/c59/1024,93024/c2/6794/c86/95251/c287/100362"  )
View(dados)



info_sidra("10064")


search_sidra(c("renda per capita"))


search_sidra(c("saneamento"))



df_envelhecimento = get_sidra( x = 9885, period = "last", geo = "City", header = T  )
df_renda = get_sidra( x = 9885, period = "last", geo = "City", header = T  )


View(df_envelhecimento)




