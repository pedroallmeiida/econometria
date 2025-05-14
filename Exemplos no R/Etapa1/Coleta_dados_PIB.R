#install.packages(c( "httr", "janitor"))
library(readxl)
library(dplyr)
library(httr)
library(janitor)



url <- "https://ftp.ibge.gov.br/Contas_Nacionais/Municipios/2022/xls/PIBmunic_2022_xls.zip"
destfile <- "PIBmunic_2022_xls.zip"

# Faz o download
GET(url, write_disk(destfile, overwrite = TRUE))

# Descompacta o arquivo
unzip(destfile, exdir = "PIB2022")


# Lista os arquivos XLSX dentro da pasta
arquivos <- list.files("PIB2022", pattern = "\\.xls$", full.names = TRUE)

# Vamos ler o principal (em geral, começa com "PIB")
arquivo_pib <- arquivos[grepl("PIBMunic", arquivos)]

# Lê o arquivo
pib_data <- read_excel(arquivo_pib, skip = 2) %>% 
  clean_names()