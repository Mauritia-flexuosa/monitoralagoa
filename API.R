# API.R
library(httr2)
library(jsonlite)
library(tidyverse)

# URL corrigida - note que os timestamps precisam ser válidos
# Vou usar um período recente
time2 <- as.numeric(Sys.time())  # agora
time1 <- time2 - (60 * 24 * 3600)  # 30 dias atrás

# Construir URL corretamente
url <- sprintf(
  "https://simcosta.furg.br/api/intrans_data?boiaID=40&type=json&time1=%d&time2=%d&params=Avg_Con_1.0_from_surface,Avg_Con_3.0_from_surface,Avg_Con_4.5_from_surface,W_Tmp_1.0_from_surface,W_Tmp_3.0_from_surface,W_Tmp_4.5_from_surface,Avg_DO_1.0_from_surface,Avg_DO_3.0_from_surface,Avg_DO_4.5_from_surface,Avg_ChlA_3.0_from_surface,Avg_ChlA_4.5_from_surface,Avg_PAR,Avg_PAR_3.0_from_surface,Avg_PAR_4.5_from_surface,Avg_Press_1.0_from_bottom",
  round(time1),
  round(time2)
)

cat("URL:", url, "\n\n")

# Fazer requisição
tryCatch({
  resp <- request(url) |> 
    req_perform()
  
  # Verificar status
  cat("Status:", resp_status(resp), "\n")
  cat("Content-Type:", resp_header(resp, "content-type"), "\n")
  cat("Content-Length:", resp_header(resp, "content-length"), "\n\n")
  
  # Tentar diferentes formas de ler os dados
  # Opção 1: Como texto
  texto <- resp |> resp_body_string()
  cat("Primeiros 500 caracteres:\n")
  cat(substr(texto, 1, 500), "\n\n")
  
  # Opção 2: Como JSON
  if (nchar(texto) > 0) {
    dados_json <- fromJSON(texto, simplifyVector = FALSE)
    cat("Estrutura do JSON:\n")
    str(dados_json, max.level = 2)
    
    # Converter para data.frame
    if (length(dados_json) > 0) {
      # Verificar estrutura do primeiro elemento
      cat("\nPrimeiro elemento:\n")
      print(dados_json[[1]])
      
      # Tentar converter
      dados_df <- fromJSON(rawToChar(resp$body), simplifyDataFrame = T)
      cat("\nDimensões do data.frame:", dim(dados_df), "\n")
      cat("\nPrimeiras linhas:\n")
      print(head(dados_df))
    }
  }
  
}, error = function(e) {
  cat("Erro:", e$message, "\n")
})

dados <- dados_df %>%
  rename(
    Time = timestamp,  # Ajuste conforme o nome real da coluna
    cond_1m = "Conductivity @ 1.0 m depth",
    cond_3m = "Conductivity @ 4.5 m depth",
    temp_1m = "Water Temp. @ 1.0 m depth",
    temp_3m = "Water Temp. @ 3.0 m depth",
    do_1m = "DO @ 1.0 m depth",
    do_3m = "DO @ 3.0 m depth",
    chl_3m = "Chlorophyll @ 3.0 m depth",
    chl_4m = "Chlorophyll @ 4.5 m depth",
    PAR = "Photosynthetically Active Radiation") %>%
  mutate(
    data_hora = make_datetime(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND),
    across(where(is.character), ~ as.numeric(ifelse(. == "NULL", NA, .))),  # Converter timestamp
    data = as.Date(data_hora),
    hora = hour(data_hora)
  )

# print(head(dados))
# print(head(dados_df))
