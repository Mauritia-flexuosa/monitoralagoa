# app.R - Dashboard Shiny
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(corrplot)
library(reshape2)
library(rsconnect)

# Verifica e instala pacotes se necessário (para Render)
if (!require("shiny")) install.packages("shiny", repos = "http://cran.rstudio.com/")
if (!require("shinydashboard")) install.packages("shinydashboard", repos = "http://cran.rstudio.com/")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.rstudio.com/")
if (!require("DT")) install.packages("DT", repos = "http://cran.rstudio.com/")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.rstudio.com/")
if (!require("lubridate")) install.packages("lubridate", repos = "http://cran.rstudio.com/")
if (!require("corrplot")) install.packages("corrplot", repos = "http://cran.rstudio.com/")
if (!require("reshape2")) install.packages("reshape2", repos = "http://cran.rstudio.com/")
if (!require("httr")) install.packages("httr", repos = "http://cran.rstudio.com/")
if (!require("jsonlite")) install.packages("jsonlite", repos = "http://cran.rstudio.com/")

css <- "
.badge {
  padding: 4px 8px;
  border-radius: 4px;
  color: white;
  font-weight: bold;
}
.bg-danger { background-color: #dc3545; }
.bg-warning { background-color: #ffc107; }
.bg-success { background-color: #28a745; }
.bg-info { background-color: #17a2b8; }
.bg-secondary { background-color: #6c757d; }

/* Reduzir tamanho da fonte dos value boxes */
.small-box .inner {
  padding: 10px !important;
}
.small-box .icon {
  top: 10px !important;
  font-size: 70px !important;
}
.small-box h3 {
  font-size: 28px !important;
  font-weight: bold !important;
  margin: 0 0 5px 0 !important;
}
.small-box p {
  font-size: 14px !important;
  margin: 0 !important;
}
"

ui <- dashboardPage(
  dashboardHeader(title = "Monitoramento"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dados", tabName = "dados", icon = icon("table")),
#      menuItem("Análises", tabName = "analises", icon = icon("chart-bar")),
      menuItem("Correlações", tabName = "correlacoes", icon = icon("chart-line")),
      menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))
    ),
    br(),
    actionButton("executar_api", "Executar API.R", icon = icon("play"), 
                 width = "80%", class = "btn-success"),
    br(), br(),
    
    # Value boxes organizados verticalmente
    div(style = "margin: 15px;",
        # Status
        valueBoxOutput("status_box", width = 12),
        br(),
        
        # Registros
        valueBoxOutput("registros_box", width = 12),
        br(),
      
        # Oxigênio
        valueBoxOutput("do_box", width = 12),
        br(),
        
        # Clorofila
        valueBoxOutput("chl_box", width = 12),
        br(),
        
        # Temperatura
        valueBoxOutput("temp_box", width = 12),
        br(),
        
        # Condutividade
        valueBoxOutput("cond_box", width = 12),
        br(),
        
        # PAR
        valueBoxOutput("par_box", width = 12)
    ),
    
    tags$div(
      style = "margin: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
      tags$h5("Status do Sistema", style = "margin-top: 0; font-size: 14px;"),
      textOutput("status_sistema")
    )
  ),
  
  dashboardBody(
    # Incluir CSS
    tags$head(tags$style(HTML(css))),
    
    tabItems(
      # Tab 1: Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Situação Atual da Lagoa",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  htmlOutput("mensagem_qualidade")
                )
              ),
	              fluidRow(
	                box(
	                  title = "Série Temporal",
	                  status = "primary",
	                  solidHeader = TRUE,
	                  width = 12,
	                  fluidRow(
	                    column(4,
	                           selectInput("variavel_serie", 
	                                       "Variável para o Gráfico:",
	                                       choices = c("Clorofila 3.0m" = "chl_3m",
	                                                   "Clorofila 4.5m" = "chl_4m",
	                                                   "Temperatura 1.0m" = "temp_1m",
	                                                   "Temperatura 3.0m" = "temp_3m",
	                                                   "Oxigênio Dissolvido 1.0m" = "do_1m",
	                                                   "Oxigênio Dissolvido 3.0m" = "do_3m",
	                                                   "Condutividade 1.0m" = "cond_1m",
	                                                   "Condutividade 3.0m" = "cond_3m",
	                                                   "PAR" = "PAR"),
	                                       selected = "chl_3m")
	                    ),
	                    column(4,
	                           dateInput("data_inicio", "Data de Início:", value = Sys.Date() - 60)
	                    ),
	                    column(4,
	                           dateInput("data_fim", "Data de Fim:", value = Sys.Date())
	                    )
	                  ),
	                  plotlyOutput("grafico_serie_unica"),
	                  tags$div(
	                    style = "margin-top: 10px; font-style: italic; color: #666;",
	                    textOutput("info_serie_unica")
	                  )
	                )
	              )
	      ),
      
      # Tab 2: Dados
      tabItem(tabName = "dados",
              fluidRow(
                box(
                  title = "Dados Brutos da API",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("tabela_dados"),
                  br(),
                  downloadButton("download_csv", "Exportar CSV"),
                  downloadButton("download_rds", "Exportar RDS")
                )
              ),
              fluidRow(
                box(
                  title = "Resumo dos Dados",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("resumo_dados")
                )
              )
      ),
      
      # Tab 3: Análises
      # tabItem(tabName = "analises",
      # 
      # ),
      
      # Tab 4: Correlações
      tabItem(tabName = "correlacoes",
              fluidRow(
                box(
                  title = "Matriz de Correlação entre Variáveis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("matriz_correlacao", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Configurações de Correlação",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           selectInput("cor_chl_profundidade", 
                                       "Profundidade da Clorofila:",
                                       choices = c("3.0m" = "chl_3m", 
                                                   "4.5m" = "chl_4m"),
                                       selected = "chl_3m")
                    ),
                    column(4,
                           selectInput("cor_variavel", 
                                       "Variável para Correlação:",
                                       choices = c("Temperatura" = "temp",
                                                   "Condutividade" = "cond",
                                                   "Oxigênio Dissolvido" = "do"),
                                       selected = "temp")
                    ),
                    column(4,
                           selectInput("cor_profundidade_var", 
                                       "Profundidade da Variável:",
                                       choices = c("1.0m" = "1m",
                                                   "3.0m" = "3m"),
                                       selected = "3m")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Gráfico de Correlação",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("grafico_correlacao_dinamico", height = "500px"),
                  tags$div(
                    style = "margin-top: 10px; font-style: italic; color: #666;",
                    textOutput("info_correlacao")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Estatísticas de Correlação",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("estatisticas_correlacao")
                )
              )
      ),
      
      # Tab 5: Sobre
      tabItem(tabName = "sobre",
              fluidRow(
                box(
                  title = "Sobre este Sistema",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 15px;",
                    tags$h3("Monitoramento de Qualidade da Água da Lagoa da Conceição - Florianópolis, SC."),
                    tags$p("Este sistema utiliza dados da API SIMCOSTA para monitoramento de:"),
                    tags$ul(
                      tags$li("Clorofila a 3.0m e 4.5m de profundidade"),
                      tags$li("Temperatura da água em diferentes profundidades"),
                      tags$li("Oxigênio dissolvido (DO)"),
                      tags$li("Condutividade"),
                      tags$li("Radiação fotossinteticamente ativa (PAR)")
                    ),
                    tags$hr(),
                    tags$h4("Classificação da Qualidade da Água:"),
                    tags$p("A qualidade da água para Clorofila, Oxigênio Dissolvido (OD) e Temperatura é classificada com base em thresholds ambientais específicos para lagunas costeiras, conforme a Resolução CONAMA 357/2005 para águas salobras Classe I e recomendações para Clorofila-a e Temperatura."),
                    tags$ul(
                      tags$li(strong("Oxigênio Dissolvido (OD) em ml/L:")), 
                      tags$ul(
                        tags$li("Ruim: < 5"),
                        tags$li("Atenção: 5 – 6"),
                        tags$li("Boa: > 6")
                      ),
                      tags$li(strong("Clorofila-a em μg/L:")), 
                      tags$ul(
                        tags$li("Ruim: > 30"),
                        tags$li("Atenção: 10 – 30"),
                        tags$li("Boa: < 10")
                      ),
                      tags$li(strong("Temperatura em °C:")), 
                      tags$ul(
                        tags$li("Ruim: > 30"),
                        tags$li("Atenção: 26 – 30"),
                        tags$li("Boa: < 26")
                      )
                    ),
                    tags$hr(),
                    tags$h4("Conversão de Oxigênio Dissolvido:"),
                    tags$p("Valores brutos de oxigênio dissolvido (micro molar) foram multiplicados por 0.032 para converter para ml/L."),
                    tags$hr(),
                    tags$h4("Como usar:"),
                    tags$ol(
                      tags$li("Clique em 'Executar API.R' para buscar os dados mais recentes"),
                      tags$li("Escolha o período de tempo para mostrar no gráfico"),
                      tags$li("Visualize os gráficos na aba Dashboard"),
                      tags$li("Consulte os dados brutos na aba Dados"),

                      tags$li("Veja correlações na aba Correlações")
                    ),
                    tags$hr(),
                    tags$h4("Detalhes Técnicos:"),
                    tags$p("Período padrão: Últimos 365 dias, ou conforme disponibilidade de dados da bóia"),
                    tags$p("Boia padrão: ID 40"),
                    tags$p("Frequência de atualização: Manual (clique no botão)"),
                    tags$p("Fonte dos dados: https://simcosta.furg.br/"),
                    tags$p("Desenvolvido por Marcio Baldissera Cure"),
                    tags$p("Contato: marciobcure@gmail.com")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Função para classificar a qualidade da água com base em thresholds ambientais
  classificar_qualidade <- function(variavel, valor) {
    if (is.na(valor)) {
      return("N/A")
    }
    
    if (variavel == "OD") {
      # Oxigênio Dissolvido (ml/L)
      if (valor < 5) {
        return("Ruim")
      } else if (valor >= 5 && valor <= 6) {
        return("Atenção")
      } else {
        return("Boa")
      }
    } else if (variavel == "Clorofila") {
      # Clorofila-a (μg/L)
      if (valor > 30) {
        return("Ruim")
      } else if (valor >= 10 && valor <= 30) {
        return("Atenção")
      } else {
        return("Boa")
      }
    } else if (variavel == "Temperatura") {
      # Temperatura (°C)
      if (valor > 30) {
        return("Ruim")
      } else if (valor >= 26 && valor <= 30) {
        return("Atenção")
      } else {
        return("Boa")
      }
    } else {
      return("N/A") # Para outras variáveis não classificadas
    }
  }
  
  # Função para gerar mensagem de qualidade da água

  gerar_mensagem_qualidade <- function(od, chl, temp) {
    
    status_od <- classificar_qualidade("OD", od)
    status_chl <- classificar_qualidade("Clorofila", chl)
    status_temp <- classificar_qualidade("Temperatura", temp)
    
    statuses <- c(status_od, status_chl, status_temp)
    
    if ("Ruim" %in% statuses) {
      
      return(list(
        texto = paste0(
          "⚠️ <b>Qualidade da água RUIM.</b><br>",
          "Condições inadequadas detectadas em pelo menos uma variável monitorada."
        ),
        cor = "#dc3545"
      ))
      
    } else if ("Atenção" %in% statuses) {
      
      return(list(
        texto = paste0(
          "⚠️ <b>Qualidade da água em ATENÇÃO.</b><br>",
          "Algumas variáveis apresentam valores fora da faixa ideal."
        ),
        cor = "#ffc107"
      ))
      
    } else {
      
      return(list(
        texto = paste0(
          "✅ <b>Qualidade da água BOA.</b><br>",
          "Os parâmetros monitorados estão dentro da faixa esperada."
        ),
        cor = "#28a745"
      ))
    }
  }
  
  
  # Função auxiliar para operador %||%
  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
  
  # Reactive values - armazena os dados
  valores <- reactiveValues(
    dados = NULL,
    dados_processados = NULL,
    ultima_execucao = NULL,
    status = "Pronto para executar API.R"
  )
  
  # Observador para o botão de executar API.R
  observeEvent(input$executar_api, {
    
    showNotification("Executando API.R...", type = "message", duration = 5)
    
    # Criar um ambiente isolado para executar o script
    env_api <- new.env()
    
    tryCatch({
      # Executar o script API.R no ambiente isolado
      source("./API.R", local = env_api)
      
      # Verificar se a variável 'dados' foi criada
      if (exists("dados", envir = env_api)) {
        dados_brutos <- get("dados", envir = env_api)
        
        if (!is.null(dados_brutos) && nrow(dados_brutos) > 0) {
          
          # Processar os dados com classificação para clorofila e oxigênio
          dados_processados <- dados_brutos %>%
            # Garantir que temos as colunas necessárias
            mutate(
              # Verificar se as colunas existem, caso contrário criar com NA
              chl_3m = if (exists("chl_3m", where = .)) chl_3m else NA_real_,
              chl_4m = if (exists("chl_4m", where = .)) chl_4m else NA_real_,
              temp_1m = if (exists("temp_1m", where = .)) temp_1m else NA_real_,
              temp_3m = if (exists("temp_3m", where = .)) temp_3m else NA_real_,
              do_1m = if (exists("do_1m", where = .)) do_1m * 0.032 else NA_real_, # Converter para ml/L
              do_3m = if (exists("do_3m", where = .)) do_3m * 0.032 else NA_real_, # Converter para ml/L
              cond_1m = if (exists("cond_1m", where = .)) cond_1m else NA_real_,
              cond_3m = if (exists("cond_3m", where = .)) cond_3m else NA_real_,
              PAR = if (exists("PAR", where = .)) PAR else NA_real_,
              
              # Calcular estatísticas simples
              chl_media = ifelse(!is.na(chl_3m) & !is.na(chl_4m), 
                                 (chl_3m + chl_4m) / 2, 
                                 coalesce(chl_3m, chl_4m)),
              
              # Classificar clorofila e oxigênio baseado em desvio padrão
              categoria_chl_3m = sapply(chl_3m, function(x) classificar_qualidade("Clorofila", x)),
              categoria_do_1m = sapply(do_1m, function(x) classificar_qualidade("OD", x)),
              categoria_do_3m = sapply(do_3m, function(x) classificar_qualidade("OD", x)),
              categoria_temp_1m = sapply(temp_1m, function(x) classificar_qualidade("Temperatura", x)),
              categoria_temp_3m = sapply(temp_3m, function(x) classificar_qualidade("Temperatura", x))
            ) %>%
            # Ordenar por data
            arrange(data_hora)
          
          # Atualizar reactive values
          valores$dados <- dados_brutos
          valores$dados_processados <- dados_processados
          valores$ultima_execucao <- Sys.time()
          valores$status <- sprintf("API executada com sucesso! %d registros.", nrow(dados_brutos))
          
          showNotification(sprintf("Dados carregados: %d registros", nrow(dados_brutos)), 
                           type = "message", duration = 3)
          
        } else {
          valores$status <- "API executada, mas nenhum dado retornado."
          showNotification("API não retornou dados", type = "warning")
        }
        
      } else {
        valores$status <- "Erro: Variável 'dados' não encontrada após execução do API.R"
        showNotification("Variável 'dados' não encontrada", type = "error")
      }
      
    }, error = function(e) {
      valores$status <- paste("Erro ao executar API.R:", e$message)
      showNotification(paste("Erro:", e$message), type = "error")
    })
  })
  
  # Value boxes - STATUS
  output$status_box <- renderValueBox({
    if (!is.null(valores$ultima_execucao)) {
      # Converte para o fuso horário de Brasília
      ultima_formatada <- format(
        valores$ultima_execucao, 
        "%H:%M", 
        tz = "America/Sao_Paulo"
      )
      valor <- ultima_formatada
    } else {
      valor <- "Nunca"
    }
    
    valueBox(
      value = valor,
      subtitle = "Última Execução",
      icon = icon("clock"),
      color = "blue",
      width = 12
    )
  })  
  # Value boxes - REGISTROS
  output$registros_box <- renderValueBox({
    if (!is.null(valores$dados)) {
      n_registros <- nrow(valores$dados)
      color <- ifelse(n_registros > 0, "green", "yellow")
    } else {
      n_registros <- 0
      color <- "blue"
    }
    
    valueBox(
      value = n_registros,
      subtitle = "Registros",
      icon = icon("database"),
      color = color,
      width = 12
    )
  })
  
  # Value boxes - CLOROFILA (3.0m) - ÚLTIMO VALOR
  output$chl_box <- renderValueBox({
    if (!is.null(valores$dados_processados) && nrow(valores$dados_processados) > 0) {
      # Obter o último valor não nulo da clorofila a 3m
      ultimo_valor <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(chl_3m)) %>%
        slice(1) %>%
        pull(chl_3m)
      
      ultima_categoria <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(chl_3m)) %>%
        slice(1) %>%
        pull(categoria_chl_3m)
      
      if (length(ultimo_valor) == 0) {
        ultimo_valor <- 0
        ultima_categoria <- "N/A"
      }
      
      color <- case_when(
        ultima_categoria == "Ruim" ~ "red",
        ultima_categoria == "Atenção" ~ "orange",
        ultima_categoria == "Boa" ~ "green",
        TRUE ~ "blue"
      )
      
      valueBox(
        value = sprintf("%.1f μg/L", ultimo_valor),
        subtitle = paste("Clorofila 3.0m (", ultima_categoria, ")"),
        icon = icon("leaf"),
        color = color,
        width = 12
      )
    } else {
      valueBox(
        value = "0 μg/L",
        subtitle = "Clorofila 3.0m (N/A)",
        icon = icon("leaf"),
        color = "blue",
        width = 12
      )
    }
  })
  
  # Value boxes - CONDUTIVIDADE (3.0m) - ÚLTIMO VALOR
  output$cond_box <- renderValueBox({
    if (!is.null(valores$dados_processados) && nrow(valores$dados_processados) > 0) {
      # Obter o último valor não nulo da condutividade a 3m
      ultimo_valor <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(cond_3m)) %>%
        slice(1) %>%
        pull(cond_3m)
      
      if (length(ultimo_valor) == 0) ultimo_valor <- 0
      
      valueBox(
        value = sprintf("%.0f μS/cm", ultimo_valor),
        subtitle = "Condutividade 3.0m",
        icon = icon("bolt"),
        color = "green",
        width = 12
      )
    } else {
      valueBox(
        value = "0 μS/cm",
        subtitle = "Condutividade 3.0m",
        icon = icon("bolt"),
        color = "blue",
        width = 12
      )
    }
  })
  
  # Value boxes - TEMPERATURA (3.0m) - ÚLTIMO VALOR
  output$temp_box <- renderValueBox({
    if (!is.null(valores$dados_processados) && nrow(valores$dados_processados) > 0) {
      # Obter o último valor não nulo da temperatura a 3m
      ultimo_valor <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(temp_3m)) %>%
        slice(1) %>%
        pull(temp_3m)
        
      ultima_categoria <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(temp_3m)) %>%
        slice(1) %>%
        pull(categoria_temp_3m)
      
      if (length(ultimo_valor) == 0) {
        ultimo_valor <- 0
        ultima_categoria <- "N/A"
      }
      
      color <- case_when(
        ultima_categoria == "Ruim" ~ "red",
        ultima_categoria == "Atenção" ~ "orange",
        ultima_categoria == "Boa" ~ "green",
        TRUE ~ "blue"
      )
      
      valueBox(
        value = sprintf("%.1f °C", ultimo_valor),
        subtitle = paste("Temperatura 3.0m (", ultima_categoria, ")"),
        icon = icon("thermometer-half"),
        color = color,
        width = 12
      )
    } else {
      valueBox(
        value = "0 °C",
        subtitle = "Temperatura 3.0m",
        icon = icon("thermometer-half"),
        color = "blue",
        width = 12
      )
    }
  })
  
  # Value boxes - OXIGÊNIO (3.0m) - ÚLTIMO VALOR COM CLASSIFICAÇÃO
  output$do_box <- renderValueBox({
    if (!is.null(valores$dados_processados) && nrow(valores$dados_processados) > 0) {
      # Obter o último valor não nulo do oxigênio dissolvido a 3m
      ultimo_valor <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(do_3m)) %>%
        slice(1) %>%
        pull(do_3m)
      
      ultima_categoria <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(do_3m)) %>%
        slice(1) %>%
        pull(categoria_do_3m)
      
      if (length(ultimo_valor) == 0) {
        ultimo_valor <- 0
        ultima_categoria <- "N/A"
      }
      
      color <- case_when(
        ultima_categoria == "Ruim" ~ "red",
        ultima_categoria == "Atenção" ~ "orange",
        ultima_categoria == "Boa" ~ "green",
        TRUE ~ "blue"
      )
      
      valueBox(
        value = sprintf("%.3f ml/L", ultimo_valor),
        subtitle = paste("OD 3.0m (", ultima_categoria, ")"),
        icon = icon("wind"),
        color = color,
        width = 12
      )
    } else {
      valueBox(
        value = "0 ml/L",
        subtitle = "OD 3.0m (N/A)",
        icon = icon("wind"),
        color = "blue",
        width = 12
      )
    }
  })
  
  # Value boxes - PAR - ÚLTIMO VALOR
  output$par_box <- renderValueBox({
    if (!is.null(valores$dados_processados) && nrow(valores$dados_processados) > 0) {
      # Obter o último valor não nulo do PAR
      ultimo_valor <- valores$dados_processados %>%
        arrange(desc(data_hora)) %>%
        filter(!is.na(PAR)) %>%
        slice(1) %>%
        pull(PAR)
      
      if (length(ultimo_valor) == 0) ultimo_valor <- 0
      
      valueBox(
        value = sprintf("%.0f μE", ultimo_valor),
        subtitle = "PAR",
        icon = icon("sun"),
        color = "orange",
        width = 12
      )
    } else {
      valueBox(
        value = "0 μE",
        subtitle = "PAR",
        icon = icon("sun"),
        color = "blue",
        width = 12
      )
    }
  })
  
  output$status_sistema <- renderText({
    valores$status
  })
  
  output$mensagem_qualidade <- renderUI({
    
    req(valores$dados_processados)
    
    ultima_linha <- valores$dados_processados %>%
      arrange(desc(data_hora)) %>%
      slice(1)
    
    mensagem <- gerar_mensagem_qualidade(
      ultima_linha$do_3m,
      ultima_linha$chl_3m,
      ultima_linha$temp_3m
    )
    
    div(
      style = paste0(
        "padding:15px;",
        "border-radius:10px;",
        "background-color:", mensagem$cor, ";",
        "color:white;",
        "font-size:18px;",
        "font-weight:bold;"
      ),
      
      HTML(mensagem$texto)
    )
  })
  
  # Gráfico de série temporal única
  output$grafico_serie_unica <- renderPlotly({
    req(valores$dados_processados)
    
    dados_filtrados <- valores$dados_processados %>%
      filter(data_hora >= input$data_inicio & data_hora <= input$data_fim + days(1))
    
    if (nrow(dados_filtrados) == 0) {
      return(plotly_empty())
    }
    
    variavel_selecionada <- input$variavel_serie
    titulo_grafico <- switch(
      variavel_selecionada,
      "chl_3m" = "Clorofila 3.0m",
      "chl_4m" = "Clorofila 4.5m",
      "temp_1m" = "Temperatura 1.0m",
      "temp_3m" = "Temperatura 3.0m",
      "do_1m" = "Oxigênio Dissolvido 1.0m",
      "do_3m" = "Oxigênio Dissolvido 3.0m",
      "cond_1m" = "Condutividade 1.0m",
      "cond_3m" = "Condutividade 3.0m",
      "PAR" = "Radiação PAR",
      "Variável Desconhecida"
    )
    
    unidade_eixo_y <- switch(
      variavel_selecionada,
      "chl_3m" = "μg/L",
      "chl_4m" = "μg/L",
      "temp_1m" = "°C",
      "temp_3m" = "°C",
      "do_1m" = "ml/L",
      "do_3m" = "ml/L",
      "cond_1m" = "μS/cm",
      "cond_3m" = "μS/cm",
      "PAR" = "μE",
      ""
    )
    
    plot_ly(dados_filtrados, x = ~data_hora, y = ~get(variavel_selecionada), type = 'scatter', mode = 'lines', 
            name = titulo_grafico,
            line = list(color = 'blue', width = 2),
            hoverinfo = 'text',
            text = ~paste(
              "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
              "<br>", titulo_grafico, ": ", round(get(variavel_selecionada), 2), " ", unidade_eixo_y
            )) %>%
      layout(
        title = paste("Série Temporal de", titulo_grafico),
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = paste(titulo_grafico, "(", unidade_eixo_y, ")")),
        hovermode = 'closest',
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  output$info_serie_unica <- renderText({
    if (!is.null(valores$dados_processados)) {
      paste("Dados reais da API SIMCOSTA - ", 
            nrow(valores$dados_processados), "registros -",
            "Período:", format(min(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y"),
            "a", format(max(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y"))
    } else {
      "Execute a API.R para carregar dados"
    }
  })
  

  
  # Tabela de dados COM STATUS PARA CLOROFILA E OXIGÊNIO
  output$tabela_dados <- renderDT({
    req(valores$dados_processados)
    
    dados_tabela <- valores$dados_processados %>%
      arrange(desc(data_hora)) %>%
      mutate(
        data_hora = format(data_hora, "%d/%m/%Y %H:%M"),
        chl_3m = round(chl_3m, 2),
        chl_4m = round(chl_4m, 2),
        temp_1m = round(temp_1m, 1),
        temp_3m = round(temp_3m, 1),
        do_1m = round(do_1m, 3),  # ml/L com 3 casas decimais
        do_3m = round(do_3m, 3),  # ml/L com 3 casas decimais
        cond_1m = round(cond_1m, 0),
        cond_3m = round(cond_3m, 0),
        PAR = round(PAR, 0),
        # Fatores para ordenação
        categoria_chl_3m = factor(categoria_chl_3m, 
                                  levels = c("Ruim", "Atenção", "Boa")),
        categoria_do_3m = factor(categoria_do_3m,
                                 levels = c("Ruim", "Atenção", "Boa")),
        categoria_temp_3m = factor(categoria_temp_3m,
                                   levels = c("Ruim", "Atenção", "Boa")),
        # Status HTML para clorofila
        status_chl = case_when(
          categoria_chl_3m == "Ruim" ~ '<span class="badge bg-danger">RUIM</span>',
          categoria_chl_3m == "Atenção" ~ '<span class="badge bg-warning">ATENÇÃO</span>',
          categoria_chl_3m == "Boa" ~ '<span class="badge bg-success">BOA</span>',
          TRUE ~ '<span class="badge bg-secondary">N/A</span>'
        ),
        # Status HTML para oxigênio
        status_do = case_when(
          categoria_do_3m == "Ruim" ~ '<span class="badge bg-danger">RUIM</span>',
          categoria_do_3m == "Atenção" ~ '<span class="badge bg-warning">ATENÇÃO</span>',
          categoria_do_3m == "Boa" ~ '<span class="badge bg-success">BOA</span>',
                    TRUE ~ 
'<span class="badge bg-secondary">N/A</span>'
        ),
        # Status HTML para temperatura
        status_temp = case_when(
          categoria_temp_3m == "Ruim" ~ 
'<span class="badge bg-danger">RUIM</span>',
          categoria_temp_3m == "Atenção" ~ 
'<span class="badge bg-warning">ATENÇÃO</span>',
          categoria_temp_3m == "Boa" ~ 
'<span class="badge bg-success">BOA</span>',
          TRUE ~ 
'<span class="badge bg-secondary">N/A</span>'
        )
      ) %>%
      select(data_hora, 
             chl_3m, categoria_chl_3m, status_chl,
             do_3m, categoria_do_3m, status_do,
             temp_3m, categoria_temp_3m, status_temp,
             chl_4m, temp_1m, do_1m, cond_1m, cond_3m, PAR)
    
    datatable(
      dados_tabela,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:13)
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      caption = "Dados Processados da API SIMCOSTA - Com Status de Clorofila e Oxigênio"
    )
  })
  
  # Resumo dos dados COM ESTATÍSTICAS DE OXIGÊNIO
  output$resumo_dados <- renderPrint({
    req(valores$dados_processados)
    
    cat("=== RESUMO DOS DADOS DA API SIMCOSTA ===\n\n")
    cat("Período dos dados:\n")
    cat("  Início: ", format(min(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y %H:%M"), "\n")
    cat("  Fim:    ", format(max(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y %H:%M"), "\n")
    cat("  Total:  ", nrow(valores$dados_processados), "registros\n\n")
    
    # Clorofila 3.0m
    cat("Clorofila a 3.0m:\n")
    cat("  Média:    ", round(mean(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n\n")
    
    # Oxigênio Dissolvido 3.0m
    cat("Oxigênio Dissolvido a 3.0m (ml/L):\n")
    cat("  Média:    ", round(mean(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n\n")
    
    # Clorofila 4.5m
    media_chl_4m <- mean(valores$dados_processados$chl_4m, na.rm = TRUE)
    cat("Clorofila a 4.5m:\n")
    cat("  Média:    ", round(media_chl_4m, 2), "μg/L\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$chl_4m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$chl_4m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$chl_4m, na.rm = TRUE), 2), "μg/L\n\n")
    
    # Condutividade 3.0m
    cat("Condutividade a 3.0m:\n")
    cat("  Média:    ", round(mean(valores$dados_processados$cond_3m, na.rm = TRUE), 0), "μS/cm\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$cond_3m, na.rm = TRUE), 0), "μS/cm\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$cond_3m, na.rm = TRUE), 0), "μS/cm\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$cond_3m, na.rm = TRUE), 0), "μS/cm\n\n")
    
    # Temperatura 3.0m
    cat("Temperatura a 3.0m:\n")
    cat("  Média:    ", round(mean(valores$dados_processados$temp_3m, na.rm = TRUE), 1), "°C\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$temp_3m, na.rm = TRUE), 1), "°C\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$temp_3m, na.rm = TRUE), 1), "°C\n\n")
    
    # Classificação da Clorofila 3.0m
    cat("Classificação da Clorofila 3.0m (baseado em thresholds ambientais):\n")
    categorias_chl <- table(valores$dados_processados$categoria_chl_3m)
    for (cat in c("Ruim", "Atenção", "Boa")) {
      if (cat %in% names(categorias_chl)) {
        cat(sprintf("  %-12s: %4d registros (%5.1f%%)\n", 
                    cat, 
                    categorias_chl[cat], 
                    categorias_chl[cat]/nrow(valores$dados_processados)*100))
      } else {
        cat(sprintf("  %-12s: %4d registros (%5.1f%%)\n", cat, 0, 0))
      }
    }
    cat("\n")
    
    # Classificação do Oxigênio Dissolvido 3.0m
    cat("Classificação do Oxigênio Dissolvido 3.0m (baseado em thresholds ambientais):\n")
    categorias_do <- table(valores$dados_processados$categoria_do_3m)
    for (cat in c("Ruim", "Atenção", "Boa")) {
      if (cat %in% names(categorias_do)) {
        cat(sprintf("  %-12s: %4d registros (%5.1f%%)\n", 
                    cat, 
                    categorias_do[cat], 
                    categorias_do[cat]/nrow(valores$dados_processados)*100))
      } else {
        cat(sprintf("  %-12s: %4d registros (%5.1f%%)\n", cat, 0, 0))
      }
    }
  })
  

  

  

  
  # MODIFICAÇÃO: Matriz de correlação com fonte maior
  output$matriz_correlacao <- renderPlot({
    req(valores$dados_processados)
    
    # Selecionar variáveis para correlação
    dados_cor <- valores$dados_processados %>%
      select(
        chl_3m, chl_4m,
        temp_1m, temp_3m,
        cond_1m, cond_3m,
        do_1m, do_3m,
        PAR
      ) %>%
      na.omit()
    
    # Renomear colunas para melhor visualização
    colnames(dados_cor) <- c(
      "Clorofila 3.0m", "Clorofila 4.5m",
      "Temperatura 1.0m", "Temperatura 3.0m",
      "Condutividade 1.0m", "Condutividade 3.0m",
      "OD 1.0m (ml/L)", "OD 3.0m (ml/L)",
      "PAR"
    )
    
    # Calcular matriz de correlação
    mat_cor <- cor(dados_cor)
    
    # MVisualizar Matriz de correlação
    corrplot(mat_cor,
             method = "color",
             type = "upper",
             order = "hclust",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 1.2,  # Aumentado de 0.8 para 1.2
             addCoef.col = "black",
             number.cex = 1.0,  # Aumentado de 0.7 para 1.0
             col = colorRampPalette(c("blue", "white", "red"))(200),
             title = "Matriz de Correlação entre Variáveis",
             mar = c(0, 0, 2, 0),
             diag = FALSE)
  })
  
  # Gráfico de correlação dinâmico
  output$grafico_correlacao_dinamico <- renderPlotly({
    req(valores$dados_processados)
    
    # Determinar variáveis baseado nas seleções
    chl_var <- input$cor_chl_profundidade
    variavel <- input$cor_variavel
    profundidade <- input$cor_profundidade_var
    
    # Determinar variável X baseado na seleção
    if (variavel == "temp") {
      var_x <- ifelse(profundidade == "1m", "temp_1m", "temp_3m")
      nome_x <- ifelse(profundidade == "1m", "Temperatura 1.0m (°C)", "Temperatura 3.0m (°C)")
      cor <- 'rgba(231, 76, 60, 0.6)'
    } else if (variavel == "cond") {
      var_x <- ifelse(profundidade == "1m", "cond_1m", "cond_3m")
      nome_x <- ifelse(profundidade == "1m", "Condutividade 1.0m (μS/cm)", "Condutividade 3.0m (μS/cm)")
      cor <- 'rgba(155, 89, 182, 0.6)'
    } else { # "do"
      var_x <- ifelse(profundidade == "1m", "do_1m", "do_3m")
      nome_x <- ifelse(profundidade == "1m", "Oxigênio Dissolvido 1.0m (ml/L)", "Oxigênio Dissolvido 3.0m (ml/L)")
      cor <- 'rgba(52, 152, 219, 0.6)'
    }
    
    nome_y <- ifelse(chl_var == "chl_3m", "Clorofila 3.0m (μg/L)", "Clorofila 4.5m (μg/L)")
    
    # Criar dados para correlação
    dados_cor <- valores$dados_processados %>%
      select(x = !!sym(var_x), y = !!sym(chl_var)) %>%
      filter(!is.na(x), !is.na(y))
    
    if (nrow(dados_cor) == 0) {
      return(plotly_empty() %>%
               layout(title = "Dados insuficientes para calcular correlação"))
    }
    
    # Calcular correlação
    correlacao <- cor(dados_cor$x, dados_cor$y, use = "complete.obs")
    
    # Criar gráfico de dispersão
    plot_ly(dados_cor, x = ~x, y = ~y,
            type = 'scatter', mode = 'markers',
            marker = list(size = 8, color = cor),
            hoverinfo = 'text',
            text = ~paste(
              nome_x, ": ", round(x, ifelse(variavel == "do", 3, ifelse(variavel == "temp", 1, 0))),
              "<br>", nome_y, ": ", round(y, 2)
            )) %>%
      add_trace(x = ~x, y = ~fitted(lm(y ~ x, data = dados_cor)),
                type = 'scatter', mode = 'lines',
                line = list(color = 'red', width = 2),
                name = 'Linha de Regressão') %>%
      layout(
        title = paste("Correlação: ", nome_y, " vs ", nome_x,
                      "<br>Coeficiente de Correlação (r) = ", round(correlacao, 3)),
        xaxis = list(title = nome_x),
        yaxis = list(title = nome_y),
        showlegend = FALSE,
        margin = list(l = 80, r = 40, t = 100, b = 80)
      )
  })
  
  output$info_correlacao <- renderText({
    req(valores$dados_processados)
    
    chl_var <- input$cor_chl_profundidade
    variavel <- input$cor_variavel
    profundidade <- input$cor_profundidade_var
    
    nome_y <- ifelse(chl_var == "chl_3m", "Clorofila 3.0m", "Clorofila 4.5m")
    
    if (variavel == "temp") {
      nome_x <- ifelse(profundidade == "1m", "Temperatura 1.0m", "Temperatura 3.0m")
    } else if (variavel == "cond") {
      nome_x <- ifelse(profundidade == "1m", "Condutividade 1.0m", "Condutividade 3.0m")
    } else {
      nome_x <- ifelse(profundidade == "1m", "Oxigênio Dissolvido 1.0m", "Oxigênio Dissolvido 3.0m")
    }
    
    paste("Correlação entre ", nome_y, " e ", nome_x, ". Use os controles acima para alterar as variáveis.")
  })
  
  # Estatísticas de correlação
  output$estatisticas_correlacao <- renderPrint({
    req(valores$dados_processados)
    
    chl_var <- input$cor_chl_profundidade
    variavel <- input$cor_variavel
    profundidade <- input$cor_profundidade_var
    
    # Determinar variável X baseado na seleção
    if (variavel == "temp") {
      var_x <- ifelse(profundidade == "1m", "temp_1m", "temp_3m")
      nome_x <- ifelse(profundidade == "1m", "Temperatura 1.0m", "Temperatura 3.0m")
    } else if (variavel == "cond") {
      var_x <- ifelse(profundidade == "1m", "cond_1m", "cond_3m")
      nome_x <- ifelse(profundidade == "1m", "Condutividade 1.0m", "Condutividade 3.0m")
    } else { # "do"
      var_x <- ifelse(profundidade == "1m", "do_1m", "do_3m")
      nome_x <- ifelse(profundidade == "1m", "Oxigênio Dissolvido 1.0m", "Oxigênio Dissolvido 3.0m")
    }
    
    nome_y <- ifelse(chl_var == "chl_3m", "Clorofila 3.0m", "Clorofila 4.5m")
    
    # Criar dados para correlação
    dados_cor <- valores$dados_processados %>%
      select(x = !!sym(var_x), y = !!sym(chl_var)) %>%
      filter(!is.na(x), !is.na(y))
    
    if (nrow(dados_cor) == 0) {
      cat("Dados insuficientes para calcular estatísticas de correlação.")
      return()
    }
    
    # Calcular estatísticas
    correlacao <- cor(dados_cor$x, dados_cor$y, use = "complete.obs")
    modelo <- lm(y ~ x, data = dados_cor)
    resumo <- summary(modelo)
    
    cat("=== ESTATÍSTICAS DE CORRELAÇÃO ===\n\n")
    cat("Variáveis analisadas:\n")
    cat("  X: ", nome_x, "\n")
    cat("  Y: ", nome_y, "\n\n")
    
    cat("Coeficiente de Correlação (r): ", round(correlacao, 4), "\n")
    cat("Coeficiente de Determinação (R²): ", round(resumo$r.squared, 4), "\n")
    cat("Número de observações: ", nrow(dados_cor), "\n\n")
    
    cat("Estatísticas da Regressão Linear:\n")
    cat("  Intercepto (α): ", round(coef(modelo)[1], 4), "\n")
    cat("  Inclinação (β): ", round(coef(modelo)[2], 4), "\n")
    cat("  Equação: Y = ", round(coef(modelo)[1], 4), " + ", round(coef(modelo)[2], 4), " * X\n\n")
    
    cat("Teste de Significância:\n")
    cat("  Valor-p: ", format.pval(resumo$coefficients[2, 4], digits = 4), "\n")
    if (resumo$coefficients[2, 4] < 0.05) {
      cat("  Resultado: Correlação estatisticamente significativa (p < 0.05)\n")
    } else {
      cat("  Resultado: Correlação não estatisticamente significativa\n")
    }
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("dados_simcosta_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      if (!is.null(valores$dados_processados)) {
        write.csv(valores$dados_processados, file, row.names = FALSE)
      }
    }
  )
  
  # Download RDS
  output$download_rds <- downloadHandler(
    filename = function() {
      paste0("dados_simcosta_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
    },
    content = function(file) {
      if (!is.null(valores$dados_processados)) {
        saveRDS(valores$dados_processados, file)
      }
    }
  )
  
  # Inicialização: tentar carregar dados existentes
  observe({
    # Verificar se existe um arquivo de dados salvo
    if (file.exists("dados_simcosta.rds")) {
      tryCatch({
        dados_salvos <- readRDS("dados_simcosta.rds")
        if (!is.null(dados_salvos)) {
          valores$dados_processados <- dados_salvos
          valores$status <- "Dados carregados do arquivo salvo"
        }
      }, error = function(e) {
        cat("Erro ao carregar dados salvos:", e$message, "\n")
      })
    }
  })
}

shinyApp(ui = ui, server = server)
