# app.R - Dashboard Shiny usando EXATAMENTE seu script API.R
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

# CSS customizado com fonte reduzida para value boxes
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
      menuItem("Análises", tabName = "analises", icon = icon("chart-bar")),
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
        
        # Clorofila
        valueBoxOutput("chl_box", width = 12),
        br(),
        
        # Condutividade
        valueBoxOutput("cond_box", width = 12),
        br(),
        
        # Temperatura
        valueBoxOutput("temp_box", width = 12),
        br(),
        
        # Oxigênio
        valueBoxOutput("do_box", width = 12),
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
                  title = "Série Temporal da Clorofila",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("grafico_serie"),
                  tags$div(
                    style = "margin-top: 10px; font-style: italic; color: #666;",
                    textOutput("info_serie")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Temperatura da Água",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_temp")
                ),
                box(
                  title = "Oxigênio Dissolvido (ml/L)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_do")
                )
              ),
              fluidRow(
                box(
                  title = "Condutividade",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_cond")
                ),
                box(
                  title = "Radiação PAR",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_par")
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
      tabItem(tabName = "analises",
              fluidRow(
                box(
                  title = "Distribuição de Frequência da Clorofila 3.0m",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_hist_chl"),
                  tags$div(
                    style = "margin-top: 10px; font-style: italic; color: #666; font-size: 12px;",
                    textOutput("info_hist_chl")
                  )
                ),
                box(
                  title = "Distribuição de Frequência do Oxigênio Dissolvido 3.0m",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_hist_do_detalhado"),
                  tags$div(
                    style = "margin-top: 10px; font-style: italic; color: #666; font-size: 12px;",
                    textOutput("info_hist_do")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Distribuição de Frequência da Condutividade 3.0m",
                  status = "success",
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("grafico_hist_cond")
                ),
                box(
                  title = "Distribuição de Frequência da Temperatura 3.0m",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("grafico_hist_temp")
                ),
                box(
                  title = "Distribuição de Frequência do PAR",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("grafico_hist_par")
                )
              ),
              fluidRow(
                box(
                  title = "Série Temporal Detalhada do Oxigênio Dissolvido",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("grafico_do_detalhado")
                )
              )
      ),
      
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
                    tags$h4("Classificação Baseada em Desvio Padrão:"),
                    tags$p("Tanto a clorofila quanto o oxigênio dissolvido são classificados usando o mesmo critério:"),
                    tags$ul(
                      tags$li(strong("Muito Baixo:")), 
                      tags$ul(tags$li("Valor < Média - 2 * Desvio Padrão")),
                      tags$li(strong("Baixo:")), 
                      tags$ul(tags$li("Média - 2 * DP ≤ Valor < Média - 1 * DP")),
                      tags$li(strong("Moderado:")), 
                      tags$ul(tags$li("Média - 1 * DP ≤ Valor < Média + 1 * DP")),
                      tags$li(strong("Alto:")), 
                      tags$ul(tags$li("Média + 1 * DP ≤ Valor < Média + 2 * DP")),
                      tags$li(strong("Muito Alto:")), 
                      tags$ul(tags$li("Valor ≥ Média + 2 * Desvio Padrão"))
                    ),
                    tags$hr(),
                    tags$h4("Conversão de Oxigênio Dissolvido:"),
                    tags$p("Valores brutos de oxigênio dissolvido (micro molar) foram multiplicados por 0.032 para converter para ml/L."),
                    tags$hr(),
                    tags$h4("Como usar:"),
                    tags$ol(
                      tags$li("Clique em 'Executar API.R' para buscar os dados mais recentes"),
                      tags$li("Visualize os gráficos na aba Dashboard"),
                      tags$li("Consulte os dados brutos na aba Dados"),
                      tags$li("Analise distribuições na aba Análises"),
                      tags$li("Veja correlações na aba Correlações")
                    ),
                    tags$hr(),
                    tags$h4("Detalhes Técnicos:"),
                    tags$p("Período padrão: Últimos 60 dias"),
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
  
  # Função genérica para classificação baseada em desvio padrão
  classificar_variavel <- function(valores) {
    if (length(valores) == 0 || all(is.na(valores))) {
      return(rep(NA, length(valores)))
    }
    
    media <- mean(valores, na.rm = TRUE)
    dp <- sd(valores, na.rm = TRUE)
    
    if (is.na(dp) || dp == 0) {
      return(rep("Moderado", length(valores)))
    }
    
    cut(valores,
        breaks = c(-Inf, media - 2*dp, media - dp, media + dp, media + 2*dp, Inf),
        labels = c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto"),
        include.lowest = TRUE)
  }
  
  # Função para criar histograma com estatísticas - VERSÃO CORRIGIDA
  criar_histograma <- function(dados, variavel, titulo, cor, unidade) {
    if (is.null(dados) || nrow(dados) == 0) {
      return(plotly_empty())
    }
    
    valores <- dados[[variavel]]
    valores <- valores[!is.na(valores)]
    
    if (length(valores) == 0) {
      return(plotly_empty())
    }
    
    media <- mean(valores, na.rm = TRUE)
    dp <- sd(valores, na.rm = TRUE)
    mediana <- median(valores, na.rm = TRUE)
    
    # Calcular os bins manualmente para ter controle preciso
    min_val <- min(valores, na.rm = TRUE)
    max_val <- max(valores, na.rm = TRUE)
    
    # Determinar o número de bins baseado na faixa de valores
    range_val <- max_val - min_val
    
    if (range_val == 0) {
      return(plotly_empty())
    }
    
    # Ajustar o número de bins baseado na faixa e no tipo de dados
    if (unidade == "ml/L") {
      # Para oxigênio dissolvido (valores pequenos)
      bin_width <- range_val / 20
      if (bin_width < 0.01) bin_width <- 0.01
    } else if (unidade == "μg/L") {
      # Para clorofila
      bin_width <- range_val / 15
      if (bin_width < 0.1) bin_width <- 0.1
    } else if (unidade == "μS/cm") {
      # Para condutividade
      bin_width <- range_val / 20
      if (bin_width < 1) bin_width <- 1
    } else if (unidade == "°C") {
      # Para temperatura
      bin_width <- range_val / 15
      if (bin_width < 0.1) bin_width <- 0.1
    } else {
      # Para PAR
      bin_width <- range_val / 20
      if (bin_width < 1) bin_width <- 1
    }
    
    # Criar breaks (limites dos bins)
    breaks <- seq(from = floor(min_val/bin_width)*bin_width, 
                  to = ceiling(max_val/bin_width)*bin_width, 
                  by = bin_width)
    
    # Calcular frequências manualmente
    freq <- hist(valores, breaks = breaks, plot = FALSE)$counts
    
    # Criar posições x (centro dos bins)
    x_pos <- (breaks[-length(breaks)] + breaks[-1]) / 2
    
    # Criar texto para hover com intervalo correto
    hover_text <- paste0(
      titulo, ": ", 
      round(breaks[-length(breaks)], ifelse(unidade == "ml/L", 3, 2)), 
      " a ", 
      round(breaks[-1], ifelse(unidade == "ml/L", 3, 2)), 
      " ", unidade,
      "<br>Frequência: ", freq
    )
    
    # Criar gráfico com barras
    plot_ly() %>%
      add_bars(
        x = x_pos,
        y = freq,
        width = bin_width * 0.9,  # 90% da largura para espaçamento
        marker = list(
          color = cor,
          line = list(color = 'rgba(0,0,0,0.5)', width = 1)
        ),
        hoverinfo = 'text',
        text = hover_text,
        name = titulo
      ) %>%
      layout(
        title = paste(
          titulo, 
          "<br>Média = ", round(media, ifelse(unidade == "ml/L", 3, 2)), 
          " ", unidade, 
          ", DP = ", round(dp, ifelse(unidade == "ml/L", 3, 2)), " ", unidade
        ),
        xaxis = list(
          title = paste(titulo, "(", unidade, ")"),
          range = c(min_val, max_val),
          tickformat = ifelse(unidade == "ml/L", ".3f", 
                              ifelse(unidade == "μg/L", ".1f",
                                     ifelse(unidade == "°C", ".1f", "d")))
        ),
        yaxis = list(title = "Frequência"),
        bargap = 0.05,
        shapes = list(
          list(
            type = "line",
            x0 = media,
            x1 = media,
            y0 = 0,
            y1 = max(freq, na.rm = TRUE),
            line = list(color = "red", width = 2, dash = "dash")
          ),
          list(
            type = "line",
            x0 = media - dp,
            x1 = media - dp,
            y0 = 0,
            y1 = max(freq, na.rm = TRUE),
            line = list(color = "orange", width = 1.5, dash = "dot")
          ),
          list(
            type = "line",
            x0 = media + dp,
            x1 = media + dp,
            y0 = 0,
            y1 = max(freq, na.rm = TRUE),
            line = list(color = "orange", width = 1.5, dash = "dot")
          )
        ),
        annotations = list(
          list(
            x = media,
            y = max(freq, na.rm = TRUE),
            text = paste("Média:", round(media, ifelse(unidade == "ml/L", 3, 2))),
            showarrow = FALSE,
            font = list(color = "red", size = 10)
          ),
          list(
            x = media - dp,
            y = max(freq, na.rm = TRUE) * 0.95,
            text = "Média - DP",
            showarrow = FALSE,
            font = list(color = "orange", size = 9)
          ),
          list(
            x = media + dp,
            y = max(freq, na.rm = TRUE) * 0.95,
            text = "Média + DP",
            showarrow = FALSE,
            font = list(color = "orange", size = 9)
          )
        )
      )
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
      source("API.R", local = env_api)
      
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
              categoria_chl_3m = classificar_variavel(chl_3m),
              categoria_chl_4m = classificar_variavel(chl_4m),
              categoria_chl_media = classificar_variavel(chl_media),
              categoria_do_1m = classificar_variavel(do_1m),
              categoria_do_3m = classificar_variavel(do_3m)
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
        ultima_categoria == "Muito Alto" ~ "red",
        ultima_categoria == "Alto" ~ "orange",
        ultima_categoria == "Moderado" ~ "yellow",
        ultima_categoria == "Baixo" ~ "green",
        ultima_categoria == "Muito Baixo" ~ "blue",
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
      
      if (length(ultimo_valor) == 0) ultimo_valor <- 0
      
      valueBox(
        value = sprintf("%.1f °C", ultimo_valor),
        subtitle = "Temperatura 3.0m",
        icon = icon("thermometer-half"),
        color = "yellow",
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
        ultima_categoria == "Muito Alto" ~ "red",
        ultima_categoria == "Alto" ~ "orange",
        ultima_categoria == "Moderado" ~ "yellow",
        ultima_categoria == "Baixo" ~ "green",
        ultima_categoria == "Muito Baixo" ~ "blue",
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
  
  # Gráfico de série temporal da clorofila
  output$grafico_serie <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~chl_3m, type = 'scatter', mode = 'lines',
                name = 'Clorofila 3m', 
                line = list(color = 'green', width = 2),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Clorofila 3m: ", round(chl_3m, 2), "μg/L",
                  "<br>Categoria: ", categoria_chl_3m
                )) %>%
      add_trace(y = ~chl_4m, type = 'scatter', mode = 'lines',
                name = 'Clorofila 4.5m', 
                line = list(color = 'darkgreen', width = 2, dash = 'dash'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Clorofila 4.5m: ", round(chl_4m, 2), "μg/L",
                  "<br>Categoria: ", categoria_chl_4m
                )) %>%
      layout(
        title = "Evolução da Clorofila",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "Clorofila (μg/L)"),
        hovermode = 'closest',
        legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  output$info_serie <- renderText({
    if (!is.null(valores$dados_processados)) {
      paste("Dados reais da API SIMCOSTA -", 
            nrow(valores$dados_processados), "registros -",
            "Período:", format(min(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y"),
            "a", format(max(valores$dados_processados$data_hora, na.rm = TRUE), "%d/%m/%Y"))
    } else {
      "Execute a API.R para carregar dados"
    }
  })
  
  # Gráfico de temperatura
  output$grafico_temp <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~temp_1m, type = 'scatter', mode = 'lines',
                name = 'Temp 1m', 
                line = list(color = 'red', width = 2),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Temp 1m: ", round(temp_1m, 1), "°C"
                )) %>%
      add_trace(y = ~temp_3m, type = 'scatter', mode = 'lines',
                name = 'Temp 3m', 
                line = list(color = 'orange', width = 2, dash = 'dash'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Temp 3m: ", round(temp_3m, 1), "°C"
                )) %>%
      layout(
        title = "Evolução da Temperatura",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "Temperatura (°C)"),
        hovermode = 'closest',
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  # Gráfico de oxigênio dissolvido (convertido) COM CLASSIFICAÇÃO
  output$grafico_do <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~do_1m, type = 'scatter', mode = 'lines',
                name = 'OD 1m', 
                line = list(color = 'blue', width = 2),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>OD 1m: ", round(do_1m, 3), "ml/L",
                  "<br>Categoria: ", categoria_do_1m
                )) %>%
      add_trace(y = ~do_3m, type = 'scatter', mode = 'lines',
                name = 'OD 3m', 
                line = list(color = 'lightblue', width = 2, dash = 'dash'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>OD 3m: ", round(do_3m, 3), "ml/L",
                  "<br>Categoria: ", categoria_do_3m
                )) %>%
      layout(
        title = "Evolução do Oxigênio Dissolvido (ml/L)",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "OD (ml/L)"),
        hovermode = 'closest',
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  # Gráfico de condutividade
  output$grafico_cond <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~cond_1m, type = 'scatter', mode = 'lines',
                name = 'Cond 1m', 
                line = list(color = 'purple', width = 2),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Condutividade 1m: ", round(cond_1m, 0), "μS/cm"
                )) %>%
      add_trace(y = ~cond_3m, type = 'scatter', mode = 'lines',
                name = 'Cond 3m', 
                line = list(color = 'violet', width = 2, dash = 'dash'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>Condutividade 3m: ", round(cond_3m, 0), "μS/cm"
                )) %>%
      layout(
        title = "Evolução da Condutividade",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "Condutividade (μS/cm)"),
        hovermode = 'closest',
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  # Gráfico de PAR
  output$grafico_par <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~PAR, type = 'scatter', mode = 'lines',
                name = 'PAR', 
                line = list(color = 'gold', width = 2),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>PAR: ", round(PAR, 0), "μE"
                )) %>%
      layout(
        title = "Evolução da Radiação PAR",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "PAR (μE)"),
        hovermode = 'closest',
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  # Gráfico detalhado do oxigênio dissolvido COM CLASSIFICAÇÃO
  output$grafico_do_detalhado <- renderPlotly({
    req(valores$dados_processados)
    
    plot_ly(valores$dados_processados, x = ~data_hora) %>%
      add_trace(y = ~do_1m, type = 'scatter', mode = 'lines+markers',
                name = 'OD 1.0m', 
                line = list(color = 'blue', width = 2),
                marker = list(size = 4, color = 'blue'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>OD 1.0m: ", round(do_1m, 3), "ml/L",
                  "<br>Categoria: ", categoria_do_1m
                )) %>%
      add_trace(y = ~do_3m, type = 'scatter', mode = 'lines+markers',
                name = 'OD 3.0m', 
                line = list(color = 'lightblue', width = 2),
                marker = list(size = 4, color = 'lightblue'),
                hoverinfo = 'text',
                text = ~paste(
                  "Data: ", format(data_hora, "%d/%m/%Y %H:%M"),
                  "<br>OD 3.0m: ", round(do_3m, 3), "ml/L",
                  "<br>Categoria: ", categoria_do_3m
                )) %>%
      layout(
        title = "Série Temporal Detalhada do Oxigênio Dissolvido",
        xaxis = list(title = "Data/Hora"),
        yaxis = list(title = "OD (ml/L)"),
        hovermode = 'closest',
        legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(l = 60, r = 40, t = 60, b = 60)
      )
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
                                  levels = c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")),
        categoria_do_3m = factor(categoria_do_3m,
                                 levels = c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")),
        # Status HTML para clorofila
        status_chl = case_when(
          categoria_chl_3m == "Muito Alto" ~ '<span class="badge bg-danger">MUITO ALTO</span>',
          categoria_chl_3m == "Alto" ~ '<span class="badge bg-warning">ALTO</span>',
          categoria_chl_3m == "Moderado" ~ '<span class="badge bg-info">MODERADO</span>',
          categoria_chl_3m == "Baixo" ~ '<span class="badge bg-success">BAIXO</span>',
          categoria_chl_3m == "Muito Baixo" ~ '<span class="badge bg-secondary">MUITO BAIXO</span>',
          TRUE ~ '<span class="badge bg-secondary">N/A</span>'
        ),
        # Status HTML para oxigênio
        status_do = case_when(
          categoria_do_3m == "Muito Alto" ~ '<span class="badge bg-danger">MUITO ALTO</span>',
          categoria_do_3m == "Alto" ~ '<span class="badge bg-warning">ALTO</span>',
          categoria_do_3m == "Moderado" ~ '<span class="badge bg-info">MODERADO</span>',
          categoria_do_3m == "Baixo" ~ '<span class="badge bg-success">BAIXO</span>',
          categoria_do_3m == "Muito Baixo" ~ '<span class="badge bg-secondary">MUITO BAIXO</span>',
          TRUE ~ '<span class="badge bg-secondary">N/A</span>'
        )
      ) %>%
      select(data_hora, 
             chl_3m, categoria_chl_3m, status_chl,
             do_3m, categoria_do_3m, status_do,
             chl_4m, temp_1m, temp_3m, 
             do_1m, cond_1m, cond_3m, PAR)
    
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
    media_chl_3m <- mean(valores$dados_processados$chl_3m, na.rm = TRUE)
    dp_chl_3m <- sd(valores$dados_processados$chl_3m, na.rm = TRUE)
    cat("Clorofila a 3.0m:\n")
    cat("  Média:    ", round(media_chl_3m, 2), "μg/L\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$chl_3m, na.rm = TRUE), 2), "μg/L\n")
    cat("  Desvio:   ", round(dp_chl_3m, 2), "μg/L\n")
    cat("  Média - 2DP: ", round(media_chl_3m - 2*dp_chl_3m, 2), "μg/L (Muito Baixo até aqui)\n")
    cat("  Média - DP:  ", round(media_chl_3m - dp_chl_3m, 2), "μg/L (Baixo até aqui)\n")
    cat("  Média + DP:  ", round(media_chl_3m + dp_chl_3m, 2), "μg/L (Moderado até aqui)\n")
    cat("  Média + 2DP: ", round(media_chl_3m + 2*dp_chl_3m, 2), "μg/L (Alto até aqui)\n")
    cat("  Acima: Muito Alto\n\n")
    
    # Oxigênio Dissolvido 3.0m
    media_do_3m <- mean(valores$dados_processados$do_3m, na.rm = TRUE)
    dp_do_3m <- sd(valores$dados_processados$do_3m, na.rm = TRUE)
    cat("Oxigênio Dissolvido a 3.0m (ml/L):\n")
    cat("  Média:    ", round(media_do_3m, 3), "ml/L\n")
    cat("  Mediana:  ", round(median(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Mínimo:   ", round(min(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Máximo:   ", round(max(valores$dados_processados$do_3m, na.rm = TRUE), 3), "ml/L\n")
    cat("  Desvio:   ", round(dp_do_3m, 3), "ml/L\n")
    cat("  Média - 2DP: ", round(media_do_3m - 2*dp_do_3m, 3), "ml/L (Muito Baixo até aqui)\n")
    cat("  Média - DP:  ", round(media_do_3m - dp_do_3m, 3), "ml/L (Baixo até aqui)\n")
    cat("  Média + DP:  ", round(media_do_3m + dp_do_3m, 3), "ml/L (Moderado até aqui)\n")
    cat("  Média + 2DP: ", round(media_do_3m + 2*dp_do_3m, 3), "ml/L (Alto até aqui)\n")
    cat("  Acima: Muito Alto\n\n")
    
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
    cat("Classificação da Clorofila 3.0m (baseado em desvio padrão):\n")
    categorias_chl <- table(valores$dados_processados$categoria_chl_3m)
    for (cat in c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")) {
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
    cat("Classificação do Oxigênio Dissolvido 3.0m (baseado em desvio padrão):\n")
    categorias_do <- table(valores$dados_processados$categoria_do_3m)
    for (cat in c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")) {
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
  
  # Histograma da clorofila 3m - VERSÃO CORRIGIDA
  output$grafico_hist_chl <- renderPlotly({
    req(valores$dados_processados)
    criar_histograma(valores$dados_processados, "chl_3m", "Clorofila 3.0m", 
                     'rgba(100, 200, 102, 0.7)', "μg/L")
  })
  
  output$info_hist_chl <- renderText({
    req(valores$dados_processados)
    
    media_chl <- mean(valores$dados_processados$chl_3m, na.rm = TRUE)
    dp_chl <- sd(valores$dados_processados$chl_3m, na.rm = TRUE)
    
    paste("Classificação: Valores < ", round(media_chl - 2*dp_chl, 2), " = Muito Baixo | ",
          round(media_chl - 2*dp_chl, 2), " a ", round(media_chl - dp_chl, 2), " = Baixo | ",
          round(media_chl - dp_chl, 2), " a ", round(media_chl + dp_chl, 2), " = Moderado | ",
          round(media_chl + dp_chl, 2), " a ", round(media_chl + 2*dp_chl, 2), " = Alto | ",
          "> ", round(media_chl + 2*dp_chl, 2), " = Muito Alto")
  })
  
  # Histograma detalhado do oxigênio dissolvido (3.0m) - VERSÃO CORRIGIDA
  output$grafico_hist_do_detalhado <- renderPlotly({
    req(valores$dados_processados)
    criar_histograma(valores$dados_processados, "do_3m", "Oxigênio Dissolvido 3.0m", 
                     'rgba(52, 152, 219, 0.7)', "ml/L")
  })
  
  output$info_hist_do <- renderText({
    req(valores$dados_processados)
    
    media_do <- mean(valores$dados_processados$do_3m, na.rm = TRUE)
    dp_do <- sd(valores$dados_processados$do_3m, na.rm = TRUE)
    
    paste("Classificação OD: Valores < ", round(media_do - 2*dp_do, 3), " = Muito Baixo | ",
          round(media_do - 2*dp_do, 3), " a ", round(media_do - dp_do, 3), " = Baixo | ",
          round(media_do - dp_do, 3), " a ", round(media_do + dp_do, 3), " = Moderado | ",
          round(media_do + dp_do, 3), " a ", round(media_do + 2*dp_do, 3), " = Alto | ",
          "> ", round(media_do + 2*dp_do, 3), " = Muito Alto")
  })
  
  # Histograma da condutividade (3.0m) - VERSÃO CORRIGIDA
  output$grafico_hist_cond <- renderPlotly({
    req(valores$dados_processados)
    criar_histograma(valores$dados_processados, "cond_3m", "Condutividade 3.0m", 
                     'rgba(155, 89, 182, 0.7)', "μS/cm")
  })
  
  # Histograma da temperatura (3.0m) - VERSÃO CORRIGIDA
  output$grafico_hist_temp <- renderPlotly({
    req(valores$dados_processados)
    criar_histograma(valores$dados_processados, "temp_3m", "Temperatura 3.0m", 
                     'rgba(231, 76, 60, 0.7)', "°C")
  })
  
  # Histograma do PAR - VERSÃO CORRIGIDA
  output$grafico_hist_par <- renderPlotly({
    req(valores$dados_processados)
    criar_histograma(valores$dados_processados, "PAR", "PAR", 
                     'rgba(241, 196, 15, 0.7)', "μE")
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
