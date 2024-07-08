library(shiny)
library(sets)
library(shinythemes)
library(DT)
library(shinyFiles)
library(dplyr)
library(ggplot2)

sets_options("universe", seq(from = 0, to = 10, by = 0.1))

if (file.exists("variables.R") && file.exists("rules.R")) {
  source("variables.R")
  source("rules.R")
} else {
  stop("Arquivos variables.R ou rules.R não encontrados")
}

ui <- navbarPage(
  title = "FSTA - FUZZY SET TEXT ANALYSIS",
  tabPanel("Criar e Exportar Banco de Dados",
           fluidPage(
             titlePanel("Criar e Exportar Banco de Dados"),
             sidebarLayout(
               sidebarPanel(
                 textInput("frase", "Frase", ""),
                 dateInput("data", "Data", value = Sys.Date()),
                 textInput("local", "Local", ""),
                 textInput("cidade", "Cidade", ""),
                 selectInput("ataqueInstituicoes", "Ataque às Instituições",
                             choices = c("Nenhuma", "Recomendação e Sugestão", "Pedido e Afirmação", "Ordem")),
                 selectInput("ataqueLaicidade", "Ataque à Laicidade",
                             choices = c("Nenhuma", "Recomendação e Sugestão", "Pedido e Afirmação", "Ordem")),
                 selectInput("ataqueMidia", "Ataque à Grande Mídia",
                             choices = c("Nenhuma", "Recomendação e Sugestão", "Pedido e Afirmação", "Ordem")),
                 actionButton("add", "Adicionar"),
                 #Botão de importar
                 fileInput("file1", "Escolha o arquivo CSV:",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 #/Botão e importar
                 downloadButton("downloadData", "Download CSV")
               ),
               mainPanel(
                 DTOutput("table")
               )
             )
           )),
  #tabPanel("Análise", fluidPage(
    #sidebarLayout(
      #sidebarPanel(
        #verbatimTextOutput("valueLaicidade"),
        #verbatimTextOutput("valueGrandeMidia"),
        #verbatimTextOutput("valueInstituicoes"),
        #actionButton("calculate", "Gerar")
      #),
      #mainPanel(
        #textOutput("populismotext"),
        #plotOutput("fuzzyPlot")
      #)
    #)
  #)#)#,
  tabPanel("Análise2",
           sidebarLayout(
             sidebarPanel(
               sliderInput("slidlaicidadeid2", "Laicidade", min = 0, max = 10, value = 5),
               sliderInput("slidgrandemidiaid2", "Grande Mídia", min = 0, max = 10, value = 5),
               sliderInput("slidinstituicoesid2", "Instituições", min = 0, max = 10, value = 5),
               actionButton("calculate2", "Gerar")
             ),
             mainPanel(
               textOutput("populismotext2"),
               plotOutput("fuzzyPlot2")
             )
           )),
  tabPanel("Banco de Dados Derivado",
           fluidPage(
             titlePanel("Banco de Dados Derivado"),
             DTOutput("derivedDataTable"),
             plotOutput("derivedDataPlot")  # Adicionando plotOutput aqui
           ))
)

server <- function(input, output, session) {
  # Função para imprimir logs
  printLog <- function(msg) {
    cat(paste(Sys.time(), "-", msg, "\n"))
  }
  ########## BANCO DE DADOS ############
  values <- reactiveValues()
  values$data <- data.frame(
    Frase = character(),
    Data = as.Date(character()),
    Local = character(),
    Cidade = character(),
    AtaqueInstituicoes = character(),
    AtaqueLaicidade = character(),
    AtaqueMidia = character(),
    Remover = character(),
    stringsAsFactors = FALSE
  )
  
  observeEvent(input$add, {
    newLine <- data.frame(
      Frase = input$frase,
      Data = as.Date(input$data),
      Local = input$local,
      Cidade = input$cidade,
      AtaqueInstituicoes = input$ataqueInstituicoes,
      AtaqueLaicidade = input$ataqueLaicidade,
      AtaqueMidia = input$ataqueMidia,
      Remover = "Remover",
      stringsAsFactors = FALSE
    )
    values$data <- rbind(values$data, newLine)
  })
  
  output$table <- renderDT({
    datatable(values$data, escape = FALSE, selection = 'none', options = list(dom = 'Bfrtip')) %>% 
      formatStyle(columns = ncol(values$data), valueColumns = ncol(values$data), target = 'cell', cursor = 'pointer')
  }, server = FALSE)
  
  observeEvent(input$table_cell_clicked, {
    info <- input$table_cell_clicked
    row <- info$row
    col <- info$col
    if (!is.null(row) && col == ncol(values$data)) { # Checa se o botão de remover foi clicado
      values$data <- values$data[-row, ]
      replaceData(dataTableProxy('table'), values$data)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )
  ############ FIM BANCO DE DADOS ############
  ############ INÍCIO SISTEMA FUZZY2 ##########
  observeEvent(input$calculate2, {
    variables <- setVariables()
    rules <- setRules()
    system <- fuzzy_system(variables, rules)
    
    inputs <- list(
      laicidade = input$slidlaicidadeid2,
      grandemidia = input$slidgrandemidiaid2,
      instituicoes = input$slidinstituicoesid2
    )
    
    inference_result <- fuzzy_inference(system, inputs)
    tip2 <- gset_defuzzify(inference_result, "centroid")
    
    output$populismotext2 <- renderText({
      paste("Nível de populismo calculado:", tip2)
    })
    # Renderizando o gráfico do sistema fuzzy
    output$fuzzyPlot2 <- renderPlot({
      plot(system)
    })
  })
  ########### FIM SISTEMA FUZZY ############
  
  # Importando banco de dados
  observeEvent(input$file1, {
    req(input$file1)
    # Ler o arquivo e supor que é um CSV
    data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    # Aqui você pode adicionar a data ao seu 'values$data' ou processar como preferir
    values$data <- rbind(values$data, data)
    # Atualizar a tabela mostrada no UI
    output$table <- renderDT({
      datatable(values$data)
    })
  })
  # /Importando banco de dados
  
  # Banco de dados derivado
  derivedData <- reactive({
    levels_map <- c("Nenhuma" = 0, "Recomendação e Sugestão" = 1, "Pedido e Afirmação" = 2, "Ordem" = 3)
    
    data <- values$data %>%
      mutate(
        AtaqueInstituicoes = levels_map[AtaqueInstituicoes],
        AtaqueLaicidade = levels_map[AtaqueLaicidade],
        AtaqueMidia = levels_map[AtaqueMidia]
      ) %>%
      group_by(Data) %>%
      summarise(
        AtaqueInstituicoes = sum(AtaqueInstituicoes, na.rm = TRUE),
        AtaqueLaicidade = sum(AtaqueLaicidade, na.rm = TRUE),
        AtaqueMidia = sum(AtaqueMidia, na.rm = TRUE)
      ) %>%
      rowwise() %>%
      mutate(
        AtaqueInstituicoes = pmin(AtaqueInstituicoes, 10),
        AtaqueLaicidade = pmin(AtaqueLaicidade, 10),
        AtaqueMidia = pmin(AtaqueMidia, 10),
        SomatorioPonderado = sum(c(AtaqueInstituicoes, AtaqueLaicidade, AtaqueMidia), na.rm = TRUE),
        GrauPertencimento = {
          variables <- setVariables()
          rules <- setRules()
          system <- fuzzy_system(variables, rules)
          inputs <- list(
            laicidade = AtaqueLaicidade,
            grandemidia = AtaqueMidia,
            instituicoes = AtaqueInstituicoes
          )
          inference_result <- fuzzy_inference(system, inputs)
          gset_defuzzify(inference_result, "centroid")
        }
      ) %>%
      ungroup()
    
    data
  })
  
  output$derivedDataTable <- renderDT({
    datatable(derivedData())
  })
  output$derivedDataPlot <- renderPlot({
    derived_data <- derivedData()
    ggplot(derived_data, aes(x = as.Date(Data), y = GrauPertencimento)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Evolução do Grau de Pertencimento ao Longo do Tempo",
           x = "Data",
           y = "Grau de Pertencimento") +
      theme_minimal()
  })
}

shinyApp(ui, server)
