library(shiny)
library(wordcloud2)
library(partykit)
library(igraph)
library(tidytext)
library(rclipboard)
library(data.table)
library(udpipe)
library(bslib)
library(tidyverse)
library(DT)
library(DBI)
library(factoextra)
library(FactoMineR)
library(lubridate)
library(viridis)
library(plotly)
library(RPostgreSQL)
library(bsicons)
library(shinydashboard)
library(shinyjs)
library(av)
library(shinybusy)
library(shinyWidgets)
library(leaflet)
library(datamods)
library(htmltools)
library(corrplot)
library(RColorBrewer)
library(httr)
library(markdown)



# Define UI
dfempty <- data.frame(
  filename = character(),
  spk = character(),
  id_phone = character(),
  id_word = character(),
  id_ip = character(),
  center_Hz = numeric(),
  standard_pitch = numeric(),
  standard_dur = numeric(),
  standard_intensity = numeric(),
  IQR_Hz = numeric(),
  pitch_Hz_HZ = numeric(),
  intensity = numeric(),
  dur = numeric(),
  content = character(),
  word = character(),
  TOBI = character(),
  MAS_pattern = character()
)

dfempty2 <- data.frame(
  pitch_Hz=numeric(),
  intensity = numeric(),
  inflexion_st = numeric(),
  range_st = numeric(),
  dur = numeric(),
  velocidad = numeric(),
  dif_pitch_sign = numeric(),
  dif_range_sign = numeric(),
  dif_inten_sign = numeric(),
  dif_inflexion_sign = numeric(),
  dif_dur_sign = numeric(),
  dif_velocidad_sign = numeric(),
  stringsAsFactors = FALSE
)

db_file_path <- "furious.duckdb"

create_db_if_not_exists <- function() {
  if (!file.exists(db_file_path)) {
    # Create the DuckDB file and perform any initial setup
    con <- dbConnect(duckdb::duckdb(), dbdir = db_file_path)
    
    
    dbDisconnect(con)
  }
}

connect_db <- function() {
  # Create a connection to the SQLite database
  con <- dbConnect(duckdb::duckdb(), dbdir = "furious.duckdb")
  
  return(con)
}

# Example usage
con <- connect_db()


accordion_filters2 <- accordion(open = F,
                                accordion_panel(
                                  "Valores fónicos", icon = bsicons::bs_icon("file-earmark-music"),
                                  
                                  sliderInput("slider_pitch", "Pitch Mean", min = 0, max = 500, value = c(0, 500)),
                                  sliderInput("slider_intensity", "Intensity Mean", min = 0, max = 120, value = c(0, 120)),
                                  sliderInput("slider_inflexion", "Inflexion ST", min = -100, max = 100, value = c(-100, 100)),
                                  sliderInput("slider_range", "Range ST", min = 0, max = 200, value = c(0, 200)),
                                  sliderInput("slider_dur", "Dur", min = 0, max = 25000, value = c(0, 25000)),
                                  sliderInput("slider_velocidad", "Velocidad", min = 0, max = 25, value = c(0, 25)),
                                  sliderInput("slider_dif_pitch_sign", "Dif Pitch Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1),
                                  sliderInput("slider_dif_range_sign", "Dif Range Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1),
                                  sliderInput("slider_dif_inten_sign", "Dif Inten Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1),
                                  sliderInput("slider_dif_inflexion_sign", "Dif Inflexion Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1),
                                  sliderInput("slider_dif_dur_sign", "Dif Dur Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1),
                                  sliderInput("slider_dif_velocidad_sign", "Dif Velocidad Sign", min = -10, max = 10, value = c(-1.9, 1.9),step = 0.1)
                                  
                                  
                                  
                                ))


tree_summary <- function(tree) {
  
  # Get the terminal node IDs
  node_ids <- nodeids(tree, terminal = TRUE)
  
  # Get the data used to build the tree
  tree_data <- partykit::data_party(tree)
  
  # Get the response variable name
  response_var <- names(tree_data)[1]  # Assuming the response variable is the first column
  
  # Extract the response variable (speakers)
  tree_response <- tree_data[[response_var]]
  speaker_names <- levels(as.factor(tree_response))
  
  # Predict node assignments for each observation
  fitted_nodes <- predict(tree, type = "node")
  
  # Initialize an empty list to store the summaries
  summary_list <- list()
  
  # Loop over each terminal node
  for (node_id in node_ids) {
    
    # Extract the rule leading to this node
    rule <- partykit:::.list.rules.party(tree, i = node_id)
    
    # Get indices of observations in this node
    obs_in_node <- which(fitted_nodes == node_id)
    
    # If there are no observations in this node (unlikely but possible), skip
    if (length(obs_in_node) == 0) next
    
    # Get the response variable values for these observations
    responses_in_node <- tree_response[obs_in_node]
    
    # Calculate class counts and probabilities
    class_counts <- table(responses_in_node)
    probs_full <- rep(0, length(speaker_names))
    names(probs_full) <- speaker_names
    probs_full[names(class_counts)] <- as.numeric(class_counts)
    probs_full <- probs_full / sum(probs_full)
    
    # Format the probabilities
    prob_str <- paste0("Probabilities: ", 
                       paste0(speaker_names, ": ", round(probs_full * 100, 2), "%", collapse = "; "))
    
    # Get the predicted speaker
    predicted_speaker <- names(which.max(probs_full))
    
    # Create the summary text
    summary_text <- paste0("Rule: ", rule, "<br/>",
                           "Predicted Speaker: ", predicted_speaker, "<br/>",
                           prob_str, "<br/><br/>")
    
    # Append to the list
    summary_list <- c(summary_list, summary_text)
  }
  
  # Combine all summaries into a single text
  return(paste(summary_list, collapse = ""))
}






pca_summary <- function(famd_results, top_n = 5) {
  # Get the percentage of variance explained by the first two components
  eigenvalues <- famd_results$eig
  explained_variance <- paste0("The first component explains ", round(eigenvalues[1,2], 2), 
                               "% of the variance, and the second component explains ", 
                               round(eigenvalues[2,2], 2), "%.")
  
  # Get the contributions of variables to the first two components
  if (!is.null(famd_results$var$contrib)) {
    var_contrib <- famd_results$var$contrib[, 1:2]  # Contributions for the first two components
    
    # Find the top contributing variables for each component
    top_vars_dim1 <- head(sort(var_contrib[, 1], decreasing = TRUE), top_n)
    top_vars_dim2 <- head(sort(var_contrib[, 2], decreasing = TRUE), top_n)
    
    # Create summaries for the most important variables
    top_vars_summary_dim1 <- paste0("Top ", top_n, " variables for Component 1: ", 
                                    paste(names(top_vars_dim1), "(", round(top_vars_dim1, 2), "%)", collapse = ", "))
    
    top_vars_summary_dim2 <- paste0("Top ", top_n, " variables for Component 2: ", 
                                    paste(names(top_vars_dim2), "(", round(top_vars_dim2, 2), "%)", collapse = ", "))
  } else {
    top_vars_summary_dim1 <- "No variable contributions available for Component 1."
    top_vars_summary_dim2 <- "No variable contributions available for Component 2."
  }
  
  # Combine the explained variance with the top variables for each dimension
  full_summary <- paste(explained_variance, 
                        top_vars_summary_dim1, 
                        top_vars_summary_dim2, 
                        sep = "\n")
  
  return(full_summary)
}





boxplot_summary <- function(df) {
  # Select only the desired numeric variables
  selected_vars <- c("pitch_Hz", "intensity", "inflexion_st", "dur", "velocidad", "range_st")
  
  # Summarize the median of the selected variables by speaker
  summary_df <- df %>%
    group_by(spk) %>%
    summarise(across(all_of(selected_vars), median, na.rm = TRUE)) %>% 
    ungroup()
  
  # Create a string summary for each speaker without repeating
  summary_text <- summary_df %>%
    rowwise() %>%
    mutate(
      text = paste0(
        "Speaker ", spk, ": ",
        paste0(
          names(select(cur_data(), -spk)),
          ": Median = ",
          unlist(select(cur_data(), -spk)),
          collapse = "; "
        ),
        "<br/><br/>"
      )
    ) %>%
    pull(text)
  
  # Combine all summaries into one string
  return(paste(summary_text, collapse = "\n"))
}



check_and_save_to_duckdb <- function(db, table_name, new_data, id_column) {
  if (dbExistsTable(db, table_name)) {
    existing_data <- dbReadTable(db, table_name)
    new_ids <- new_data[[id_column]]
    existing_ids <- existing_data[[id_column]]
    rows_to_insert <- new_data[!new_ids %in% existing_ids, ]
    
    if (nrow(rows_to_insert) > 0) {
      dbWriteTable(db, table_name, rows_to_insert, append = TRUE)
      return(TRUE)  # Data was inserted
    } else {
      return(FALSE)  # Data already exists
    }
  } else {
    dbWriteTable(db, table_name, new_data)
    return(TRUE)  # Data was inserted
  }
}

interpolate_by_filename <- function(data) {
  new_times <- seq(min(data$time), max(data$time), by = 0.01)
  interpolated_pitch <- approx(data$time, data$pitch, xout = new_times, method = "linear")
  interpolated_intensity <- approx(data$time, data$intensity, xout = new_times, method = "linear")
  
  interpolated_df <- data.frame(
    filename = unique(data$filename),
    time = new_times,
    pitch = interpolated_pitch$y,
    intensity = interpolated_intensity$y
  )
  
  return(interpolated_df)
}

ui <- fluidPage(
  useShinyjs(),
  rclipboardSetup(),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/howler/2.2.3/howler.min.js"),
    
    tags$link(rel = "stylesheet", href = "https://cdn.plyr.io/3.6.8/plyr.css"),
    tags$script(src = "https://cdn.plyr.io/3.6.8/plyr.polyfilled.js"),
    tags$style(HTML("
      .modal-title {
        color: red;
        font-weight: bold;
        font-family: 'Arial', sans-serif;
        font-size: 24px;
      }
    "))
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "united",
    primary = "#E30000",
    secondary = "#2D2C2C",
    base_font = font_google("Roboto", local = TRUE),
    heading_font = font_google("Roboto", local = TRUE),
    code_font = "Roboto"
  ),
  navbarPage(
    title = tags$img(src="furious_logo.png", width = "80px", height = "80px"),
    
    tabPanel("FURIOUS",
             
             layout_sidebar(
               sidebar = sidebar(
                 hidden(actionButton("refresh_button", "Refresh Page")),
                 # tags$img(src="furious_logo.png", width = "80px", height = "80px"),
                 
                 selectInput("furious_filename","Selecciona discurso",choices= NULL),
                 varSelectInput("furious_var","Selecciona variable",data = dfempty2,selected = "pitch_Hz"),
                 fluidRow(
                   column(4, tags$a(href = "https://github.com/acabedo/furious/blob/main/tutorial.md", "Tutorial", target = "_blank")),
                   column(8, actionButton("furious_button", "Crea furioso", ))
                 ),             tags$hr(),
                 tags$strong("Define normalidad aquí"),
                 accordion_filters2,open = T),
               fluidRow( 
                
                 tabsetPanel(
                   
                   tabPanel("Visión térmica",
                            fluidRow(
                              selectInput("heatmap_vars", 
                                          "Elige variables",width = "100%", multiple = TRUE,
                                          choices = c("phon_pitch_Hz", 
                                                      "phon_intensity", 
                                                      "phon_inflexion_st", 
                                                      "phon_range_st", 
                                                      "phon_pausa_intra", 
                                                      "phon_pausa_inter", 
                                                      "phon_dur", 
                                                      "phon_velocidad", 
                                                      "morph_qnoun", 
                                                      "morph_qverb", 
                                                      "morph_qadv", 
                                                      "morph_qaux", 
                                                      "morph_qcconj", 
                                                      "morph_qsconj", 
                                                      "morph_qadj", 
                                                      "morph_qdet", 
                                                      "morph_qpron", 
                                                      "morph_qpos", 
                                                      "morph_qneg"),
                                          selected = "phon_pitch_Hz")),
                            plotlyOutput("furious_plot1")),
                   
                   tabPanel("Por variable",
                            card(
                              height = 350,
                              full_screen = TRUE,
                              card_body(plotlyOutput("furious_plot",height="350px"))
                            )
                   ))
               ),
               fluidRow(
                 tabsetPanel(
                   tabPanel("Contexto",
                            fluidRow(
                              column(8,  # Adjust the width as necessary (e.g., 12 is full width)
                                     sliderInput("timecontext",width="100%", "Indica segundos para contexto", min = 1, max = 3600, value = 10)),
                              column(4,
                                     fluidRow(
                                       column(4,actionButton("show_legend", label = "Leyendas", icon = icon("question-circle"))),
                                       column(8, checkboxInput("add_icons", "Leyendas prosódicas", value = FALSE),
                                              # checkboxInput("add_sentiments", "Leyendas emocionales", value = FALSE),
                                              checkboxInput("toggle_crossout", label = "Marcar solapamientos y risas", value = FALSE))),
                         
                              ))
                            
                            ,
                            downloadButton("download_transcription", "Descargar transcripción"),
                            uiOutput("content_output")),
                   tabPanel("Datos",DTOutput("dtlinear2")),
                   tabPanel("Boxplot",plotlyOutput("furious_boxplot")),
                   tabPanel("Wordcloud",
                            tabsetPanel(
                              selectInput("wordcloud_var", "Excluye estas variables",multiple = T, choices = c("DET","CCONJ","ADP","SCONJ"), selected = c("DET","CCONJ","ADP","SCONJ"),selectize = T),
                              tabPanel("Furioso",
                                       fluidRow(
                                         wordcloud2Output("wordcloud_yes", height = "400px")
                                       )
                              ),
                              tabPanel("No furioso",
                                       fluidRow(wordcloud2Output("wordcloud_no", height = "400px")
                                       )
                              )
                            )
                            
                   ),
                   tabPanel("Vis. térm hablante",
                            
                            fluidRow(plotlyOutput("heatmap_spk")
                            ),
                            fluidRow(DTOutput("heatmap_spk_table"))
                            
                   ),
                   tabPanel("Webchart",
                            sliderInput("sliderselect","Define límites",min=-100,max=100,value=c(-2,2),step=0.5),
                            
                            plotOutput("webchart",height = "750px",fill = T,width = "100%")
                   ),
                   tabPanel("PCA",
                            
                            tabsetPanel(
                              tabPanel("Variables",
                                       plotOutput("FAMD_plot_variables")),
                              tabPanel("Eigenvalues",
                                       plotOutput("FAMD_plot_eigenvalues")),
                              tabPanel("Individuals",
                                       plotOutput("FAMD_plot_categories",height = "600px",width="100%"))
                            )
                            
                   ),
                   tabPanel("Tree",
                            textOutput("spk_mapping_text"),
                            plotOutput("tree_plot")
                            
                   ),
                   
                   tabPanel("CHATGPT",
                            tabsetPanel(
                              tabPanel("Crea pregunta para CHATGPT",
                                       
                                       actionButton("generate_prompt", "Genera información para pegar en CHATGPT"),
                                       uiOutput("copy_button"),
                                       uiOutput("prompt_output"),
                                       
                                       
                              ),
                              tabPanel("Pregunta a CHATGPT",
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Pregunta a CHATGPT"),
                                           a(href="https://github.com/tolgakurtuluss/shinychatgpt", target="_blank",
                                             img(src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height="30px"),
                                             "Inspirado en shinychatgpt"
                                           ),
                                           tags$p(""),
                                           textInput("api_key", "API Key", "¡PEGA_TU_APIKEY_AQUÍ!"),
                                           tags$p("Crear API keys aquí:", 
                                                  tags$a(href = "https://platform.openai.com/account/api-keys", target="_blank", "https://platform.openai.com/account/api-keys")
                                           ),tags$hr(),
                                           selectInput("model_name", "¿Qué modelo quieres elegir?",
                                                       choices = c("gpt-4o-mini", "gpt-4o", "o1-mini"), selected = "gpt-4o-mini"),
                                           tags$hr(),
                                           sliderInput("temperature", "Temperature", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
                                           sliderInput("max_length", "Maximum Length", min = 1, max = 2048, value = 512, step = 1),
                                           tags$hr(),
                                           # textAreaInput(inputId = "sysprompt", label = "SYSTEM PROMPT",height = "200px", placeholder = "You are a helpful assistant."),
                                           tags$hr(),
                                           tags$div(
                                             style="text-align:center; margin-top: 15px; color: white; background-color: #FFFFFF",
                                             tags$br(),
                                             actionButton("send_message", "Enviar pregunta a CHATGPT",icon = icon("play"),height = "350px"),
                                             downloadButton("download_response", "Descargar respuesta")
                                           ),
                                           style = "background-color: #FFFFFF; color: black"
                                         )
                                         ,
                                         mainPanel(
                                           tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
                                           tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
                                           tags$style(type = "text/css", "label {font-weight: bold;}"),
                                           fluidRow(
                                             column(12,tags$h3("Respuesta de CHATGPT"),tags$hr(),uiOutput("chat_history"),tags$hr())
                                           ),
                                           fluidRow(
                                             textAreaInput(inputId = "user_message", placeholder = "Escribe la pregunta:", label="Pregunta a CHATGPT", width = "100%",height = "300px"),
                                             
                                           ),style = "background-color:# FFFFFF")
                                       )
                              ))
                            
                   )
                   )
               )
               
               
               
               
               
             )),
    tabPanel("CREATE",
             layout_sidebar(
               sidebar = sidebar(
                 
                 tags$script(
                   HTML(
                     "$(document).on('click', '#refresh_button', function() {
         location.reload();
       });"
                   )
                 ),
                 selectInput("language", "Select Language", choices = c("Spanish" = "spanish", "Catalan" = "catalan","English" = "english","French"="french","Italian"="italian")),
                 # checkboxInput("interpolate", "Apply Interpolation", FALSE),
                 radioButtons("dataorigin", "Your transcription is in", choices = c("Only in Ips"="only","Words and phones (only Spanish)"="notonly")),
                 
                 actionButton("process", "Do Oralstats thing!"),
                 tags$a(href = "https://github.com/acabedo/furious/blob/main/tutorial.md", "Tutorial", target = "_blank")
                 
               ),
               nav_panel(
                 title = "Búsqueda",
                 id = "main_tabs",
                 tabsetPanel(
                   tabPanel("Upload",
                            fluidRow(
                              fileInput("files", "Choose TXT Files*", multiple = TRUE, accept = ".txt"),
                              fileInput("pitchFiles", "Upload Pitch Files*", multiple = TRUE, accept = c(".txt")),
                              fileInput("intensityFiles", "Upload Intensity Files*", multiple = TRUE, accept = c(".txt")),
                              fileInput("sentimentFile", "Upload Sentiment File (optional)", multiple = FALSE, accept = c(".csv")),
                              fileInput("audio_files", "Choose Audio Files (only MP3)*", multiple = TRUE, accept = c("audio/mp3"))
                            )),
                   tabPanel("Prosody Data", DTOutput("prosodyTable")),
                   tabPanel("Words Data", DTOutput("wordsdf")),
                   tabPanel("Ips Data", DTOutput("ipsdf")),
                   tabPanel("Turns Data", DTOutput("turnsdf")),
                   tabPanel("Download",
                            
                            tooltip(
                              span("Warning", bs_icon("info-circle")),
                              "Downloads are only available after creation process.",
                              placement = "bottom"
                            ),
                            downloadButton("download_turns", "Download Turns CSV", disabled = TRUE),
                            downloadButton("download_ips", "Download Ips CSV", disabled = TRUE),
                            downloadButton("download_words", "Download Words CSV", disabled = TRUE)
                            
                   )
                 )
               )
             )
    )
  ),
  
  div(
    class = "footer", 
    style = "position: relative; bottom: 0; width: 100%; background-color: gray; color: #FFFFFF; padding: 10px; text-align: center;",
    HTML("© 2024 - <a style='color:black;' href='https://github.com/acabedo/furious/'>Oralstats Furious</a>. Adrián Cabedo Nebot | <a style='color:black;' href='https://creativecommons.org/licenses/by/4.0/deed.es'>CC BY 4.0</a>")
  )
  
  
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200 * 1024 ^ 2)
 
  showModal(modalDialog(
    title = tags$div(class = "modal-title", "Advertencia sobre Oralstats Furious"),
    "Esta aplicación todavía está en desarrollo; actualmente es operativa, pero puede contener errores. Gracias por usarlo.",
    easyClose = TRUE,
    footer = NULL
  ))
  if (!dir.exists("www/audios")) {
    dir.create("www/audios", recursive = TRUE)
  }
  
  observeEvent(input$audio_files, {
    req(input$audio_files)
    
    for (i in 1:nrow(input$audio_files)) {
      file.copy(input$audio_files$datapath[i], file.path("www/audios", input$audio_files$name[i]))
    }
  })
  
  observeEvent(input$process, {
    req(input$pitchFiles)
    req(input$intensityFiles)
    req(input$files)
    
    file_paths <- input$files$datapath
    file_names <- input$files$name
    
    df <- map2_df(file_names, file_paths, ~ fread(.y, sep = "\t", header = TRUE) %>%
                    mutate(filename = .x)) %>%
      filter(text != "") %>%
      rename(content = text) %>%
      mutate(
        filename = gsub("TXT", "", filename),
        filename = gsub(".txt", "", filename),
        tier_name = tier,
        source = filename,
        time_start = tmin,
        dur = tmax - tmin,
        time_end = tmax,
        spk = gsub(" -.*", "", tier_name),
        spk = paste(filename, spk, sep = "_")
      ) %>%
      relocate(time_end, .after = time_start) %>%
      relocate(tier_name, .after = content) %>%
      na.omit()
    
    withProgress(message = 'Processing files', value = 0, {
      incProgress(0.1, detail = "Reading pitch files")
      
      pitch <- map2_df(input$pitchFiles$name, input$pitchFiles$datapath, ~ fread(.y) %>% 
                         mutate(filename = tools::file_path_sans_ext(.x))) %>%
        rename(time = V1, pitch = V2) %>%
        mutate(time = round(time, 2),
               time_ms = round(time * 1000),
               pitch = round(pitch, 1),
               pitch_st = 12 * log2(pitch / 1))
      
      incProgress(0.4, detail = "Reading intensity files")
      
      intensity <- map2_df(input$intensityFiles$name, input$intensityFiles$datapath, ~ fread(.y) %>% 
                             mutate(filename = tools::file_path_sans_ext(.x))) %>%
        rename(time = `Time (s)`, intensity = `Intensity (dB)`) %>%
        mutate(time = round(time, 2),
               time_ms = round(time * 1000),
               intensity = round(intensity, 1))
      
      incProgress(0.7, detail = "Merging data")
      
      prosody <- merge(pitch, intensity, by = c("filename", "time"))
      prosody <- prosody[order(prosody$filename, prosody$time), ]
      prosody$id_prosody <- paste(prosody$filename, prosody$time_ms, sep = "_")
      
      if (input$interpolate) {
        incProgress(0.8, detail = "Applying interpolation")
        
        prosody <- prosody %>% group_split(filename) %>%
          map_df(interpolate_by_filename)
        
        prosody <- prosody %>%
          mutate(
            time_ms = round(time * 1000),
            id_prosody = paste(filename, time_ms, sep = "_"),
            pitch = round(pitch, 1),
            intensity = round(intensity, 1)
          )
      }
      
      if(input$dataorigin == "only"){
 
      
      # Process 'grupos'
      grupos <- df %>%
        mutate(id = paste(spk, time_start, sep = "_"))
      
      intervencionesdb <- grupos %>%arrange(filename,time_start)
      intervencionesdb$cambio <- ifelse(intervencionesdb$filename == lead(intervencionesdb$filename,1) & intervencionesdb$tier != lead(intervencionesdb$tier,1) ,
                                        intervencionesdb$intid <- "cambio", 
                                        ifelse(intervencionesdb$filename != lead(intervencionesdb$filename,1),
                                               intervencionesdb$intid <- "cambio",
                                               intervencionesdb$intid <- NA))
      intervencionesdb <-  intervencionesdb %>% group_by(filename,cambio) %>% mutate(intid = row_number())
      intervencionesdb$intid <- ifelse(intervencionesdb$cambio != "cambio", intervencionesdb$intid <- NA, intervencionesdb$intid <- intervencionesdb$intid)
      
      # Orden de las intervenciones
      
      intervencionesdb <-  intervencionesdb %>% group_by(filename) %>% 
        fill(intid, .direction = "up") #default direction down
      
      # time_start de las intervenciones
      
      intervencionesdb$time_start_int <- ifelse(lag(intervencionesdb$cambio == "cambio",1) | intervencionesdb$intid == 1, intervencionesdb$time_start_int <- intervencionesdb$time_start, intervencionesdb$time_start_int <- NA)
      intervencionesdb <-  intervencionesdb %>% group_by(filename) %>% 
        fill(time_start_int, .direction = "down") #default direction down
      
      # time_end de las intervenciones
      
      intervencionesdb$time_end_int <- ifelse(intervencionesdb$cambio == "cambio" | intervencionesdb$intid == 1, intervencionesdb$time_end_int <- intervencionesdb$time_end, intervencionesdb$time_end_int <- NA)
      intervencionesdb <-  intervencionesdb %>% group_by(filename) %>% 
        fill(time_end_int, .direction = "up") #default direction down
      
      # Duración de las intervenciones
      
      intervencionesdb$dur_int <- intervencionesdb$time_end_int - intervencionesdb$time_start_int
      
      intervencionesdb <-  intervencionesdb %>% group_by(filename,intid) %>% mutate(ordgeint = row_number())
      
      #Creación de las pausas
      
      intervencionesdb$pausa <- ifelse(intervencionesdb$filename == lead(intervencionesdb$filename,1) & intervencionesdb$tier == lead(intervencionesdb$tier,1), lead(intervencionesdb$time_start,1)-intervencionesdb$time_end,intervencionesdb$pausa <-NA)
      intervencionesdb$pausacod <- ifelse(intervencionesdb$pausa<500, intervencionesdb$pausacod <- "/", 
                                          ifelse(intervencionesdb$pausa>=501 & intervencionesdb$pausa <= 1000, intervencionesdb$pausacod <- "//", 
                                                 ifelse(intervencionesdb$pausa>1001,intervencionesdb$pausacod <- paste("/// (", round((intervencionesdb$pausa*0.001),1),")",sep = ""),intervencionesdb$pausacod <-NA))) 
      intervencionesdb$pausacod[is.na(intervencionesdb$pausacod)] <- " "
      
      #Creación del valor de fto
      
      intervencionesdb<- intervencionesdb%>%ungroup()%>%mutate(fto_post = ifelse((filename == lead(filename,1) 
                                                                                  & tier != lead(tier,1)), 
                                                                                 (lead(time_start,1)-time_end),NA),
                                                               fto_post_cod = ifelse(fto_post>0 & fto_post<50,"§",NA),
                                                               fto_pre = ifelse((filename == lag(filename,1) 
                                                                                 & tier != lag(tier,1)), 
                                                                                (time_start-lag(time_end,1)),NA),
                                                               fto_prev_cod = ifelse(fto_pre>0 & fto_pre<50,"§",NA))
      
      intervencionesdb <- intervencionesdb%>%mutate(content1 = ifelse(!is.na(fto_post_cod)|!is.na(fto_prev_cod),paste(fto_prev_cod,content,pausacod,fto_post_cod, sep = ""), content),
                                                    content1 = gsub("^NA","",content1),
                                                    content1 = gsub("NA$","",content1),
                                                    content1 = ifelse(!is.na(pausacod),paste(content1,pausacod, sep = ""), content1),
                                                    content1 = gsub("NA$","",content1))
      
      
      #Eliminación de los espacios iniciales
      
      intervencionesdb$content1 <- trimws(intervencionesdb$content1)
      
      #Creación de la variable "intervención" y de la base de datos "intervenciones"
      
      intervencionesdb <- intervencionesdb %>% 
        group_by(filename,intid) %>% 
        mutate(intervencion = paste0(content1, collapse = " "))
      intervencionesdb$intervencion <- ifelse(intervencionesdb$cambio !="cambio", intervencionesdb$intervencion <- NA, intervencionesdb$intervencion <-intervencionesdb$intervencion)
      intervencionesdb <- intervencionesdb %>% mutate(id_turn = paste0(spk,"_int_",time_start_int, sep=""),id_ip=id)
      intervenciones <- subset(intervencionesdb, intervencionesdb$cambio == "cambio", select=c(tier,spk,filename,time_end_int,time_start_int,intervencion))
      intervenciones <- intervenciones %>% mutate(id_turn = paste(spk,"_int_",time_start_int, sep=""))
      intervenciones$intervencion_export <- paste(intervenciones$tier,": ",intervenciones$intervencion,sep = "")
      intervenciones_export <- subset(intervenciones, select=c(time_start_int,intervencion_export,filename))
      intervenciones_export <- intervenciones_export %>% mutate(inicio = format(as.POSIXct(time_start_int , "UTC", origin = "1970-01-01"), "%H:%M:%OS2"))%>%select(inicio,intervencion_export,filename)
      
     
      
       tokenized <- grupos%>%mutate(content = gsub("t=","t=&",content),content = gsub('">','">&',content),content = gsub('"/>','"/>&',content))%>%unnest_tokens(content,content,token = stringr::str_split, pattern = '&',to_lower = FALSE)%>%mutate(content=ifelse(grepl('"',content),gsub(" +","_",content),content),cita=NA,fsr=NA,fsr_attr=NA,extranjero=NA,extranjero_attr=NA,risas=NA,enfasis=NA,obs=NA)%>%unnest_tokens(content,content,token = stringr::str_split, pattern = ' ',to_lower = FALSE)%>%mutate(
        enfasis=ifelse(grepl("<enfasis",content),"yes",NA),enfasis=ifelse(grepl("</enfasis>",lag(content,1)),"no",enfasis))%>%fill(enfasis,.direction = "down")%>%mutate(enfasis=ifelse(is.na(enfasis),"no",enfasis))%>%mutate(
          risas=ifelse(grepl("<entre",content),"yes",NA),risas=ifelse(grepl("</entre",lag(content,1)),"no",risas))%>%fill(risas,.direction = "down")%>%mutate(risas=ifelse(is.na(risas),"no",risas))%>%mutate(
            cita=ifelse(grepl("<cita",content),"yes",NA),cita=ifelse(grepl("</cita>",lag(content,1)),"no",cita))%>%fill(cita,.direction = "down")%>%mutate(cita=ifelse(is.na(cita),"no",cita))%>%mutate(
              fsr=ifelse(grepl("<fsr",content),"yes",NA),fsr=ifelse(grepl("</fsr>",lag(content,1)),"no",fsr))%>%fill(fsr,.direction = "down")%>%mutate(fsr=ifelse(is.na(fsr),"no",fsr))%>%mutate(
                anonimo=ifelse(grepl("<anonimo",content),"yes",NA),anonimo=ifelse(grepl("</anonimo>",lag(content,1)),"no",anonimo))%>%fill(anonimo,.direction = "down")%>%mutate(anonimo=ifelse(is.na(anonimo),"no",anonimo))%>%mutate(
                  extranjero=ifelse(grepl("<extranjero",content),"yes",NA),extranjero=ifelse(grepl("</extranjero>",lag(content,1)),"no",extranjero))%>%fill(extranjero,.direction = "down")%>%mutate(extranjero=ifelse(is.na(extranjero),"no",extranjero))%>%mutate(
                    solap=ifelse(grepl("\\[",content),"yes",NA),solap=ifelse(grepl("\\]",lag(content,1))&!grepl("\\[",content),"no",solap))%>%fill(solap,.direction = "down")%>%mutate(solap=ifelse(is.na(solap),"no",solap))%>%mutate(
                      interr=ifelse(grepl("\\¿",content),"yes",NA),interr=ifelse(grepl("\\?",lag(content,1))&!grepl("\\¿",content),"no",interr))%>%fill(interr,.direction = "down")%>%mutate(interr=ifelse(is.na(interr),"no",interr))%>% mutate(exclam=ifelse(grepl("\\¡",content),"yes",NA),exclam=ifelse(grepl("\\!",lag(content,1))&!grepl("\\¡",content),"no",exclam))%>%fill(exclam,.direction = "down")%>%mutate(exclam=ifelse(is.na(exclam),"no",exclam))%>%mutate(fsr_attr = ifelse(grepl("t=",lag(content,2))&grepl("fsr",lag(content,3)),lag(content,1),NA),extranjero_attr = ifelse(grepl("t=",lag(content,2))&grepl("extranjero",lag(content,3)),lag(content,1),NA),alargamiento=ifelse(grepl("<alargamiento",content),"yes","no"),content1 = gsub('\\¡|\\!|\\?|\\¿|".*"|\\[|\\]|-|–|\\(|\\)|<|\\/|>|entre_risas|fsr|obs|cita|extranjero|alargamiento|anonimo|t=|"|enfasis',"",content))%>%filter(content1!="")%>%group_by(id)%>%mutate(posicion_grupo = row_number())
      
      
      
      pathudpipe <- "spanish-ancora-ud-2.5-191206.udpipe"
      
      if(file.exists(pathudpipe)){
        spmodel <- udpipe_load_model("spanish-ancora-ud-2.5-191206.udpipe")} else {spmodel<- udpipe_download_model(language = "spanish-ancora")
        spmodel<-udpipe_load_model("spanish-ancora-ud-2.5-191206.udpipe")}
      
      tokenized_udpipe <- udpipe_annotate(x=tokenized$content1,object = spmodel, tokenizer = "vertical",tagger = "default", doc_id=tokenized$id)%>%as.data.frame()
      tokenized_tagged <- cbind(tokenized,tokenized_udpipe%>%rename(ulemma = lemma)%>%select(token,upos,ulemma)
                                # ,freeling%>%rename(freepos = pos,freelem = lemma)%>%select(content3,freepos,freelem),tagant
      )%>%group_by(id)%>%mutate(id_word = paste(filename,"_pal_",spk,"_",time_start,"_",row_number(),sep=""))
      
      pathsentimientos <- "diccionarios/sentimientos.txt"
      
      if (file.exists(pathsentimientos)) {
        sentimientos <- read.delim(pathsentimientos,header = TRUE,sep = ",")
      } else {
        NULL
      }
      
      if(exists("sentimientos")){
        
        tokenized_tagged_sent <- tokenized_tagged %>% left_join(sentimientos%>%distinct(palabra,.keep_all=TRUE), by=c("token"="palabra"))
        
      }
      
      prosody_j <- setDT(prosody%>%mutate(timems = time*1000))[setDT(grupos), on = .(filename, time >= time_start, time < time_end), id := id]
      
      # Definir cuartiles
      prosody_q <- prosody_j%>%group_by(id)%>%mutate(dur=max(time)-min(time),
                                                       quartil = ifelse(time <= first(time)+(dur*0.25),"q1",NA),
                                                       quartil = ifelse(time <= first(time)+(dur*0.5)&time >= first(time)+(dur*0.25),"q2",quartil),
                                                       quartil = ifelse(time <= first(time)+(dur*0.75)&time >= first(time)+(dur*0.5),"q3",quartil),
                                                       quartil = ifelse(is.na(quartil),"q4",quartil))
      
      # Aplicar resumen con los datos fónicos
      
      prosody_resumen <- prosody_q%>%group_by(id)%>%summarise(pitch_Hz=mean(pitch,na.rm = TRUE), intensity= mean(intensity, na.rm = TRUE),inflexion_st = 12*log2(mean(pitch[quartil=="q4"],trim=0.1,na.rm=TRUE)/mean(pitch[quartil=="q1"],trim=0.1,na.rm=TRUE)),range_st = 12*log2(max(pitch,na.rm=TRUE)/min(pitch,na.rm=TRUE)))%>%mutate_if(is.numeric,round,digits=1)
      prosodyq4 <- prosody_q%>%filter(quartil=="q4")%>%group_by(id)%>%summarise(pitch_Hz=mean(pitch,na.rm=TRUE),trim=0.1)
      prosodyq1 <- prosody_q%>%filter(quartil=="q1")%>%group_by(id)%>%summarise(pitch_Hz=mean(pitch,na.rm=TRUE),trim=0.1)
      
      
      grupos_ampliados <- grupos%>%left_join(prosody_resumen, by="id")%>%rename(id_ip = id)
      grupos_ampliados <- grupos_ampliados%>%left_join(intervencionesdb%>%select(id_turn,id_ip),by="id_ip")
      grupos_pos <- tokenized_tagged%>%group_by(id_ip = id)%>%summarise(
        qpalabras = n(),
        qnoun = sum(upos=="NOUN",na.rm = TRUE),
        qverb = sum(upos=="VERB",na.rm = TRUE),
        qadj = sum(upos=="ADJ",na.rm = TRUE),
        qadv = sum(upos=="ADV",na.rm = TRUE),
        qadp = sum(upos=="ADP",na.rm = TRUE),
        qcconj = sum(upos=="CCONJ",na.rm = TRUE),
        qdet = sum(upos=="DET",na.rm = TRUE),
        qaux = sum(upos=="AUX",na.rm = TRUE),
        qintj = sum(upos=="INTJ",na.rm = TRUE),
        qnum = sum(upos=="NUM",na.rm = TRUE),
        qpron = sum(upos=="PRON",na.rm = TRUE),
        qpropn = sum(upos=="PROPN",na.rm = TRUE),
        qsconj = sum(upos=="SCONJ",na.rm = TRUE)
      )%>%ungroup()
      grupos_ampliados <- grupos_ampliados%>%left_join(grupos_pos, by="id_ip")
      
      grupos_sent <- tokenized_tagged_sent%>%group_by(id_ip = id)%>%summarise(
        
        qpos = sum(sentimiento=="positivo",na.rm = TRUE),
        qneg = sum(sentimiento=="negativo",na.rm = TRUE)
        
      )%>%ungroup()
      
      grupos_ampliados <- grupos_ampliados %>%left_join(grupos_sent,by="id_ip")
      
      
      ## Relaciones----
      
      ### Palabra por grupo entonativo----
      
      cantidad_palabras_grupo <- tokenized_tagged%>%ungroup()%>%group_by(id_ip=id)%>%summarise(palabras=n())
      
      grupos_ampliados <- grupos_ampliados%>%left_join(cantidad_palabras_grupo,by="id_ip")
      grupos_ampliados <- grupos_ampliados%>%mutate(velocidad = palabras/(dur))%>%mutate_if(is.numeric,round,digits=2)
      
      speakers <- grupos_ampliados %>%group_by(spk)%>% summarise(grupos_entonativos_spk=n(),palabras_cantidad_spk=sum(palabras,na.rm = TRUE),palabras_mean_spk=mean(palabras,na.rm=TRUE),pitch_Hz_spk = mean(pitch_Hz,na.rm=TRUE),pitch_SD_spk = sd(pitch_Hz,na.rm=TRUE),range_mean_spk = mean(range_st,na.rm=TRUE),range_SD_spk = sd(range_st,na.rm=TRUE),intensity_mean_spk=mean(intensity,na.rm=TRUE),intensity_SD_spk=sd(intensity,na.rm=TRUE),inflexion_mean_spk=mean(inflexion_st,na.rm=TRUE),inflexion_SD_spk=sd(inflexion_st,na.rm=TRUE),  dur_mean_spk = mean(dur, na.rm=TRUE),dur_SD_spk = sd(dur, na.rm=TRUE),velocidad_spk=mean(velocidad,na.rm=TRUE),velocidad_SD_spk=sd(velocidad,na.rm=TRUE))%>%mutate_if(is.numeric,round,digits=2)
      
      cantidad_intervenciones <- intervenciones%>%group_by(spk)%>%summarise(intervenciones = n()) 
      
      speakers <- speakers%>% left_join(cantidad_intervenciones,by="spk")%>%rename(grupos = grupos_entonativos_spk, palabras = palabras_cantidad_spk)
      
      if(exists("spkmetadatos")){
        speakers <- speakers %>%left_join(spkmetadatos%>%select(spk,filename,sexo,edad,nivel), by="spk")%>%mutate(sexo=ifelse(is.na(sexo),"desconocido",sexo),edad=ifelse(is.na(edad),"desconocido",edad),nivel=ifelse(is.na(nivel),"desconocido",nivel))%>%distinct(spk,.keep_all = TRUE)}else {speakers <- speakers %>%mutate(sexo=NA,edad=NA,nivel=NA)}
      ## Grupos entonativos ampliados con diferencias respecto al hablante
      
      grupos_ampliados2 <- grupos_ampliados %>% left_join(speakers,by="spk")%>%mutate(
        
        dif_pitch = 12*log2(pitch_Hz/pitch_Hz_spk),
        dif_range = range_st-range_mean_spk,
        dif_inten = intensity-intensity_mean_spk,
        dif_inflexion = inflexion_st-inflexion_mean_spk,
        dif_dur = dur - dur_mean_spk,
        dif_velocidad = velocidad - velocidad_spk,
        dif_pitch_sign = (pitch_Hz-pitch_Hz_spk)/pitch_SD_spk,
        dif_range_sign = (range_st-range_mean_spk)/range_SD_spk,
        dif_inten_sign = (intensity-intensity_mean_spk)/intensity_SD_spk,
        dif_inflexion_sign = (inflexion_st-inflexion_mean_spk)/inflexion_SD_spk,
        dif_dur_sign = (dur - dur_mean_spk)/dur_SD_spk,
        dif_velocidad_sign = (velocidad - velocidad_spk)/velocidad_SD_spk)%>%group_by(id_turn)%>%mutate(
          posicion_en_intervencion = row_number())%>%ungroup() %>%mutate_if(is.numeric,round,digits=2)
      grupos_ampliados2 <- grupos_ampliados2%>% select(-palabras_mean_spk,-inflexion_mean_spk,-pitch_Hz_spk,-range_mean_spk, -intensity_mean_spk,-dur_mean_spk,-velocidad_spk,-filename.y,-tmin,-tmax,-palabras.x,-palabras.y)%>%rename(filename=filename.x)
      
      ### Añadir posiciones en grupo e intervención y distancias
      
      tokenized_tagged <- tokenized_tagged %>% left_join(grupos_ampliados2%>%select(id_ip,posicion_en_intervencion),by=c("id"="id_ip"))
      tokenized_tagged <- tokenized_tagged %>%ungroup()%>%arrange(filename,time_start)%>% mutate(posicion_grupo_tag = ifelse(posicion_grupo==1&lead(posicion_grupo==2,1),"primera",NA),posicion_grupo_tag = ifelse(posicion_grupo!=1 & lead(posicion_grupo==1,1),"última",posicion_grupo_tag),posicion_grupo_tag = ifelse(posicion_grupo==1&lead(posicion_grupo==1,1),"única",posicion_grupo_tag),posicion_grupo_tag = ifelse(is.na(posicion_grupo_tag),"intermedia",posicion_grupo_tag))
      tokenized_tagged <- tokenized_tagged%>%mutate(
        posicion_intervencion = ifelse(posicion_grupo_tag=="primera"&posicion_en_intervencion==1,"primera",NA),
        posicion_intervencion = ifelse(posicion_grupo_tag=="última"&lead(posicion_en_intervencion==1,1),"última",posicion_intervencion),
        posicion_intervencion = ifelse(posicion_grupo_tag=="única"&posicion_en_intervencion==1&lead(posicion_en_intervencion==1,1),"única",posicion_intervencion),
        posicion_intervencion = ifelse(is.na(posicion_intervencion),"intermedia",posicion_intervencion) )
      tokenized_tagged <- tokenized_tagged%>%group_by(id)%>%mutate(
        
        token_minus_one = lag(token,1),
        token_minus_two = lag(token,2),
        token_plus_one = lead(token,1),
        token_plus_two = lead(token,2),
        upos_minus_one = lag(upos,1),
        upos_minus_two = lag(upos,2),
        upos_plus_one = lead(upos,1),
        upos_plus_two = lead(upos,2),
        
      )%>%ungroup()
      tokenized_tagged <- tokenized_tagged%>%mutate(id_pal=NA)
      
      }
      
      else {
        
        incProgress(0.9, detail = "Processing words data")
        
        words <- df %>% filter(grepl("word", tier_name)) %>% 
          mutate(id_word = paste(spk, "word", time_start, time_end, sep ="_"),
                 time_start_word = time_start,
                 time_end_word = time_end,
                 word = content)
        setDT(words)
        
        if(input$language == "spanish") {
          words[, vowels_structure := gsub("[^áéíóúaeiouÁÉÍÓÚAEIOUÜü]", "", word)]
          words[, accent := ifelse(grepl("[aeiou][^aeiouáéíóúns]$", word) & !grepl("[áéíóú]", vowels_structure), "oxitone", NA)]
          words[, accent := ifelse(grepl("[áéíóú][ns]$", word), "oxitone", accent)]
          words[, accent := ifelse(grepl("[áéíóú]$", word), "oxitone", accent)]
          words[, accent := ifelse(grepl("[aeons]$", word) & !grepl("[áéíóú]", vowels_structure), "paroxitone", accent)]
          words[, accent := ifelse(grepl("[áéíóú][aeiou][aeiou]", vowels_structure), "proparoxitone", accent)]
          words[, accent := ifelse(vowels_structure %in% c("a", "e", "u", "o", "i"), "oxitone", accent)]
          words[, accent := ifelse(vowels_structure == "í" | vowels_structure == "ú", "oxitone", accent)]
          words[, accent := ifelse(grepl("ía$|íe$|ío$|úa$|úe$|úo$", vowels_structure), "paroxitone", accent)]
          words[, accent := ifelse(word %in% c("tu", "pero", "desde", "nuestro", "vuestro", "aun", "medio", "tan", "mi", "su", "me", "te", "se", "el", "la", "los", "las", "que", "a", "ante", "con", "de", "pues", "por", "para", "desde", "en", "al", "san", "porque", "y", "o", "u", "donde", "cuando", "como", "entre", "del"), "non_tonic", accent)]
        } 
        
        else if(input$language == "catalan") {
          words[, vowels_structure := gsub("[^àáèéíòóúaeiouÀÁÈÉÍÒÓÚAEIOUÜü]", "", word)]
          words[, accent := ifelse(grepl("(à|é|è|í|ó|ò|ú|én|èn|ín|às|és|ès|ís|ós|òs|ús)$", word), "oxitone", NA)]
          words[, accent := ifelse(grepl("(a|e|i|o|u|en|in|as|es|is|os|us|ia)$", word) & !grepl("[àéèíòóú]", vowels_structure), "paroxitone", accent)]
          words[, accent := ifelse(grepl("(à|é|è|í|ó|ò|í|ú)[aeiou]$", vowels_structure), "paroxitone", accent)]
          words[, accent := ifelse(grepl("(à|é|è|í|ó|ò|í|ú)[aeiou][aeiou]$", vowels_structure), "proparoxitone", accent)]
          words[, accent := ifelse(vowels_structure %in% c("a", "e", "i", "o", "u", "à", "è", "ò", "é", "í", "ó", "ú"), "oxitone", accent)]
          words[, accent := ifelse(word %in% c("A", "a", "El", "el", "la", "La", "Els", "els", "com", "per", "amb", "Amb", "em", "al", "quan", "doncs", "Per", "del", "Doncs", "ets", "se'm", "tan", "Em", "LA", "dels", "I", "i"), "non_tonic", accent)]
          words[, accent := ifelse(is.na(accent), "oxitone", accent)]
        }
        
        setDF(words)
        words <- words %>% select(-word)
        
        setDT(words)
        
        words <- words %>% arrange(filename, time_start) %>% 
          mutate(pause = lead(time_start) - time_end,
                 changeip = ifelse(pause > 0.15 | lead(filename) != filename, "change", NA),
                 changeturn = ifelse(lead(spk) != spk | lead(filename) != filename, "change", NA)) %>% 
          group_by(filename, spk, changeip) %>% 
          mutate(id_ip = row_number(),
                 id_ip = ifelse(changeip != "change", NA, paste(spk, time_start, time_end, id_ip, sep = "_"))) %>% 
          ungroup() %>% fill(id_ip, .direction = "up") %>% 
          group_by(id_ip) %>% 
          mutate(token_id = row_number(), token_id = as.character(token_id)) %>% 
          ungroup() %>% arrange(filename, time_start) %>% 
          group_by(filename, id_ip) %>% 
          mutate(ip = paste(content, collapse = " ")) %>% 
          ungroup() %>% group_by(filename, id_ip) %>% 
          mutate(id_ip = paste(spk, first(time_start), last(time_end), sep = "_"),
                 time_start_ip = first(time_start),
                 time_end_ip = last(time_end)) %>% 
          ungroup()
        
        words <- words %>% group_by(filename, spk, changeturn) %>%
          mutate(id_turn = row_number(),
                 id_turn = ifelse(changeturn != "change", NA, paste(spk, time_start, time_end, id_turn, sep = "_"))) %>%
          ungroup() %>% fill(id_turn, .direction = "up") %>% 
          group_by(id_turn) %>% 
          ungroup() %>% arrange(filename, time_start) %>% 
          group_by(filename, id_turn) %>% 
          mutate(pause_cod = ifelse(changeip == "change", "/ ", ""),
                 pause_cod = ifelse(is.na(pause_cod), "", pause_cod),
                 turn = paste(paste(content, pause_cod, sep = ""), collapse = " ")) %>% 
          ungroup() %>% group_by(filename, id_turn) %>% 
          mutate(id_turn = paste(spk, first(time_start), last(time_end), sep = "_")) %>% 
          ungroup() %>% arrange(filename, time_start)
        
        # Render the table in the main panel
        incProgress(4, detail = "Part of speech tagging")
        
        model_path <- udpipe_download_model(language = input$language)
        
        spmodel <- udpipe_load_model(model_path)
        
        ips <- words%>%group_by(id_ip,ip)%>%summarise(ip=first(ip))%>%ungroup()
        
        tokenized_udpipe <- udpipe_annotate(x=ips$ip,object = spmodel, tokenizer = "horizontal",tagger = "default", doc_id=ips$id_ip)%>%as.data.frame()%>%rename(id_ip=doc_id)
        
        setDT(tokenized_udpipe)
        setDT(words)
        
        words <- words[tokenized_udpipe[,c("id_ip","upos","lemma","token_id")], on = c('id_ip',"token_id"), nomatch = 0]
        
        words<- words%>%mutate(accent=ifelse(upos%in%c("ADP","CCONJ"),"non_tonic",accent),accent=ifelse(content=="según","oxitone",accent),accent=ifelse(content%in%c("lo","los","si","le","les","mis","mi"),"non_tonic",accent),first_word=ifelse(token_id==1,"yes","no"))%>%group_by(id_ip)%>%mutate(toneme_word = ifelse(id_word==last(id_word)&accent!="non_tonic","yes","no"))%>%ungroup()
        
        words_non_tonic_first <- words%>%filter(accent=="non_tonic") %>%group_by(id_ip)%>%mutate(first_non_tonic_ip=ifelse(id_word ==first(id_word)&token_id==1,"yes","no"))%>%ungroup()%>%select(id_word,first_non_tonic_ip)
        
        words_tonic_first <- words%>%filter(accent!="non_tonic") %>%group_by(id_ip)%>%mutate(first_tonic_ip=ifelse(id_word ==first(id_word)&token_id==1,"yes","no"))%>%ungroup()%>%select(id_word,first_tonic_ip)
        
        words_tonic_non_first <- words%>%filter(accent!="non_tonic") %>%group_by(id_ip)%>%mutate(tonic_ip_no_first=ifelse(id_word ==first(id_word)&token_id>1,"yes","no"))%>%ungroup()%>%select(id_word,tonic_ip_no_first)
        
        words <- words%>%left_join(words_non_tonic_first,by="id_word")
        words <- words%>%left_join(words_tonic_first,by="id_word")
        words <- words%>%left_join(words_tonic_non_first,by="id_word")
        
        incProgress(1, detail = "Adding sentiments")
        
        if (!is.null(input$sentimentFile)) {
          sentiment <- read.csv(input$sentimentFile$datapath, header = TRUE, sep = ",")
          sentiment <- sentiment %>% distinct(content, .keep_all = TRUE)
          setDT(sentiment)
          
          words <- merge(words, sentiment, by = "content", all.x = TRUE)
        } else {
          words$sentiment <- NA
        }
        
        words <- words %>% arrange(filename, time_start)
        
        incProgress(1, detail = "Applying proximities")
        
        setDF(words)
        words <- words%>%group_by(id_ip)%>%mutate(word=content,
                                                  wordleft2 = lag(word,2),
                                                  wordleft = lag(word,1),
                                                  wordright = lead(word,1),
                                                  wordright2 = lead(word,2),
                                                  uposleft2 = lag(upos,2),
                                                  uposleft = lag(upos,1),
                                                  uposright = lead(upos,1),
                                                  uposright2 = lead(upos,2)
                                                  
        )%>%ungroup()
        
        incProgress(1, detail = "Creating vowels")
        
        phones <- df %>% filter(grepl("phone", tier_name)) %>% mutate(id_phone = paste(spk, "phone", time_start*1000, time_end*1000, sep ="_"),category = ifelse(grepl("[aeiou@EO]", content),"vowel", ifelse(grepl("[jw]", content), "glide", "consonant")),category=ifelse(grepl("[nlñ]",content),"vowel",category),time_start = time_start + 0.001, time_end = time_end - 0.001)
        
        incProgress(1, detail = "Merging words and phones")
        
        setDT(words) # make a data.table
        setDT(phones) # make a data.table
        
        
        setkey(words, filename,spk,time_start, time_end)
        setkey(phones, filename,spk,time_start, time_end)
        
        phones_1 <- foverlaps(words, phones, nomatch = NA)%>%select(-i.time_start,-i.time_end,-i.tier_name)
        
        vowels <- phones_1 %>% filter(category=="vowel",grepl("[aeiou@EOIAU]",content)|(grepl("[nlñ]",content) & lead(id_word)!=id_word) )%>%arrange(filename,time_start)
        
        phones <- phones_1
        
        incProgress(1, detail = "Calculating prosodic values for vowels")
        
        setDT(vowels) 
        setDT(prosody) 
        
        prosody <- prosody[, dummy := time]
        prosody <- prosody[order(filename,time)]  
        
        setkey(vowels, filename, time_start, time_end)  
        setkey(prosody, filename, time, dummy) 
        
        summary <- foverlaps(vowels, prosody, nomatch = NA)[, dummy := NULL]
        summary <- summary[, dur := time_end - time_start]
        summary <- summary[,quarter := ifelse(time <= (time_start + (0.25 * dur)), "q1", NA)]
        summary <- summary[,quarter := ifelse(time >= (time_start + (0.25 * dur)) & time <= (time_start + (0.5 * dur)),"q2",quarter)]
        summary <- summary[,quarter := ifelse(time >= (time_start + (0.5 * dur)) &
                                                time <= (time_start + (0.75 * dur)),"q3",quarter)]
        summary <- summary[,quarter := ifelse(time >= (time_start + (0.75 * dur)) &
                                                time <= (time_start + (dur)), "q4",quarter)]
        q1alof <- summary[quarter == "q1", .(q1piHz = mean(pitch, na.rm = TRUE),q1piHz_time = mean(time, na.rm = TRUE)),by=(id_phone)]
        q2alof <- summary[quarter == "q2", .(q2piHz = mean(pitch, na.rm = TRUE),q2piHz_time = mean(time, na.rm = TRUE)),by=(id_phone)]
        q3alof <- summary[quarter == "q3", .(q3piHz = mean(pitch, na.rm = TRUE),q3piHz_time = mean(time, na.rm = TRUE)),by=(id_phone)]
        q4alof <- summary[quarter == "q4", .(q4piHz = mean(pitch, na.rm = TRUE),q4piHz_time = mean(time, na.rm = TRUE)),by=(id_phone)]
        
        data2 = summary[, .(pitch_Hz = mean(pitch, na.rm = TRUE), intensity = mean(intensity, na.rm = TRUE)), by=.(id_phone,id_ip,id_word,filename,spk,content,word,ip,first_tonic_ip,first_non_tonic_ip,tonic_ip_no_first,time_start,time_end,time_start_word,time_end_word,time_start_ip,time_end_ip,toneme_word,category,accent,upos,lemma,sentiment)]%>%mutate_if(is.numeric,round,2)
        
        setDF(data2)
        
        data2 <- data2%>%left_join(q1alof,by="id_phone")
        data2 <- data2%>%left_join(q2alof,by="id_phone")
        data2 <- data2%>%left_join(q3alof,by="id_phone")
        data2 <- data2%>%left_join(q4alof,by="id_phone")
        
        data2 <- data2%>%group_by(id_ip)%>%mutate(pitch_Hz=ifelse(is.na(pitch_Hz),mean(pitch_Hz,na.rm = TRUE),pitch_Hz),q1piHz=ifelse(is.na(q1piHz),mean(q1piHz,na.rm = TRUE),q1piHz),q2piHz=ifelse(is.na(q2piHz),mean(q2piHz,na.rm = TRUE),q2piHz),q3piHz=ifelse(is.na(q3piHz),mean(q3piHz,na.rm = TRUE),q3piHz),q4piHz=ifelse(is.na(q4piHz),mean(q4piHz,na.rm = TRUE),q4piHz),intensity=ifelse(is.na(intensity),mean(intensity,na.rm = TRUE),intensity))%>%ungroup()
        
        incProgress(1, detail = "Adding tonicity")
        
        setDT(data2)
        data2[, first := ifelse(id_phone==first(id_phone),"yes","no"), by=id_word]
        # data2[, first_ip := ifelse(id_phone==first(id_phone),"yes","no"), by=id_ip]
        data2[, last := ifelse(id_phone==last(id_phone),"yes","no"), by=id_word]
        data2[, tonic := ifelse(last=="yes"&accent=="oxitone","yes","no")]
        data2[, tonic := ifelse(accent=="paroxitone"&shift(last,1,type="lead")=="yes","yes",tonic), by=id_word]
        data2[, tonic := ifelse(accent=="proparoxitone"&shift(last,2,type="lead")=="yes","yes",tonic), by=id_word]
        
        data2[, tonic := ifelse(is.na(tonic),"no",tonic)]
        setDF(data2)
        data2 <- data2%>%relocate(tonic, .after = "content")
        setDT(data2)
        
        incProgress(1, detail = "Compute tonal differences")
        
        setDT(data2)
        percent_treshold <- 10
        
        data2[,q1piHz := ifelse(is.na(q1piHz),rowMeans(data2[,c("q2piHz","q3piHz","q4piHz")],na.rm = TRUE),q1piHz)]
        data2[,q2piHz := ifelse(is.na(q2piHz),rowMeans(data2[,c("q1piHz","q3piHz","q4piHz")],na.rm = TRUE),q2piHz)]
        data2[,q3piHz := ifelse(is.na(q3piHz),rowMeans(data2[,c("q2piHz","q1piHz","q4piHz")],na.rm = TRUE),q3piHz)]
        data2[,q4piHz := ifelse(is.na(q4piHz),rowMeans(data2[,c("q2piHz","q3piHz","q1piHz")],na.rm = TRUE),q4piHz)]
        data2[,center_Hz := rowMeans(data2[,c("q2piHz","q3piHz")],na.rm=TRUE)]
        data2[, ':='(
          
          inflexion_percent_Hz_from_prev = ((center_Hz - shift(center_Hz, 1,type="lag"
          )) / shift(center_Hz, 1,type="lag")) * 100,
          inflexion_ST_from_prev =  12 * log2(center_Hz / shift(center_Hz, 1,type="lag")),
          inflexion_percent_Hz_to_next = ((shift(
            center_Hz, 1,type="lead"
          ) - center_Hz) / center_Hz) * 100)]
        
        data2[,':='(
          inflexion_percent_Hz_to_next = ifelse(
            accent == "oxitone",
            ((q4piHz - q1piHz) / q1piHz) * 100,
            inflexion_percent_Hz_to_next
          ),
          inflexion_ST_to_next =  12 * log2(shift(center_Hz, 1,type="lead") /
                                              center_Hz))]
        
        data2[,':='(
          inflexion_ST_to_next = ifelse(
            accent == "oxitone",
            12*log2(q4piHz/q1piHz),
            inflexion_ST_to_next
          ),
          q1piHz_q2piHZ = ((q2piHz - q1piHz) / q2piHz) * 100,
          q2piHz_q3piHZ = ((q3piHz - q2piHz) / q3piHz) * 100,
          q3piHz_q4piHZ = ((q4piHz - q3piHz) / q4piHz) * 100,
          q1piHz_q3piHZ = ((q3piHz - q1piHz) / q3piHz) * 100,
          q1piHz_q4piHZ = ((q4piHz - q1piHz) / q4piHz) * 100,
          q2piHz_q4piHZ = ((q4piHz - q2piHz) / q4piHz) * 100,
          q1piHz_q4piHZ_ST = 12*log2(q4piHz/q1piHz),
          q1piHz_q2piHZ_ST = 12*log2(q2piHz/q1piHz),
          q2piHz_q3piHZ_ST = 12*log2(q3piHz/q2piHz),
          q3piHz_q4piHZ_ST = 12*log2(q4piHz/q3piHz),
          q2piHz_q4piHZ_ST = 12*log2(q4piHz/q2piHz)
        )]
        setDF(data2)
        data2 <- data2%>%mutate(circumflex =  
                                  case_when(
                                    q1piHz_q2piHZ <= -percent_treshold & q2piHz_q3piHZ >= percent_treshold & q3piHz_q4piHZ <= -percent_treshold ~ "desc_asc_desc",
                                    q1piHz_q2piHZ >= percent_treshold & q2piHz_q3piHZ <= -percent_treshold & q3piHz_q4piHZ >= percent_treshold ~ "asc_desc_asc",
                                    q1piHz_q2piHZ >= percent_treshold & between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & q3piHz_q4piHZ <= -percent_treshold ~ "asc_desc",
                                    q1piHz_q2piHZ >= percent_treshold & q2piHz_q3piHZ <= -percent_treshold & q3piHz_q4piHZ <= -percent_treshold ~ "asc_desc",
                                    q1piHz_q2piHZ >= percent_treshold & between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & between(q3piHz_q4piHZ,-percent_treshold,percent_treshold) ~ "asc_q1",
                                    between(q1piHz_q2piHZ,-percent_treshold,percent_treshold)&  q2piHz_q3piHZ >= percent_treshold &between(q3piHz_q4piHZ,-percent_treshold,percent_treshold) ~ "asc_q2",
                                    between(q1piHz_q2piHZ,-percent_treshold,percent_treshold)& between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & q3piHz_q4piHZ >= percent_treshold ~ "asc_q3",
                                    q1piHz_q2piHZ <= -percent_treshold & between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & between(q3piHz_q4piHZ,-percent_treshold,percent_treshold) ~ "desc_q1",
                                    q1piHz_q2piHZ <= -percent_treshold & between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & q3piHz_q4piHZ <= -percent_treshold ~ "desc_q1q3",
                                    between(q1piHz_q2piHZ,-percent_treshold,percent_treshold)&  q2piHz_q3piHZ <= -percent_treshold &between(q3piHz_q4piHZ,-percent_treshold,percent_treshold) ~ "desc_q2",
                                    between(q1piHz_q2piHZ,-percent_treshold,percent_treshold)& between(q2piHz_q3piHZ,-percent_treshold,percent_treshold) & q3piHz_q4piHZ <= -percent_treshold ~ "desc_q3",
                                    between(q1piHz_q2piHZ,-percent_treshold,percent_treshold) & q2piHz_q3piHZ >= percent_treshold & q3piHz_q4piHZ >= percent_treshold ~ "asc_q2q3",
                                    q1piHz_q2piHZ >= percent_treshold & q2piHz_q3piHZ >= percent_treshold & between(q3piHz_q4piHZ,-percent_treshold,percent_treshold) ~ "asc_q1q2",
                                    q1piHz_q2piHZ >= percent_treshold & q2piHz_q3piHZ >= percent_treshold & q3piHz_q4piHZ >= percent_treshold ~"asc_q1q2q3",
                                    q1piHz_q2piHZ <= -percent_treshold & q2piHz_q3piHZ <= -percent_treshold & q3piHz_q4piHZ <= -percent_treshold ~"desc_q1q2q3",
                                    q1piHz_q2piHZ <= -percent_treshold & q2piHz_q3piHZ <= -percent_treshold & between(q3piHz_q4piHZ, -percent_treshold,percent_treshold) ~"desc_q1q2",
                                    between(q1piHz_q2piHZ, -percent_treshold,percent_treshold) & between(q2piHz_q3piHZ, -percent_treshold,percent_treshold)&between(q3piHz_q4piHZ, -percent_treshold,percent_treshold) ~ "no_circumflex",
                                    is.na(center_Hz) ~"non_applicable"))
        setDT(data2)
        data2[,dur := time_end - time_start]
        data2[order(filename,time_start), midpoint := (time_start + time_end)/2]
        data2[order(filename,time_start), distancias := midpoint - shift(midpoint,1,type="lag")]
        data2[order(filename,time_start), pause_prev := time_start - shift(time_end,1,type="lag")]
        setDF(data2)
        data2 <-data2%>%group_by(id_ip)%>%mutate(order_ip=row_number())%>%ungroup()%>%mutate(distancias= as.numeric(distancias),distancias = ifelse(order_ip==1,distancias-pause_prev,distancias))
        setDT(data2)
        data2[order(filename,time_start), ':=' ( 
          dur_percent_from_prev = ((distancias - shift(
            distancias, 1,type="lag"
          )) / shift(distancias, 1,type="lag"))*100,
          intensity_percent_from_prev = ((intensity - shift(
            intensity, 1,type="lag"
          )) / shift(intensity, 1,type="lag")) * 100)]
        setDF(data2)
        data2 <- data2%>%mutate_if(is.numeric, round,2)
        data2 <- data2%>%group_by(id_ip)%>%mutate(maximum_Hz = ifelse(center_Hz == max(center_Hz),"yes","no"),displacement = ifelse(tonic == "yes" & lead(center_Hz) > (center_Hz+ percent_treshold) & lead(tonic)!="yes","yes","no"),displacement=ifelse(is.na(displacement),"no",displacement))%>%ungroup()
        tonics <- data2%>%filter(tonic=="yes")%>%group_by(id_ip)%>%summarise(q_tonic_vowels=n())%>%mutate(multiple_tonic=ifelse(q_tonic_vowels>1,"yes","no"))
        data2 <- data2%>%left_join(tonics, by="id_ip")
        setDT(data2)
        
        incProgress(4, detail = "Getting MAS structure")
        
        setDF(data2)
        data2 <- data2%>%group_by(id_ip)%>%mutate(MAS_structure=
                                                    case_when(.default = "body",
                                                              toneme_word=="yes"~"toneme",
                                                              first_non_tonic_ip=="yes" ~"anacrusis",
                                                              lead(first_tonic_ip)=="yes" ~"body",
                                                              lead(tonic_ip_no_first) =="yes"~"body",
                                                              first_tonic_ip=="yes" ~"body",
                                                              tonic_ip_no_first =="yes"~"body"
                                                              
                                                    ))
        
        incProgress(1, detail = "Computation of MAS")
        
        setDT(data2)
        toneme <- data2[tonic=="yes" & toneme_word=="yes"]
        body <- data2[multiple_tonic=="yes"  & MAS_structure=="body",c("id_phone","time_start","time_end","id_ip","content","upos","word","ip","center_Hz","displacement","circumflex","MAS_structure","intensity","tonic","inflexion_percent_Hz_from_prev")]
        
        body2 <- body%>%group_by(id_ip,ip)%>%mutate(body=cumsum(inflexion_percent_Hz_from_prev))%>%summarise(body=last(body)/n())
        
        body_saw <- body%>%group_by(id_ip,ip,circumflex,tonic)%>%summarise(freq=n())%>%group_by(id_ip,ip)%>%summarise(freq_total=sum(freq[tonic=="yes"]),freq_tonic_circumflex=sum(freq[circumflex!="no_circumflex"&tonic=="yes"]))%>%ungroup()%>%mutate(body_structure = ifelse(freq_tonic_circumflex/freq_total>0.5,"yes","no"))
        
        anacrusis_non_tonic <- data2[multiple_tonic=="yes" & tonic=="no" & first_non_tonic_ip=="yes",c("id_phone","id_ip","content","upos","word","ip","center_Hz","displacement")]
        
        first_tonic <- data2[multiple_tonic=="yes" & tonic=="yes" & first_tonic_ip=="yes",c("id_phone","id_ip","content","accent","upos","word","ip","center_Hz","displacement","inflexion_percent_Hz_from_prev")]
        
        anacrusis_tonic <- data2[multiple_tonic=="yes" & tonic=="yes" & tonic_ip_no_first =="yes",c("id_phone","id_ip","accent","content","upos","word","ip","center_Hz","displacement","inflexion_percent_Hz_from_prev")]
        
        MAS <- data2[tonic=="yes"&toneme_word=="yes",c("id_phone","id_ip","ip","content","word","accent","center_Hz","q1piHz_q4piHZ","circumflex","displacement","inflexion_percent_Hz_from_prev","inflexion_percent_Hz_to_next","multiple_tonic","intensity","time_start","time_end","id_word")]
        
        setDF(MAS)
        MAS <- MAS%>%left_join(anacrusis_non_tonic%>%select(center_Hz,displacement,word,id_ip)%>%rename(anacrusis_center_Hz = center_Hz,anacrusis_non_tonic_word=word,anacrusis_displacement = displacement),by="id_ip")
        
        MAS <- MAS%>%left_join(body_saw,by="id_ip")
        
        MAS <- MAS%>%left_join(anacrusis_tonic%>%select(center_Hz,displacement,word,id_ip,inflexion_percent_Hz_from_prev)%>%rename(anacrusis_tonic_center_Hz = center_Hz,anacrusis_tonic_inflexion_percent_Hz_from_prev = inflexion_percent_Hz_from_prev,anacrusis_tonic_displacement = displacement,anacrusis_tonic_word=word),by="id_ip")
        MAS <- MAS%>%left_join(first_tonic%>%select(center_Hz,displacement,word,id_ip,inflexion_percent_Hz_from_prev)%>%rename(
          first_tonic_inflexion_percent_Hz_from_prev = inflexion_percent_Hz_from_prev,
          first_tonic_center_Hz = center_Hz,first_tonic_displacement = displacement,first_tonic_word=word),by="id_ip")
        
        MAS <- MAS%>%mutate(anacrusis= anacrusis_tonic_inflexion_percent_Hz_from_prev,anacrusis= ifelse(is.na(anacrusis), first_tonic_inflexion_percent_Hz_from_prev,anacrusis),body = ((center_Hz - first_tonic_center_Hz)/first_tonic_center_Hz)*100, body=ifelse(is.na(body), ((center_Hz - anacrusis_tonic_center_Hz)/anacrusis_tonic_center_Hz)*100,body),toneme=ifelse(accent=="oxitone"& !grepl("[nl]$",word),q1piHz_q4piHZ,inflexion_percent_Hz_to_next))%>%mutate_if(is.numeric,round,2)%>%mutate(relocated = ifelse((center_Hz < first_tonic_center_Hz +10 & center_Hz > first_tonic_center_Hz -10) |(center_Hz < anacrusis_tonic_center_Hz +10 & center_Hz > anacrusis_tonic_center_Hz -10),"yes","no"),anacrusis_displacement= first_tonic_displacement,anacrusis_displacement=ifelse(is.na(anacrusis_displacement),anacrusis_tonic_displacement,anacrusis_displacement),anacrusis_displacement=ifelse(is.na(anacrusis_displacement),"no",anacrusis_displacement))
        
        
        MAS <- MAS %>%mutate(body_2=body)%>%select(-body)%>%left_join(body2,by="id_ip")%>%mutate_if(is.numeric,round,2)
        
        MAS <- MAS%>%distinct(id_phone, .keep_all = TRUE)
        
        incProgress(1, detail = "Calculating standard values")
        
        data2 <- data2 %>%
          group_by(id_ip) %>%
          arrange(filename, time_start) %>%
          mutate(
            standard_q1_pitch = cumprod(c(100, (1 + ((((q1piHz - shift(q1piHz, 1,type="lag")) / shift(q1piHz, 1,type="lag")) * 100) / 100))[-1])),    
            standard_q2_pitch = cumprod(c(100, (1 + ((((q2piHz - shift(q2piHz, 1,type="lag")) / shift(q2piHz, 1,type="lag")) * 100) / 100))[-1])),
            standard_q3_pitch = cumprod(c(100, (1 + ((((q3piHz - shift(q3piHz, 1,type="lag")) / shift(q3piHz, 1,type="lag")) * 100) / 100))[-1])),
            standard_q4_pitch = cumprod(c(100, (1 + ((((q4piHz - shift(q4piHz, 1,type="lag")) / shift(q4piHz, 1,type="lag")) * 100) / 100))[-1])),
            standard_pitch = cumprod(c(100, (1 + (inflexion_percent_Hz_from_prev / 100))[-1])),
            standard_dur = cumprod(c(100, (1 + (dur_percent_from_prev / 100))[-1])),
            standard_intensity = cumprod(c(100, (1 + (intensity_percent_from_prev / 100))[-1]))
          ) %>%mutate_if(is.numeric,round,2)%>%
          ungroup()%>%relocate(standard_pitch,.after=center_Hz)
        
        incProgress(1, detail = "Assigning patterns")
        
        setDF(MAS)
        
        # First round
        
        MAS <- MAS %>%
          mutate(
            pattern = case_when(
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & between(toneme, -40, 15) ~ "PI",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & toneme > 70 ~ "PII",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis)) & (between(body_2,-80,10)) & between(toneme, 40, 60) ~ "PIII",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & circumflex %in% c("asc_desc", "desc_q3","desc_q2")  ~ "PIVa",
              (is.na(anacrusis) | between(anacrusis,-10,10)) & between(body_2, -3, 10) & circumflex %in% c("asc_desc","desc_q3","desc_q2")  ~ "PIVb",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis))  & (between(body_2,-80,10)|is.na(body)) & (between(toneme, -9, 9) | (between(inflexion_percent_Hz_from_prev,-5,10) & between(toneme,-5,5))) ~ "PV",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "no")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & between(toneme, 15, 70) ~ "PVIa",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & between(toneme, 15, 60) ~ "PVIb",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis)) & relocated == "yes" & toneme < -10 ~ "PVII",
              is.na(anacrusis) & (between(body_2,-80,10)|is.na(body)) & multiple_tonic=="no" & !is.na(toneme) & toneme !="NaN"  ~ "PVIII",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis)) & (between(body_2,-80,10)|is.na(body)) & toneme < -40  ~ "PIX",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis))& between(toneme, -10, 60) & relocated == "yes" & circumflex %in% c("asc_desc","desc_q3","desc_q2") ~ "PXa",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis))  & relocated == "yes" & circumflex %in% c("desc_asc","asc_q3","asc_q2","asc_desc_asc") ~ "PXb",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis)) & between(body_2, -9, 10) & toneme < -10 ~ "PXI",
              ((between(anacrusis,-10,40) & anacrusis_displacement == "yes")| is.na(anacrusis)) & between(body_2, -3, 10) & toneme < -10 & body_structure == "no" ~ "PXIIa",
              is.na(anacrusis) & between(body_2, -3, 10) & toneme < -10 & body_structure == "yes" ~ "PXIIb",
              is.na(anacrusis) & (between(body,-80,10)|is.na(body)) & toneme < -10 & body_structure == "yes" ~ "PXIIc",
              between(body_2, 15, 140) & toneme > 40 ~ "PXIII",
              is.na(toneme) | toneme=="NaN" ~ "undefined_toneme"
            )
            
          )
        
        
        MAS <- MAS %>% mutate( MAS_phonological= ifelse(pattern=="PI","assertion",NA))
        MAS <- MAS %>% mutate( MAS_phonological = ifelse(pattern %in% c("PII","PIII","PIVa","PIVb"), "question",MAS_phonological))
        MAS <- MAS %>% mutate( MAS_phonological = ifelse( pattern %in% c("PV","PVIa","VIb"), "continuous",MAS_phonological))
        MAS <- MAS %>% mutate( MAS_phonological = ifelse(is.na(MAS_phonological),"emphatic",MAS_phonological ))
        
        rm(anacrusis_non_tonic,anacrusis_tonic,body,body_saw,body2,first_tonic,toneme,tonics,percent_treshold)
        
        MAS <- MAS%>%group_by(pattern)%>%mutate(order_pattern_group = row_number())%>%ungroup()
        
        data2<- data2%>%left_join(MAS%>%select(id_ip,pattern,order_pattern_group), by="id_ip")
        
        incProgress(1, detail = "TOBI computation")
        
        percent_treshold_ST <- 1.2
        
        TOBI <- data2%>%filter(tonic=="yes")%>%mutate(center_ST = 12*log2(center_Hz/1),
                                                      circumflex_st =  
                                                        case_when(
                                                          q1piHz_q2piHZ_ST <= -percent_treshold_ST & q2piHz_q3piHZ_ST >= percent_treshold_ST & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~ "LHL",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & q2piHz_q3piHZ_ST <= -percent_treshold_ST & q3piHz_q4piHZ_ST >= percent_treshold_ST ~ "HLH",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~ "HL",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & q2piHz_q3piHZ_ST <= -percent_treshold_ST & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~ "HL",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & between(q3piHz_q4piHZ_ST,-percent_treshold_ST,percent_treshold_ST) ~ "H",
                                                          between(q1piHz_q2piHZ_ST,-percent_treshold_ST,percent_treshold_ST)&  q2piHz_q3piHZ_ST >= percent_treshold_ST &between(q3piHz_q4piHZ_ST,-percent_treshold_ST,percent_treshold_ST) ~ "H",
                                                          between(q1piHz_q2piHZ_ST,-percent_treshold_ST,percent_treshold_ST)& between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & q3piHz_q4piHZ_ST >= percent_treshold_ST ~ "H",
                                                          q1piHz_q2piHZ_ST <= -percent_treshold_ST & between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & between(q3piHz_q4piHZ_ST,-percent_treshold_ST,percent_treshold_ST) ~ "L",
                                                          q1piHz_q2piHZ_ST <= -percent_treshold_ST & between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~ "L>",
                                                          between(q1piHz_q2piHZ_ST,-percent_treshold_ST,percent_treshold_ST)&  q2piHz_q3piHZ_ST <= -percent_treshold_ST &between(q3piHz_q4piHZ_ST,-percent_treshold_ST,percent_treshold_ST) ~ "L",
                                                          between(q1piHz_q2piHZ_ST,-percent_treshold_ST,percent_treshold_ST)& between(q2piHz_q3piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~ "L",
                                                          between(q1piHz_q2piHZ_ST,-percent_treshold_ST,percent_treshold_ST) & q2piHz_q3piHZ_ST >= percent_treshold_ST & q3piHz_q4piHZ_ST >= percent_treshold_ST ~ "H>",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & q2piHz_q3piHZ_ST >= percent_treshold_ST & between(q3piHz_q4piHZ_ST,-percent_treshold_ST,percent_treshold_ST) ~ "H>",
                                                          q1piHz_q2piHZ_ST >= percent_treshold_ST & q2piHz_q3piHZ_ST >= percent_treshold_ST & q3piHz_q4piHZ_ST >= percent_treshold_ST ~"H>",
                                                          q1piHz_q2piHZ_ST <= -percent_treshold_ST & q2piHz_q3piHZ_ST <= -percent_treshold_ST & q3piHz_q4piHZ_ST <= -percent_treshold_ST ~"L>",
                                                          q1piHz_q2piHZ_ST <= -percent_treshold_ST & q2piHz_q3piHZ_ST <= -percent_treshold_ST & between(q3piHz_q4piHZ_ST, -percent_treshold_ST,percent_treshold_ST) ~"L>",
                                                          between(q1piHz_q2piHZ_ST, -percent_treshold_ST,percent_treshold_ST) & between(q2piHz_q3piHZ_ST, -percent_treshold_ST,percent_treshold_ST)&between(q3piHz_q4piHZ_ST, -percent_treshold_ST,percent_treshold_ST) ~ "no_circumflex",
                                                          is.na(center_Hz) ~"non_applicable"))
        
        TOBI <- TOBI%>%mutate(
          
          TOBI_pattern=  
            case_when(.default="unchanged",
                      
                      inflexion_ST_from_prev < -percent_treshold_ST & between(inflexion_ST_to_next,-percent_treshold_ST,percent_treshold_ST)  ~ "H+L*",
                      inflexion_ST_from_prev > percent_treshold_ST & between(inflexion_ST_to_next,-percent_treshold_ST,percent_treshold_ST)  ~ "L+H*",
                      inflexion_ST_to_next < -percent_treshold_ST & between(inflexion_ST_from_prev,-percent_treshold_ST,percent_treshold_ST)  ~ "H*+L",
                      inflexion_ST_to_next > percent_treshold_ST & between(inflexion_ST_from_prev,-percent_treshold_ST,percent_treshold_ST)  ~ "L*+H",
                      inflexion_ST_from_prev < -percent_treshold_ST & inflexion_ST_to_next > percent_treshold_ST &MAS_structure=="toneme"  ~ "H+L*H%",
                      inflexion_ST_from_prev > percent_treshold_ST & inflexion_ST_to_next < -percent_treshold_ST&MAS_structure=="toneme"  ~ "L+H*L%",
                      inflexion_ST_from_prev > percent_treshold_ST & inflexion_ST_to_next > -percent_treshold_ST &MAS_structure=="toneme" ~ "L+H*H%",
                      inflexion_ST_from_prev < -percent_treshold_ST & inflexion_ST_to_next < -percent_treshold_ST &MAS_structure=="toneme" ~ "H+L*L%",
                      inflexion_ST_from_prev < -percent_treshold_ST & inflexion_ST_to_next > percent_treshold_ST &MAS_structure!="toneme"  ~ "H+L*H-",
                      inflexion_ST_from_prev > percent_treshold_ST & inflexion_ST_to_next < -percent_treshold_ST&MAS_structure!="toneme"  ~ "L+H*L-",
                      inflexion_ST_from_prev > percent_treshold_ST & inflexion_ST_to_next > -percent_treshold_ST &MAS_structure!="toneme" ~ "L+H*H-",
                      inflexion_ST_from_prev < -percent_treshold_ST & inflexion_ST_to_next < -percent_treshold_ST &MAS_structure!="toneme" ~ "H+L*L-"
            ),TOBI_pattern= ifelse(is.na(TOBI_pattern)& !is.na(circumflex_st)&circumflex_st!="no_circumflex", circumflex_st, TOBI_pattern))
        
        data2<- data2%>%left_join(TOBI%>%select(id_word,TOBI_pattern),by="id_word")
        
        incProgress(1, detail = "Calcultating intonational phrases prosody")
        
        ips <- setDF(words) %>%arrange(filename,time_start)%>% mutate(pause = lead(time_start) - time_end,changeip = ifelse(pause > 0.15 | lead(filename)!=filename, "change", NA)) %>% group_by(filename, spk, changeip) 
        # %>%
        #           mutate(id_ip = row_number(),
        #                  id_ip = ifelse(
        #                    changeip != "change",
        #                    NA,
        #                    paste(spk, time_start, time_end, id_ip, sep = "_")
        #                  )) %>%ungroup()
        #         ips <- ips%>% fill(id_ip, .direction = "up")
        ips <- ips %>% group_by(id_ip) %>%
          mutate(
            time_start_ip = first(time_start_word),
            time_end_ip = last(time_end_word), dur= time_end_ip  - time_start_ip,
            s_rate = n_distinct(id_word)/dur
          ) %>% ungroup() %>%arrange(filename,time_start_ip)
        
        ips <- ips%>% group_by(filename,spk, id_word) %>% summarise(tier_name=first(tier_name),time_start=first(time_start_ip),time_end=last(time_end_ip),id_ip = first(id_ip), word =first(content)) %>% ungroup()
        ips <- ips%>%arrange(filename,time_start) %>% group_by(filename, id_ip) %>% mutate(content = paste(word, collapse = " ")) %>%ungroup()
        ips <- ips %>% group_by(filename, id_ip) %>% summarise(time_start=first(time_start),time_end=first(time_end),content=first(content),spk=first(spk),tier_name=first(tier_name))%>%ungroup()%>%mutate(time_start=as.numeric(time_start))%>%arrange(filename,time_start)
        ips <- ips%>%mutate(time_start= ifelse(time_start>time_end,time_end,time_start))
        
        incProgress(1, detail = "Adding part of speech")
        
        ips <- setDF(words)
        
        # %>%arrange(filename,time_start)%>% mutate(pause = lead(time_start) - time_end,changeip = ifelse(pause > 0.15 | lead(filename)!=filename, "change", NA)) 
        # %>% group_by(filename, spk, changeip) 
        # %>%
        #           mutate(id_ip = row_number(),
        #                  id_ip = ifelse(
        #                    changeip != "change",
        #                    NA,
        #                    paste(spk, time_start, time_end, id_ip, sep = "_")
        #                  )) %>%ungroup()
        #         ips <- ips%>% fill(id_ip, .direction = "up")
        ips <- ips %>% group_by(id_ip) %>%
          mutate(
            time_start_ip = first(time_start_word),
            time_end_ip = last(time_end_word)
          ) %>% ungroup() %>%arrange(filename,time_start_ip)
        ips <- ips%>% group_by(filename,spk, id_word) %>% summarise(tier_name=first(tier_name),time_start=first(time_start_ip),time_end=last(time_end_ip),id_ip = first(id_ip), word =first(content),upos=first(upos)) %>% ungroup()
        ips <- ips%>%arrange(filename,time_start) %>% group_by(filename, id_ip) %>% mutate(content = paste(word, collapse = " ")) %>%ungroup()
        ips <- ips %>% group_by(filename, id_ip) %>% summarise(time_start=first(time_start),time_end=first(time_end),content=first(content),spk=first(spk),tier_name=first(tier_name),qnoun = sum(upos=="NOUN",na.rm = TRUE),qpropn=sum(sentiment=="propn",na.rm = TRUE),qnum=sum(sentiment=="num",na.rm = TRUE),qverb = sum(upos=="VERB",na.rm = TRUE),qadj = sum(upos=="ADJ",na.rm = TRUE),qsconj = sum(upos=="SCONJ",na.rm = TRUE),qcconj = sum(upos=="CCONJ",na.rm = TRUE),qadp = sum(upos=="ADP",na.rm = TRUE),qadv = sum(upos=="ADV",na.rm = TRUE),qintj = sum(upos=="INTJ",na.rm = TRUE),qaux = sum(upos=="AUX",na.rm = TRUE),qdet = sum(upos=="DET,na.rm = TRUE"),qpron = sum(upos=="PRON",na.rm = TRUE))%>%ungroup()%>%mutate(time_start=as.numeric(time_start))%>%arrange(filename,time_start)
        ips <- ips%>%mutate(time_start= ifelse(time_start>time_end,time_end,time_start))
        
        incProgress(1, detail = "Adding sentiments")
        
        ips_sentiment <- setDF(words)
        
        # %>%arrange(filename,time_start)%>% mutate(pause = lead(time_start) - time_end,changeip = ifelse(pause > 0.15 | lead(filename)!=filename, "change", NA)) %>% group_by(filename, spk, changeip) 
        # %>%
        #           mutate(id_ip = row_number(),
        #                  id_ip = ifelse(
        #                    changeip != "change",
        #                    NA,
        #                    paste(spk, time_start, time_end, id_ip, sep = "_")
        #                  )) %>%ungroup()
        #         ips_sentiment <- ips_sentiment%>% fill(id_ip, .direction = "up")
        ips_sentiment <- ips_sentiment %>% group_by(id_ip) %>%
          mutate(
            time_start_ip = first(time_start_word),
            time_end_ip = last(time_end_word)
          ) %>% ungroup() %>%arrange(filename,time_start_ip)
        ips_sentiment <- ips_sentiment%>% group_by(filename,spk, id_word) %>% summarise(tier_name=first(tier_name),time_start=first(time_start_ip),time_end=last(time_end_ip),id_ip = first(id_ip), word =first(content),sentiment=first(sentiment)) %>% ungroup()
        ips_sentiment <- ips_sentiment%>%arrange(filename,time_start) %>% group_by(filename, id_ip) %>% mutate(content = paste(word, collapse = " ")) %>%ungroup()
        
        ips_sentiment <- ips_sentiment %>% group_by(filename, id_ip) %>% summarise(time_start=first(time_start),time_end=first(time_end),content=first(content),spk=first(spk),tier_name=first(tier_name),qpalabras=n_distinct(id_word),qfear = sum(sentiment=="miedo",na.rm = TRUE),qpos=sum(sentiment=="positivo",na.rm = TRUE),qneg=sum(sentiment=="negativo",na.rm = TRUE),qsurprise = sum(sentiment=="sorpresa",na.rm = TRUE),qdisgust = sum(sentiment=="asco",na.rm = TRUE),qjoy = sum(sentiment=="alegría",na.rm = TRUE),qanticip = sum(sentiment=="anticipación",na.rm = TRUE))%>%ungroup()%>%mutate(time_start=as.numeric(time_start))%>%arrange(filename,time_start)
        ips_sentiment <- ips_sentiment%>%mutate(time_start= ifelse(time_start>time_end,time_end,time_start))
        
        incProgress(1, detail = "Completing ips")
        
        ips_MAS <- setDF(MAS) %>%group_by(id_ip)%>%summarise(
          
          toneme = mean(toneme,na.rm=TRUE),
          anacrusis = mean(anacrusis, na.rm=TRUE),
          body = mean(body,na.rm=TRUE),
          MAS_pattern = first(pattern)
          
        )%>%ungroup()
        
        ips_TOBI <- setDF(TOBI) %>%filter(MAS_structure=="toneme")%>%group_by(id_ip,TOBI_pattern)%>%summarise(
          freq = n()
          
        )%>%ungroup()%>%rename(TOBI=TOBI_pattern)
        
        ips_prosody <- setDF(data2) %>%group_by(filename,spk,id_ip)%>% summarise(
          palabras = n_distinct(id_word,na.rm = TRUE),
          intensity = mean(intensity),
          pitch_reset_from_ST = mean(inflexion_ST_from_prev,na.rm = TRUE),
          pitch_reset_to_ST = mean(inflexion_ST_to_next,na.rm = TRUE),
          pitch_Hz = mean(center_Hz,na.rm = TRUE),
          pitch_St = mean(12*log2(center_Hz/1),na.rm = TRUE),
          range_st = mean(12*log2(max(center_Hz)/min(center_Hz))),
          inflexion_st = mean(12*log2(q4piHz/q1piHz)),
          dur = last(time_end)-first(time_start),
          velocidad = (last(time_end)-first(time_start))/n_distinct(id_word,na.rm = TRUE),
          sexo = NA,
          edad = NA,
          nivel = NA,
          grupos = NA,
          intervenciones = NA
          
        )
        
speakers <- ips_prosody%>%group_by(spk)%>%
          summarise(
            grupos_entonativos_spk=n(),
            palabras_cantidad_spk=sum(palabras,na.rm = TRUE),
            palabras_mean_spk=mean(palabras,na.rm=TRUE),
            pitch_Hz_spk = mean(pitch_Hz,na.rm=TRUE),
            pitch_SD_spk = sd(pitch_Hz,na.rm=TRUE),
            range_mean_spk = mean(range_st,na.rm=TRUE),
            range_SD_spk = sd(range_st,na.rm=TRUE),
            intensity_mean_spk=mean(intensity,na.rm=TRUE),
            intensity_SD_spk=sd(intensity,na.rm=TRUE),
            inflexion_mean_spk=mean(inflexion_st,na.rm=TRUE),
            inflexion_SD_spk=sd(inflexion_st,na.rm=TRUE), 
            dur_mean_spk = mean(dur, na.rm=TRUE),
            dur_SD_spk = sd(dur, na.rm=TRUE),
            velocidad_spk=mean(velocidad,na.rm=TRUE),
            velocidad_SD_spk=sd(velocidad,na.rm=TRUE))%>%mutate_if(is.numeric,round,digits=2)

ips_prosody <- ips_prosody %>% left_join(speakers, by="spk")%>%mutate(
  
  dif_pitch = 12*log2(pitch_Hz/pitch_Hz_spk),
  dif_range = range_st-range_mean_spk,
  dif_inten = intensity-intensity_mean_spk,
  dif_inflexion = inflexion_st-inflexion_mean_spk,
  dif_dur = dur - dur_mean_spk,
  dif_velocidad = velocidad - velocidad_spk,
  dif_pitch_sign = (pitch_Hz-pitch_Hz_spk)/pitch_SD_spk,
  dif_range_sign = (range_st-range_mean_spk)/range_SD_spk,
  dif_inten_sign = (intensity-intensity_mean_spk)/intensity_SD_spk,
  dif_inflexion_sign = (inflexion_st-inflexion_mean_spk)/inflexion_SD_spk,
  dif_dur_sign = (dur - dur_mean_spk)/dur_SD_spk,
  dif_velocidad_sign = (velocidad - velocidad_spk)/velocidad_SD_spk)%>%ungroup() %>%mutate_if(is.numeric,round,digits=2)
  
        ips <- ips %>%left_join(ips_sentiment%>%select(id_ip,qpos,qneg,qjoy,qfear,qsurprise,qdisgust,qanticip),by="id_ip")%>%ungroup()
        ips <- ips %>% left_join(ips_prosody%>%select(id_ip,
                                                      pitch_Hz,
                                                      pitch_St,
                                                      intensity,
                                                      inflexion_st,
                                                      range_st,
                                                      dur,
                                                      velocidad,
                                                      pitch_reset_from_ST,
                                                      pitch_reset_to_ST,
                                                      dif_pitch,
                                                      dif_range,
                                                      dif_inten,
                                                      dif_inflexion,
                                                      dif_dur,
                                                      dif_velocidad,
                                                      dif_pitch_sign,
                                                      dif_range_sign,
                                                      dif_inten_sign,
                                                      dif_inflexion_sign,
                                                      dif_dur_sign,
                                                      dif_velocidad_sign
                                                      
                                                      ), by="id_ip")
        ips <- ips %>% left_join(ips_MAS%>%select(id_ip,anacrusis,body,toneme,MAS_pattern), by="id_ip")%>%mutate_if(is.numeric,round,2)
        ips <- ips %>% left_join(ips_TOBI%>%select(id_ip,TOBI), by="id_ip")
        ips <- ips %>% mutate(time_start_ip = time_start, time_end_ip = time_end)
        
        incProgress(1, detail = "Getting and completing turns")
        
        ips <-  ips
        turnsdb <- ips %>%arrange(filename,time_start)
        turnsdb$change <- ifelse(turnsdb$filename == lead(turnsdb$filename,1) & turnsdb$tier_name != lead(turnsdb$tier_name,1) ,turnsdb$id_turn <- "change", 
                                 ifelse(turnsdb$filename != lead(turnsdb$filename,1),
                                        turnsdb$id_turn <- "change",
                                        turnsdb$id_turn <- NA))
        turnsdb <-  turnsdb %>% group_by(filename,change) %>% mutate(id_turn = row_number())
        turnsdb$id_turn <- ifelse(turnsdb$change != "change", turnsdb$id_turn <- NA, turnsdb$id_turn <- turnsdb$id_turn)
        
        # Orden de las turns
        
        turnsdb <-  turnsdb %>% group_by(filename) %>% 
          fill(id_turn, .direction = "up") #default direction down
        
        # time_start de las turns
        
        turnsdb$time_start_int <- ifelse(lag(turnsdb$change == "change",1) | turnsdb$id_turn == 1, turnsdb$time_start_int <- turnsdb$time_start, turnsdb$time_start_int <- NA)
        turnsdb <-  turnsdb %>% group_by(filename) %>% 
          fill(time_start_int, .direction = "down") #default direction down
        
        # time_end de las turns
        
        turnsdb$time_end_int <- ifelse(turnsdb$change == "change" | turnsdb$id_turn == 1, turnsdb$time_end_int <- turnsdb$time_end, turnsdb$time_end_int <- NA)
        turnsdb <-  turnsdb %>% group_by(filename) %>% 
          fill(time_end_int, .direction = "up") #default direction down
        
        # Duración de las turns
        
        turnsdb$dur_int <- turnsdb$time_end_int - turnsdb$time_start_int
        
        turnsdb <-  turnsdb %>% group_by(filename,id_turn) %>% mutate(ordgeint = row_number())
        
        #Creación de las pauses
        
        turnsdb$pause <- ifelse(turnsdb$filename == lead(turnsdb$filename,1) & turnsdb$tier_name == lead(turnsdb$tier_name,1), lead(turnsdb$time_start,1)-turnsdb$time_end,turnsdb$pause <-NA)
        turnsdb$pausecod <- ifelse(turnsdb$pause<500, turnsdb$pausecod <- "/", 
                                   ifelse(turnsdb$pause>=501 & turnsdb$pause <= 1000, turnsdb$pausecod <- "//", 
                                          ifelse(turnsdb$pause>1001,turnsdb$pausecod <- paste("/// (", round((turnsdb$pause*0.001),1),")",sep = ""),turnsdb$pausecod <-NA))) 
        turnsdb$pausecod[is.na(turnsdb$pausecod)] <- " "
        
        #Creación del valor de fto
        
        turnsdb<- turnsdb%>%ungroup()%>%mutate(fto_post = ifelse((filename == lead(filename,1) 
                                                                  & tier_name != lead(tier_name,1)), 
                                                                 (lead(time_start,1)-time_end),NA),
                                               fto_post_cod = ifelse(fto_post>0 & fto_post<50,"§",NA),
                                               fto_pre = ifelse((filename == lag(filename,1) 
                                                                 & tier_name != lag(tier_name,1)), 
                                                                (time_start-lag(time_end,1)),NA),
                                               fto_prev_cod = ifelse(fto_pre>0 & fto_pre<50,"§",NA))
        
        turnsdb <- turnsdb%>%mutate(content1 = ifelse(!is.na(fto_post_cod)|!is.na(fto_prev_cod),paste(fto_prev_cod,content,pausecod,fto_post_cod, sep = ""), content),
                                    content1 = gsub("^NA","",content1),
                                    content1 = gsub("NA$","",content1),
                                    content1 = ifelse(!is.na(pausecod),paste(content1,pausecod, sep = ""), content1),
                                    content1 = gsub("NA$","",content1))
        
        
        #Eliminación de los espacios iniciales
        
        turnsdb$content1 <- trimws(turnsdb$content1)
        
        #Creación de la variable "intervención" y de la base de datos "turns"
        
        turnsdb <- turnsdb %>% 
          group_by(filename,id_turn) %>% 
          mutate(intervencion = paste0(content1, collapse = " "))
        turnsdb$intervencion <- ifelse(turnsdb$change !="change", turnsdb$intervencion <- NA, turnsdb$intervencion <-turnsdb$intervencion)
        turnsdb <- turnsdb %>% mutate(id_turn = paste0(spk,"_int_",time_start_int, sep=""),id_ip=id_ip)
        turnsdb <-  turnsdb %>% group_by(filename) %>% 
          fill(intervencion, .direction = "up") 
        
        turns <- turnsdb%>%filter(change == "change")%>% select(tier_name,spk,filename,time_end_int,time_start_int,intervencion)
        turns <- turns %>% mutate(id_turn = paste(spk,"_int_",time_start_int, sep=""))
        turns$intervencion_export <- paste(turns$spk,": ",turns$intervencion,sep = "")
        turns_export <- subset(turns, select=c(id_turn,time_start_int,intervencion_export,filename))
        turns_export <- turns_export %>% mutate(inicio = format(as.POSIXct(time_start_int , "UTC", origin = "1970-01-01"), "%H:%M:%OS2"))%>%select(inicio,intervencion_export,filename)
        
        intervenciones <- turns_export %>%mutate(source=filename)
        grupos_ampliados2 <- ips%>%mutate(source=filename)
        tokenized_tagged <- words%>%mutate(source=filename)
       
      }
      
      
      output$wordsdf <- renderDT({
        DT::datatable(tokenized_tagged)
      })
      
      output$ipsdf <- renderDT({
        DT::datatable(grupos_ampliados2)
      })
      
      output$prosodyTable <- renderDT({
        DT::datatable(prosody_j)
      })
      
      output$turnsdf <- renderDT({
        DT::datatable(intervenciones)
      })
      
      
      create_db_if_not_exists()
      
      db <- dbConnect(duckdb::duckdb(), dbdir = "furious.duckdb")
      
      prosody_saved <- check_and_save_to_duckdb(db, "prosody", prosody, "id_prosody")
      ips_saved <- check_and_save_to_duckdb(db, "ips", grupos_ampliados2, "id_ip")
      turns_saved <- check_and_save_to_duckdb(db, "turns", intervenciones, "id_turn")
      words_saved <- check_and_save_to_duckdb(db, "words", tokenized_tagged, "id_word")

      # Disconnect from the database
      dbDisconnect(db)
      
      incProgress(1, detail = "Saving data to database")
      
      # Check if any table was altered
      if (prosody_saved | ips_saved | turns_saved | words_saved) {
        showModal(modalDialog(
          title = "Database Altered",
          "Database altered correctly.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        showModal(modalDialog(
          title = "No Alterations",
          "Database not altered. This data is already there.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
    })
    
    output$download_turns <- downloadHandler(
      filename = function() {
        paste("turns-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(intervenciones, file, row.names = FALSE)
      }
    )
    
    output$download_ips <- downloadHandler(
      filename = function() {
        paste("ips-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(grupos, file, row.names = FALSE)
      }
    )
    
    output$download_words <- downloadHandler(
      filename = function() {
        paste("words-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(tokenized_tagged, file, row.names = FALSE)
      }
    )
    
  })
  
  dffiltered <-reactive({
    # Construct SQL query to filter data based on selected filename
    con <- connect_db()
    query <- glue::glue("
      SELECT 
      id_ip,
      spk,
             source,
             time_start,
             time_end,
             content,
             pitch_Hz,
             intensity,
             inflexion_st,
             range_st,
             dur,
             qnoun,
             qverb,
             qadv,
             qaux,
             qcconj,
             qsconj,
             qadj,
             qdet,
             qpron,
             qpos,
             qneg,
             velocidad,
             dif_velocidad,
             dif_pitch_sign,
             dif_range_sign,
             dif_inten_sign,
             dif_inflexion_sign,
             dif_dur_sign,
             dif_velocidad_sign
      FROM ips
      WHERE source = '{input$furious_filename}' ORDER BY time_start ASC
    ")
    
    # Execute query and fetch data
    df <- dbGetQuery(con, as.character(query))
    dbDisconnect(con)
    df
  })
  
  dffiltered <-reactive({
    # Construct SQL query to filter data based on selected filename
    con <- connect_db()
    query <- glue::glue("
      SELECT 
      id_ip,
      spk,
             source,
             time_start,
             time_end,
             content,
             pitch_Hz,
             intensity,
             inflexion_st,
             range_st,
             dur,
             qnoun,
             qverb,
             qadv,
             qaux,
             qcconj,
             qsconj,
             qadj,
             qdet,
             qpron,
             qpos,
             qneg,
             velocidad,
             dif_velocidad,
             dif_pitch_sign,
             dif_range_sign,
             dif_inten_sign,
             dif_inflexion_sign,
             dif_dur_sign,
             dif_velocidad_sign
      FROM ips
      WHERE source = '{input$furious_filename}' ORDER BY time_start ASC
    ")
    
    # Execute query and fetch data
    df <- dbGetQuery(con, as.character(query))
    dbDisconnect(con)
    df
  })
  
  
  df1 <- eventReactive(input$furious_button,{
    df <- dffiltered()
    df2 <- df %>%group_by(source)%>%mutate(pausa_inter = ifelse(spk!=lag(spk), time_start - lag(time_end),NA),pausa_intra = ifelse(spk==lag(spk),time_start-lag(time_end),NA))%>%ungroup()%>%
      mutate(
        furioso = ifelse(
          pitch_Hz >= input$slider_pitch[1] & pitch_Hz <= input$slider_pitch[2] &
            intensity >= input$slider_intensity[1] & intensity <= input$slider_intensity[2] &
            inflexion_st >= input$slider_inflexion[1] & inflexion_st <= input$slider_inflexion[2] &
            range_st >= input$slider_range[1] & range_st <= input$slider_range[2] &
            dur >= input$slider_dur[1] & dur <= input$slider_dur[2] &
            velocidad >= input$slider_velocidad[1] & velocidad <= input$slider_velocidad[2] &
            dif_pitch_sign >= input$slider_dif_pitch_sign[1] & dif_pitch_sign <= input$slider_dif_pitch_sign[2] &
            dif_range_sign >= input$slider_dif_range_sign[1] & dif_range_sign <= input$slider_dif_range_sign[2] &
            dif_inten_sign >= input$slider_dif_inten_sign[1] & dif_inten_sign <= input$slider_dif_inten_sign[2] &
            dif_inflexion_sign >= input$slider_dif_inflexion_sign[1] & dif_inflexion_sign <= input$slider_dif_inflexion_sign[2] &
            dif_dur_sign >= input$slider_dif_dur_sign[1] & dif_dur_sign <= input$slider_dif_dur_sign[2] &
            dif_velocidad_sign >= input$slider_dif_velocidad_sign[1] & dif_velocidad_sign <= input$slider_dif_velocidad_sign[2],
          "no", "yes"
        )) %>%mutate(spk = ifelse(furioso == "yes", paste(spk, "_FUR"), spk))%>%filter(!is.na(spk),spk!="NA")
    return(df2)

  })
  
  
  filenames <- reactive({
    # Query to fetch unique filenames from grupos table
    if (file.exists(db_file_path)) {
      con <- connect_db()
      
      # Check if the 'ips' table exists
      table_exists_query <- "
      SELECT COUNT(*) AS count 
      FROM information_schema.tables 
      WHERE table_name = 'ips' AND table_schema = 'main'
    "
      table_exists <- dbGetQuery(con, table_exists_query)$count > 0
      
      if (table_exists) {
        # Query to fetch unique sources from the ips table
        query <- "SELECT DISTINCT source FROM ips ORDER BY source"
        df <- dbGetQuery(con, query)$source  # Assuming 'source' is the column name
      } else {
        # Handle the case where the 'ips' table does not exist
        df <- NULL
      }
      
      dbDisconnect(con)
      return(df)
    } else {
      # Return NULL or handle the case where the file doesn't exist
      return(NULL)
    }
  })
  
  # Update selectInput choices based on filenames
  observe({
    updateSelectInput(session, "furious_filename", choices = filenames())
  })
  
  
  
  output$content_output <- renderUI({
    
    validate(
      need(!is.null(df1()) && nrow(df1()) > 0, "Haz click en alguno de los puntos del gráfico de arriba o en el mapa de calor"),
      need(!is.null(dtlinear2()) && nrow(dtlinear2()) > 0, "Haz click en alguno de los puntos del gráfico de arriba o en el mapa de calor")
    )
    
    df <- df1() %>%
      filter(
        time_start >= (dtlinear2()$time_start - (input$timecontext)),
        time_start <= (dtlinear2()$time_start + (input$timecontext))
      ) %>%
      mutate(
        content = ifelse(id_ip == dtlinear2()$id_ip, paste0("*****", content, "*****"), content),
        combined = paste0(spk, ": ", content),
        time_start_formatted = format(as.POSIXct(time_start , origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
      )
    
    # Format time for additional text
    time_start_first <- df$time_start[1]
    time_end_last <- df$time_end[nrow(df)]
    source <- df$source[1]
    
    formatted_time_start <- format(as.POSIXct(time_start_first , origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    formatted_time_end <- format(as.POSIXct(time_end_last , origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    
    additional_text <- paste0(" (Corpus de Conversaciones coloquiales, ", source ,", from time ",formatted_time_start, " to time ", formatted_time_end, ")")
    
    tagList(
      tags$audio(
        id = "plyr-audio",
        preload = "auto",
        class = "plyr",  # The class is important for Plyr to identify the element
        controls = NA,
        tags$source(src = paste0("audios/", source, ".mp3"), type = "audio/mp3")
      ),
      tags$div(
        id = "content-container",
        
        lapply(1:nrow(df), function(i) {
          content_html <- htmltools::htmlEscape(df$combined[i])
          
          color <- ifelse(df$furioso[i] == "yes", "red", "black")
          
          if (input$toggle_crossout && grepl("<risas/>|\\[", df$content[i]) & df$furioso[i] == "yes") {
            content_html <- paste0("<del>", content_html, "</del>")
          }
          
          icons_before <- tagList()
          icons_after <- tagList()
          
          if (input$add_icons) {
            if (!is.na(df$velocidad[i]) && (df$velocidad[i] < input$slider_velocidad[1] || df$dif_velocidad_sign[i] < input$slider_dif_velocidad_sign[1])) {
              icons_before <- tagList(icons_before, "\U1F422")
              icons_after <- tagList(icons_after, "\U1F422")
            }
            if (!is.na(df$velocidad[i]) && (df$velocidad[i] > input$slider_velocidad[2] || df$dif_velocidad_sign[i] > input$slider_dif_velocidad_sign[2])) {
              icons_before <- tagList(icons_before, "\U1F6B4")
              icons_after <- tagList(icons_after, "\U1F6B4")
            }
            
            if (!is.na(df$range_st[i]) && (df$range_st[i] < input$slider_range[1] || df$dif_range_sign[i] < input$slider_dif_range_sign[1])) {
              icons_before <- tagList(icons_before, "⬇")
              icons_after <- tagList(icons_after, "⬇")
            }
            if (!is.na(df$range_st[i]) && (df$range_st[i] > input$slider_range[2] || df$dif_range_sign[i] > input$slider_dif_range_sign[2])) {
              icons_before <- tagList(icons_before, "⬆")
              icons_after <- tagList(icons_after, "⬆")
            }
            
            if (!is.na(df$pitch_Hz[i]) && (df$pitch_Hz[i] < input$slider_pitch[1] || df$dif_pitch_sign[i] < input$slider_dif_pitch_sign[1])) {
              icons_before <- tagList(icons_before, "🎷")
              icons_after <- tagList(icons_after, "🎷")
            }
            if (!is.na(df$pitch_Hz[i]) && (df$pitch_Hz[i] > input$slider_pitch[2] || df$dif_pitch_sign[i] > input$slider_dif_pitch_sign[2])) {
              icons_before <- tagList(icons_before, "🎻")
              icons_after <- tagList(icons_after, "🎻")
            }
            
            if (!is.na(df$intensity[i]) && (df$intensity[i] < input$slider_intensity[1] || df$dif_inten_sign[i] < input$slider_dif_inten_sign[1])) {
              icons_before <- tagList(icons_before, "🔇")
              icons_after <- tagList(icons_after, "🔇")
            }
            if (!is.na(df$intensity[i]) && (df$intensity[i] > input$slider_intensity[2] || df$dif_inten_sign[i] > input$slider_dif_inten_sign[2])) {
              icons_before <- tagList(icons_before, "🔊")
              icons_after <- tagList(icons_after, "🔊")
            }
            
            if (!is.na(df$inflexion_st[i]) && (df$inflexion_st[i] < input$slider_inflexion[1] || df$dif_inflexion_sign[i] < input$slider_dif_inflexion_sign[1])) {
              icons_before <- tagList(icons_before, "↘️")
              icons_after <- tagList(icons_after, "↘️")
            }
            if (!is.na(df$inflexion_st[i]) && (df$inflexion_st[i] > input$slider_inflexion[2] || df$dif_inflexion_sign[i] > input$slider_dif_inflexion_sign[2])) {
              icons_before <- tagList(icons_before, "↗️")
              icons_after <- tagList(icons_after, "↗️")
            }
            
            if (!is.na(df$dur[i]) && (df$dur[i] < input$slider_dur[1] || df$dif_dur_sign[i] < input$slider_dif_dur_sign[1])) {
              icons_before <- tagList(icons_before, "🤏")
              icons_after <- tagList(icons_after, "🤏")
            }
            if (!is.na(df$dur[i]) && (df$dur[i] > input$slider_dur[2] || df$dif_dur_sign[i] > input$slider_dif_dur_sign[2])) {
              icons_before <- tagList(icons_before, "⏳")
              icons_after <- tagList(icons_after, "⏳")
            }
          }
          
          content_with_icons <- tagList(
            icons_before,
            HTML(content_html),
            icons_after
          )
          
          tags$div(
            style = paste0("cursor: pointer; color: ", color, ";"),
            class = "audio-row",
            `data-start` = as.numeric(df$time_start[i]),
            `data-end` = as.numeric(df$time_end[i]),
            `data-source` = df$source[i],
            df$time_start_formatted[i], " - ", content_with_icons
          )
        })
      ),
      tags$p(additional_text),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function () {
          const player = new Plyr('#plyr-audio', { controls: ['play', 'progress', 'current-time', 'duration', 'mute', 'volume'] });
        });
      ")),
      # JavaScript code to handle clicks
      tags$script(HTML(paste0("
      $(document).ready(function() {
        var sound = new Howl({
          src: ['audios/", source, ".mp3'],
          format: ['mp3'],
          preload: true
        });

        $(document).on('click', '.audio-row', function() {
          var startTime = parseFloat($(this).data('start'));
          var endTime = parseFloat($(this).data('end'));
          var source = $(this).data('source');

          if (sound._src !== 'audios/' + source + '.mp3') {
            sound.unload();
            sound = new Howl({
              src: ['audios/' + source + '.mp3'],
              html5: true,
              format: ['mp3'],
              preload: true
            });
          }

          if (sound.playing()) {
            sound.stop();
          }

          sound.seek(startTime);
          sound.play();

          var checkInterval = setInterval(function() {
            if (sound.seek() >= endTime) {
              sound.stop();
              clearInterval(checkInterval);
            }
          }, 100);
        });
      });
    ")))
    )
  })
  

  
  output$download_transcription <- downloadHandler(
    filename = function() {
      paste("furioso_", Sys.time(), ".txt", sep = "")
    },
    content = function(file) {
      df <- df1() %>%
        filter(
          time_start >= (dtlinear2()$time_start - (input$timecontext)),
          time_start <= (dtlinear2()$time_start + (input$timecontext))
        ) %>%
        mutate(
          content = ifelse(id_ip == dtlinear2()$id_ip, paste0("*****", content, "*****"), content),
          combined = paste0(spk, ": ", content)
        )
      
      text_content <- lapply(1:nrow(df), function(i) {
        # Escape the combined content to show HTML tags as text
        content_html <-df$combined[i]
        
        # Conditionally add icons based on the checkbox value
        icons_before <- ""
        icons_after <- ""
        
        if(input$add_icons) {
          if(!is.na(df$dif_velocidad_sign[i]) && df$dif_velocidad_sign[i] < input$slider_dif_velocidad_sign[1]) {
            icons_before <- paste0(icons_before, "\U1F422")  # Turtle emoji
            icons_after <- paste0("\U1F422", icons_after)    # Turtle emoji
          }
          if(!is.na(df$dif_velocidad_sign[i]) && df$dif_velocidad_sign[i] > input$slider_dif_velocidad_sign[2]) {
            icons_before <- paste0(icons_before, "\U1F6B4")  # Bicycle emoji
            icons_after <- paste0("\U1F6B4", icons_after)    # Bicycle emoji
          }
          if(!is.na(df$dif_range_sign[i]) && df$dif_range_sign[i] < input$slider_dif_range_sign[1]) {
            icons_before <- paste0(icons_before, "⬇")        # Down arrow emoji
            icons_after <- paste0("⬇", icons_after)          # Down arrow emoji
          }
          if(!is.na(df$dif_range_sign[i]) && df$dif_range_sign[i] > input$slider_dif_range_sign[2]) {
            icons_before <- paste0(icons_before, "⬆")        # Up arrow emoji
            icons_after <- paste0("⬆", icons_after)          # Up arrow emoji
          }
          if(!is.na(df$dif_pitch_sign[i]) && df$dif_pitch_sign[i] < input$slider_dif_pitch_sign[1]) {
            icons_before <- paste0(icons_before, "🎷")        # Saxophone emoji
            icons_after <- paste0("🎷", icons_after)          # Saxophone emoji
          }
          if(!is.na(df$dif_pitch_sign[i]) && df$dif_pitch_sign[i] > input$slider_dif_pitch_sign[2]) {
            icons_before <- paste0(icons_before, "🎻️")        # Violin emoji
            icons_after <- paste0("🎻️", icons_after)          # Violin emoji
          }
          if(!is.na(df$dif_inten_sign[i]) && df$dif_inten_sign[i] < input$slider_dif_inten_sign[1]) {
            icons_before <- paste0(icons_before, "🔇")        # Muted speaker emoji
            icons_after <- paste0("🔇", icons_after)          # Muted speaker emoji
          }
          if(!is.na(df$dif_inten_sign[i]) && df$dif_inten_sign[i] > input$slider_dif_inten_sign[2]) {
            icons_before <- paste0(icons_before, "🔊️")        # Speaker emoji
            icons_after <- paste0("🔊️", icons_after)          # Speaker emoji
          }
          if(!is.na(df$dif_inflexion_sign[i]) && df$dif_inflexion_sign[i] < input$slider_dif_inflexion_sign[1]) {
            icons_before <- paste0(icons_before, "↘️")        # Down-right arrow emoji
            icons_after <- paste0("↘️", icons_after)          # Down-right arrow emoji
          }
          if(!is.na(df$dif_inflexion_sign[i]) && df$dif_inflexion_sign[i] > input$slider_dif_inflexion_sign[2]) {
            icons_before <- paste0(icons_before, "↗️️️")        # Up-right arrow emoji
            icons_after <- paste0("↗️️️", icons_after)          # Up-right arrow emoji
          }
          if(!is.na(df$dif_dur_sign[i]) && df$dif_dur_sign[i] < input$slider_dif_dur_sign[1]) {
            icons_before <- paste0(icons_before, "🤏")        # Pinching hand emoji
            icons_after <- paste0("🤏", icons_after)          # Pinching hand emoji
          }
          if(!is.na(df$dif_dur_sign[i]) && df$dif_dur_sign[i] > input$slider_dif_dur_sign[2]) {
            icons_before <- paste0(icons_before, "⏳")        # Hourglass emoji
            icons_after <- paste0("⏳", icons_after)          # Hourglass emoji
          }
        }
        
        # Combine icons and content
        content_with_icons <- paste0(
          icons_before,
          content_html,
          icons_after
        )
        
        # Create the final text content for each row
        content_output <- paste0(
          df$time_start_formatted[i], " - ", content_with_icons
        )
        
        return(content_output)
      })
      
      # Combine all rows into a single text
      final_text_content <- paste(text_content, collapse = "\n")
      
      # Write the final text content to the file
      writeLines(final_text_content, con = file)
    }
  )
  
  
  
  
  
  output$data_table2 <- renderDT({
    df <- df1()
    
    
  })
  
  
  output$furious_plot <- renderPlotly({
    df <- df1() %>% mutate(furious_var = !!input$furious_var)
    y_label <- as.character(input$furious_var)
    
    p <- ggplot(df, aes(x = time_start, y = furious_var, fill = spk)) +
      geom_point(size = 3, shape = 21) +  # shape 21 allows fill color
      ggtitle("Valores furiosos y no furiosos por hablante") +
      ylab(y_label) + xlab("Grupos entonativos a lo largo del tiempo")
    
    d <- ggplotly(p, source = "scatter_click")
    return(d)
  })
  
  
  output$furious_plot1 <- renderPlotly({
    df <- df1() %>%
      rename_with(~ paste0("phon_", .), 
                  .cols = c("pitch_Hz", "intensity", 
                            "inflexion_st", "range_st", "pausa_intra", 
                            "pausa_inter", "dur", "velocidad")) %>%
      rename_with(~ paste0("morph_", .), 
                  .cols = starts_with("q")) %>%
      select(time_start, all_of(!!input$heatmap_vars)) %>%
      mutate(across(.cols = starts_with("morph_q"), 
                    .fns = ~ ifelse(. == 0, NA, .))) %>%
      mutate(across(.cols = -time_start, 
                    .fns = ~ as.numeric(scale(.))))  

    df_long <- df %>%distinct(time_start,.keep_all = T)%>%
      pivot_longer(cols = -time_start, names_to = "variable", values_to = "value")%>%arrange(time_start)
    
    p <- ggplot(df_long, aes(y = variable, x = time_start, fill = value)) +
      geom_tile(color = "black", size = 0.2) +  # Optional: Add borders to tiles
      scale_fill_gradientn(
        colors = c("blue", "cyan", "green", "yellow", "orange", "red")  # Thermal vision color palette
      ) +
      ggtitle("Visión térmica por variable del discurso") +
      xlab("Grupos entonativos a lo largo del tiempo") +
      ylab("Variables") +
      theme_minimal()+
      theme(
        panel.background = element_blank(),     # Remove background color
        plot.background = element_blank(),      # Remove plot background
        panel.grid.major = element_blank(),     # Remove major gridlines
        panel.grid.minor = element_blank(),     # Remove minor gridlines
        axis.line = element_blank()             # Remove axis lines
      )
    
    d <- ggplotly(p, source = "heatmap_click") 
    
    return(d)
  })
  
  output$furious_boxplot <- renderPlotly({
    df <- df1() %>% mutate(furious_var = !!input$furious_var)
    y_label <- as.character(input$furious_var)
    
    p <- ggplot(df, aes(x = as.factor(spk), y = furious_var, fill = spk)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, size = 2) +  
      ggtitle("Diagrama de caja hablante furioso y no furioso") +
      ylab(y_label) +
      xlab("Time Start") +
      theme_minimal()  
    
    print(p)
  })
  
  model_directory <- "models"  
  
  language_files <- list(
    catalan = "catalan-ancora-ud-2.5-191206.udpipe",
    english = "english-ewt-ud-2.5-191206.udpipe",
    french = "french-gsd-ud-2.5-191206.udpipe",
    italian = "italian-isdt-ud-2.5-191206.udpipe",
    spanish = "spanish-gsd-ud-2.5-191206.udpipe"
  )
  
  model_path <- reactive({
    language_model_file <- language_files[[input$language]]
    
    full_path <- file.path(model_directory, language_model_file)
    
    if (file.exists(full_path)) {
      full_path  
    } else {
      stop(paste("No encuentro el modelo para el etiquetado:", input$language))  
    }
  })
    spmodel <- reactive({
    udpipe_load_model(model_path())
  })
  
  wordcloud_data_yes <- reactive({
    df <- df1() %>% filter(furioso == "yes")
    df1 <- udpipe_annotate(x=df$content,object = spmodel(), tokenizer = "horizontal",tagger = "default")%>%as.data.frame()
    
    df_tokens <- df1 %>%rename(word=token)%>%group_by(word)%>%summarise(n=n(),upos=first(upos))%>%
      filter(n > 1,!upos %in% input$wordcloud_var) 
    
   df_tokens
  })
  
  wordcloud_data_no <- reactive({
    df <- df1() %>% filter(furioso == "no")
    df1 <- udpipe_annotate(x=df$content,object = spmodel(), tokenizer = "horizontal",tagger = "default")%>%as.data.frame()
    
    df_tokens <- df1 %>%rename(word=token)%>%group_by(word)%>%summarise(n=n(),upos=first(upos))%>%
      filter(n > 1,!upos %in% input$wordcloud_var) 
    
  })
  
  # Render ggwordcloud for furious == "yes"
  output$wordcloud_yes <- renderWordcloud2({
    wordcloud2(wordcloud_data_yes(), size = 1, color = 'random-light', backgroundColor = "black")
  })
  
  # Render wordcloud for furious == "no"
  output$wordcloud_no <- renderWordcloud2({
    wordcloud2(wordcloud_data_no(), size = 1, color = 'random-light', backgroundColor = "black")
  })
  
  df_heatmap_spk <- reactive({
    
    df_medians <- df1() %>%
      group_by(spk) %>%
      summarize(across(.cols = c(pitch_Hz, intensity, inflexion_st, range_st, dur, velocidad,pausa_intra,pausa_inter, qverb, qadv, qaux, qcconj, qsconj, qadj, qdet, qpron, qpos, qneg), .fns = ~ round(mean(.x, na.rm = TRUE), 2)))
    
    
    
  })
  
  output$heatmap_spk <- renderPlotly({
    df_scaled <- df_heatmap_spk() %>%
      mutate(across(.cols = -spk, .fns = ~ as.numeric(scale(.x))))
    df_long <- df_scaled %>%
      pivot_longer(cols = -spk, names_to = "variable", values_to = "value")
    p <- ggplot(df_long, aes(x = variable, y = spk, fill = value)) +
      geom_tile(color = "black", size = 0.2) +  # Optional: Add borders to tiles
      scale_fill_gradient(low = "blue", high = "red") +  # Default ggplot2 gradient
      ggtitle("Visión térmica por variable del discurso") +
      xlab("Hablantes furiosos y no furiosos") +
      ylab("Variables") +
      theme_minimal() +
      theme(
        panel.background = element_blank(),     # Remove background color
        plot.background = element_blank(),      # Remove plot background
        panel.grid.major = element_blank(),     # Remove major gridlines
        panel.grid.minor = element_blank(),     # Remove minor gridlines
        axis.line = element_blank(),             # Remove axis lines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
    
  })
  
  output$heatmap_spk_table <- renderDT({
    
    df_heatmap_spk()
    
    
  })
  
  last_click <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "scatter_click"), {
    event_data_scatter <- event_data("plotly_click", source = "scatter_click")
    last_click(list(type = "scatter", data = event_data_scatter))
  })
  
  observeEvent(event_data("plotly_click", source = "heatmap_click"), {
    event_data_heatmap <- event_data("plotly_click", source = "heatmap_click")
    last_click(list(type = "heatmap", data = event_data_heatmap))
  })
  
  dtlinear2 <- reactive({
    click_info <- last_click()
    df <- df1() %>% mutate(selected = !!input$furious_var)
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    if (click_info$type == "scatter") {
      event_data_scatter <- click_info$data
      res_scatter <- df[event_data_scatter$x == df$time_start & event_data_scatter$y == df$selected, ]
      return(res_scatter)
    }
    
    if (click_info$type == "heatmap") {
      event_data_heatmap <- click_info$data
      res_heatmap <- df[event_data_heatmap$x == df$time_start, ]
      return(res_heatmap)
    }
    
    return(NULL)
  })
  
  output$dtlinear2 <- renderDT({
    
    dtlinear2()
  })
  
  relationships <- reactive({
    
    grupos <- df1()
    dd <- grupos%>%mutate(from=lag(spk,1),to=spk)%>%mutate(selection=!!input$furious_var)%>%select(from,to,selection,content)%>%filter(!is.na(from),!is.na(to))
    
    
  })
  
  output$webchart <- renderPlot({
    
    
    graph2 <- graph_from_data_frame(relationships(), directed = TRUE)
    
    E(graph2)$weight <- relationships()$selection
    
    edge_colors <- ifelse(E(graph2)$weight > input$sliderselect[2], "red", ifelse(E(graph2)$weight < input$sliderselect[1],"blue","green"))
    degred <- degree(graph = graph2,mode="in")
    plot(
      graph2,
      vertex.frame.color = "black",           
      vertex.shape=c("circle"), 
      edge.color = edge_colors,
      edge.arrow.size = 0.6,
      vertex.size=degred*0.3, 
      edge.lty="solid",
      edge.width = 0.5,
      layout=  layout_as_star,  
      main="Relaciones tonales de los hablantes con hablantes posteriores"
      
    )
    legend(1, 1, legend=c("Nivel tonal alto", "Nivel tonal neutro","Nivel tonal bajo"),
           col=c("red","green", "blue"), lty=1, cex=0.8)
    

  })
  
  FAMD_results <- reactive({
    
    df <- df1() %>%
      mutate(pausa = paste(pausa_intra, pausa_inter)) %>%
      select(spk, everything(),-furioso, -source, -id_ip, -pausa_intra, -pausa_inter, -time_start, -time_end, -content,-pausa) %>%
      na.omit()
    
    FAMD_results <- FactoMineR::PCA(df,quali.sup = 1)
    
    
  })  
  
  output$FAMD_plot_variables <- renderPlot({
    
    factoextra::fviz_pca_var(FAMD_results(), repel = TRUE)
    
  })
  
  output$FAMD_plot_categories <- renderPlot({
    
    factoextra::fviz_pca_ind(FAMD_results(),habillage ="spk",label="none",pointsize = 5)
    
  })
  
  
  output$FAMD_plot_eigenvalues <- renderPlot({
    
    factoextra::fviz_eig(FAMD_results())
    
  })
  
  tree_df <- reactive({
    df <- df1()  %>%
      select(everything(), -furioso, -pausa_inter, -pausa_intra, -source, -id_ip, -time_start, -time_end, -content) %>%
      na.omit() %>%
      mutate_if(is.character, as.factor)
        df$spk <- as.numeric(factor(df$spk))
        spk_levels <- levels(factor(df1()$spk))  
    spk_mapping <- setNames(seq_along(spk_levels), spk_levels)
    
    list(tree = partykit::ctree(spk ~ ., data = df%>%mutate(spk=as.factor(spk)), control = partykit::ctree_control(maxdepth = 3)),
         spk_mapping = spk_mapping)
  })
  
  output$tree_plot <- renderPlot({
    
    plot(tree_df()$tree)
  })
  
  output$spk_mapping_text <- renderText({
    spk_mapping <- tree_df()$spk_mapping
    paste("Correspondencia de hablantes en el mapa: ", paste(names(spk_mapping), "=", spk_mapping, collapse = ", "))
  })
  
  observeEvent(input$show_legend, {
    showModal(modalDialog(
      title = "Leyendas prosódicas",
      HTML(paste(
        "<ul>",
        "<li>🐢 - Velocidad baja</li>",
        "<li>🚴️ - Velocidad alta</li>",
        "<li>⬇️ - Rango bajo</li>",
        "<li>⬆️ - Rango alto</li>",
        "<li>🎷 - Tono bajo</li>",
        "<li>🎻 - Tono alto</li>",
        "<li>🔇 - Intensidad baja</li>",
        "<li>🔊 - Intensidad alta</li>",
        "<li>↘️ - Inflexión descendente</li>",
        "<li>↗️ - Inflexión ascendente</li>",
        "<li>🤏 - Duración baja</li>",
        "<li>⏳ - Duración alta</li>",
        "</ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$process, {

    show("refresh_button")
  })
  
  # Reactive value to hold the plain text prompt
  gpt_response_reactive <- reactiveVal()
  
  # Reactive value to store the plain text prompt
  prompt_text_plain <- reactiveVal()
  
  # When the button is clicked, generate the prompt and call the GPT API
  observeEvent(input$generate_prompt, {
    df <- df1()  # Getting filtered data
    
    # Generating summaries
    boxplot_prompt <- boxplot_summary(df)
    pca_prompt <- pca_summary(FAMD_results())
    tree_prompt <- tree_summary(tree_df()$tree)
    
    # Create prompt with HTML breaks for display in the UI
    full_prompt_html <- paste(
      "Analiza las similitudes y diferencias entre los hablantes utilizando los valores medianos, resultados de PCA y resultados del árbol de decisiones que proporciono. Dámelo en formato Markdown:", "<br/><br/>",
      "Speaker Analysis:", "<br/>", boxplot_prompt, "<br/><br/>",
      "PCA Results:", "<br/>", pca_prompt, "<br/><br/>",
      "Tree Results:", "<br/>", tree_prompt
    )
    
    # Create prompt without HTML breaks for copying to clipboard
    full_prompt_plain <- paste(
      "Analiza las similitudes y diferencias entre los hablantes utilizando los valores medianos, resultados de PCA y resultados del árbol de decisiones que proporciono. Dámelo en formato Markdown:", "\n\n",
      "Speaker Analysis:\n", boxplot_prompt, "\n\n",
      "PCA Results:\n", pca_prompt, "\n\n",
      "Tree Results:\n", tree_prompt
    )
    
    # Update the reactive value with the plain text version
    prompt_text_plain(full_prompt_plain)
    
    # Call the OpenAI API to get the response (Ensure your API key is stored securely)
    # api_key <- Sys.getenv("OPENAI_API_KEY")  # Replace with secure storage for API keys
    # gpt_response <- get_gpt_response(full_prompt_html, api_key)
    # 
    # # Update the reactive value with the GPT response
    # gpt_response_reactive(gpt_response)
    # 
    # # Render the GPT response in your UI
    # output$gpt_response <- renderUI({
    #   req(gpt_response_reactive())  # Ensure GPT response is available
    #   gpt_response_reactive() # Convert Markdown to HTML
    # })
    
    # Update the output to display the generated prompt in HTML format
    output$prompt_output <- renderUI({
      HTML(full_prompt_html)
    })
    
    # Update the copy button with the text that should be copied
    output$copy_button <- renderUI({
      rclipboard::rclipButton(
        inputId = "copy_prompt_button",  # Button ID
        label = "Copy Prompt",           # Button label
        clipText = prompt_text_plain(),  # Get the reactive value (plain text)
        icon = icon("clipboard")         # Optional icon
      )
    })
  })

  
  # Show a modal dialog after the copy button is clicked
  observeEvent(input$copy_prompt_button, {
    showModal(
      modalDialog(
        title = "Copia realizada",
        "Copia realizada. Pégalo ahora en CHATGPT.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
    )
  })
  
  chat_data <- reactiveVal(data.frame())
  
  observeEvent(input$send_message, {
    if (input$user_message != "") {
      new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))
      
      gpt_res <- call_gpt_api(input$api_key, input$user_message, input$model_name, input$temperature, input$max_length)
      
      if (!is.null(gpt_res)) {
        gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
      }
      updateTextInput(session, "user_message", value = "")
    }
  })
  
  call_gpt_api <- function(api_key, prompt, model_name, temperature, max_length) {
    
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = model_name,
        messages = list(
          list(role = "user", content = prompt)
          # list(role = "system", content = sysprompt)
        ),
        temperature = temperature,
        max_tokens = max_length
      )
    )
    return(str_trim(content(response)$choices[[1]]$message$content))
  }
  
  output$chat_history <- renderUI({
    current_message <- chat_data()[nrow(chat_data()), ]
    
    chatBox <- tags$div(
      class = ifelse(current_message["source"] == "User", "alert alert-secondary", "alert alert-success"),
      HTML(paste0("<b>", current_message["source"], ":</b> ", 
                  markdownToHTML(text = current_message["message"], fragment.only = TRUE)))
    )
    
    do.call(tagList, list(chatBox))
  })
  output$download_response <- downloadHandler(
    filename = function() {
      paste("response-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      current_message <- chat_data()[nrow(chat_data()), ]
      writeLines(paste(current_message["source"], ":", current_message["message"]), file)
    }
  )


  

  
}

shinyApp(ui, server)
