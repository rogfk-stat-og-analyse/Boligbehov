# app.R ----------------------------------------------------------
options(encoding = "UTF-8")

# Pakker ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(httr2)
library(rjstat)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

#-------------------------------------------------
# 1. Funksjon for SSB PxWeb v2 -------------------
#-------------------------------------------------

ssb_pxweb2_get <- function(url) {
  req <- request(url) |>
    req_method("GET")
  
  resp <- req_perform(req)
  resp_check_status(resp)
  
  raw_json <- resp_body_string(resp)
  js <- rjstat::fromJSONstat(raw_json)
  
  if (is.data.frame(js)) js else js[[1]]
}

get_06265 <- function() {
  message("Henter 06265-data fra SSB API ...")
  
  url_06265 <- paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/06265/data?",
    "lang=no",
    "&valueCodes[ContentsCode]=Boliger",
    "&valueCodes[Region]=",
    "K-1101,K-1103,K-1106,K-1108,K-1111,K-1112,K-1114,K-1119,",
    "K-1120,K-1121,K-1122,K-1124,K-1127,K-1130,K-1133,K-1134,",
    "K-1135,K-1144,K-1145,K-1146,K-1149,K-1151,K-1160",
    "&valueCodes[BygnType]=01,02,03,04,05,999",
    "&valueCodes[Tid]=",
    "2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,",
    "2017,2018,2019,2020,2021,2022,2023,2024,2025",
    "&codelist[Region]=agg_KommSummer",
    "&outputValues[Region]=aggregated"
  )
  
  ssb_pxweb2_get(url_06265)
}

#-------------------------------------------------
# 2. Hent data (direkte fra API) og klargjør -----
#-------------------------------------------------

df_06265 <- get_06265()

# rjstat::fromJSONstat() gir her allerede kolonner som 'år', 'region',
# 'bygningstype' og 'value' (slik som i originalscriptet ditt).
# Vi standardiserer kun typer/encoding.
df_06265 <- df_06265 |>
  mutate(
    år     = as.integer(år),
    value  = as.numeric(value),
    region = enc2utf8(as.character(region)),
    bygningstype = enc2utf8(as.character(bygningstype))
  )

#-------------------------------------------------
# Fargeblind-vennlig palett (Okabe–Ito)
#-------------------------------------------------

pal_okabe <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

#-------------------------------------------------
# 3. UI
#-------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    titleWidth = 260,
    title = tagList(
      span("Boliger", style = "font-weight:600; font-size:18px;"),
      span("SSB 06265", style = "font-weight:400; font-size:12px; margin-left:6px; opacity:0.8;")
    )
  ),
  
  dashboardSidebar(
    width = 260,
    
    div(
      style = "padding: 12px 16px 4px 16px; color:#e5e7eb; text-transform:uppercase; letter-spacing: .08em; opacity:0.85;",
      "Filtre"
    ),
    
    div(
      style = "padding: 0 16px 16px 16px;",
      
      selectInput(
        "region",
        "Velg region (kommune eller område):",
        choices = sort(unique(df_06265$region)),
        selected = sort(unique(df_06265$region))[1]
      ),
      
      checkboxGroupInput(
        "bygningstype",
        "Velg én eller flere bygningstyper:",
        choices = unique(df_06265$bygningstype),
        selected = unique(df_06265$bygningstype)
      ),
      
      sliderInput(
        "aar",
        "Velg periode (år fra og til):",
        min = min(df_06265$år, na.rm = TRUE),
        max = max(df_06265$år, na.rm = TRUE),
        value = c(min(df_06265$år), max(df_06265$år)),
        step = 1,
        sep = ""
      ),
      
      checkboxInput("high_contrast", "Høy kontrast", value = FALSE)
    ),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-column"))
    )
  ),
  
  dashboardBody(
    uiOutput("theme_css"),
    
    tags$head(
      tags$style(HTML("
        /* Litt pynt + bedre fokusmarkeringer */
        .content-wrapper, .right-side { background: #f3f4f6; }
        .box { border-radius: 14px !important; border: 0 !important; box-shadow: 0 14px 30px rgba(15, 23, 42, 0.14); }
        .box-header.with-border { border-bottom: 1px solid rgba(148, 163, 184, 0.35); }
        .box-title { font-weight: 700; }
        .main-header .logo { font-weight: 700; }
        .btn:focus, button:focus, a:focus { outline: 3px solid #111827 !important; outline-offset: 2px; }
        .sr-only { position:absolute !important; width:1px !important; height:1px !important; padding:0 !important; margin:-1px !important; overflow:hidden !important; clip:rect(0,0,0,0) !important; border:0 !important; }
        .fullscreen-btn { position:absolute; top:8px; right:8px; z-index:999; }
      ")),
      tags$script(HTML("
        function toggleFullscreen(id) {
          var el = document.getElementById(id);
          if (!document.fullscreenElement) { el.requestFullscreen(); }
          else { document.exitFullscreen(); }
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "dash",
        
        fluidRow(
          valueBoxOutput("kpi_n_siste", width = 4),
          valueBoxOutput("kpi_endring_abs", width = 4),
          valueBoxOutput("kpi_endring_pct", width = 4)
        ),
        
        fluidRow(
          tabBox(
            width = 12,
            id = "dash_inner_tabs",
            
            tabPanel(
              title = "Antall boliger per år",
              box(
                title = "Antall boliger per år",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                div(
                  id = "plot_boliger_container",
                  style = "position: relative; height: 350px;",
                  role = "img",
                  `aria-label` = "Søylediagram som viser antall boliger per år og bygningstype for valgt region og periode.",
                  plotlyOutput("plot_boliger", height = "100%"),
                  tags$button(
                    type = "button",
                    class = "btn btn-default btn-xs fullscreen-btn",
                    onclick = "toggleFullscreen('plot_boliger_container');",
                    title = "Vis plottet i fullskjerm",
                    icon("expand")
                  )
                ),
                tags$small(
                  class = "sr-only",
                  "Diagrammet viser antall boliger per år for den valgte regionen og årsspennet du har valgt i filtrene."
                )
              )
            ),
            
            tabPanel(
              title = "Endring fra år til år",
              box(
                title = "Endring fra år til år (sum boliger)",
                width = 12,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                div(
                  id = "plot_endring_container",
                  style = "position: relative; height: 350px;",
                  role = "img",
                  `aria-label` = "Linje-/søylediagram som viser endring i totalt antall boliger fra år til år for valgt region og periode.",
                  plotlyOutput("plot_endring", height = "100%"),
                  tags$button(
                    type = "button",
                    class = "btn btn-default btn-xs fullscreen-btn",
                    onclick = "toggleFullscreen('plot_endring_container');",
                    title = "Vis plottet i fullskjerm",
                    icon("expand")
                  )
                ),
                tags$small(
                  class = "sr-only",
                  "Diagrammet viser hvor mye totalt antall boliger har økt eller gått ned fra år til år i den valgte regionen."
                )
              )
            ),
            
            tabPanel(
              title = "Filtrerte data",
              fluidRow(
                box(
                  title = "Filtrerte data",
                  width = 8,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tags$small(
                    class = "sr-only",
                    "Tabell som viser alle rader fra SSB-tabell 06265 filtrert på valgt region, bygningstype og år."
                  ),
                  DTOutput("tabell_boliger")
                ),
                box(
                  title = "Eksport",
                  width = 4,
                  status = "info",
                  solidHeader = TRUE,
                  p("Last ned de filtrerte dataene som CSV:"),
                  downloadButton("download_data", "Last ned filtrerte data som CSV"),
                  br(), br(),
                  p(icon("filter"), " Gjeldende filtre (region, bygningstype og år) brukes både i tabell, figurer og CSV-fil.")
                )
              )
            )
          )
        )
      )
    )
  )
)

#-------------------------------------------------
# 4. Server
#-------------------------------------------------

server <- function(input, output, session) {
  
  #----------------------------
  # Dynamisk høy-kontrast-CSS
  #----------------------------
  output$theme_css <- renderUI({
    if (isTRUE(input$high_contrast)) {
      tags$style(HTML("
        body, .content-wrapper, .right-side { background: #000000 !important; color: #ffffff !important; }
        .skin-blue .main-header .navbar, .skin-blue .main-header .logo { background-color: #000000 !important; }
        .skin-blue .main-sidebar { background-color: #000000 !important; }
        .box { background: #000000 !important; color: #ffffff !important; }
        .box-header.with-border { border-bottom: 1px solid #ffffff !important; }
        .box-title { color: #ffffff !important; }
        .dataTables_wrapper, table.dataTable { color: #ffffff !important; background: #000000 !important; }
      "))
    }
  })
  
  #----------------------------
  # Filtrert datasett
  #----------------------------
  filtrert_data <- reactive({
    req(input$region, input$bygningstype, input$aar)
    
    df_06265 |>
      filter(
        region == input$region,
        bygningstype %in% input$bygningstype,
        år >= input$aar[1],
        år <= input$aar[2]
      )
  })
  
  #----------------------------
  # KPI-er (siste år + endring)
  #----------------------------
  output$kpi_n_siste <- renderValueBox({
    dat <- filtrert_data()
    
    oppsummert <- dat |>
      group_by(år) |>
      summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") |>
      arrange(år)
    
    if (nrow(oppsummert) == 0) {
      valueBox("–", "Boliger (siste år)", icon = icon("house"), color = "aqua")
    } else {
      siste <- tail(oppsummert, 1)
      valueBox(format(siste$tot, big.mark = " "), paste0("Boliger i ", siste$år), icon = icon("house"), color = "aqua")
    }
  })
  
  output$kpi_endring_abs <- renderValueBox({
    dat <- filtrert_data()
    
    oppsummert <- dat |>
      group_by(år) |>
      summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") |>
      arrange(år)
    
    if (nrow(oppsummert) < 2) {
      valueBox("–", "Endring siste år", icon = icon("arrow-right-arrow-left"), color = "yellow")
    } else {
      siste <- tail(oppsummert, 1)
      nest_siste <- tail(oppsummert, 2)[1, ]
      
      diff_abs <- siste$tot - nest_siste$tot
      lab <- paste0(ifelse(diff_abs >= 0, "+", ""), format(diff_abs, big.mark = " "))
      
      valueBox(
        value = lab,
        subtitle = paste0("Fra ", nest_siste$år, " til ", siste$år),
        icon = icon("arrow-right-arrow-left"),
        color = ifelse(diff_abs >= 0, "green", "red")
      )
    }
  })
  
  output$kpi_endring_pct <- renderValueBox({
    dat <- filtrert_data()
    
    oppsummert <- dat |>
      group_by(år) |>
      summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") |>
      arrange(år)
    
    if (nrow(oppsummert) < 2 || oppsummert$tot[nrow(oppsummert) - 1] == 0) {
      valueBox("–", "Endring siste år (%)", icon = icon("percent"), color = "yellow")
    } else {
      siste <- tail(oppsummert, 1)
      nest_siste <- tail(oppsummert, 2)[1, ]
      
      diff_pct <- 100 * (siste$tot - nest_siste$tot) / nest_siste$tot
      lab <- paste0(ifelse(diff_pct >= 0, "+", ""), round(diff_pct, 1), " %")
      
      valueBox(
        value = lab,
        subtitle = paste0("Endring fra ", nest_siste$år, " til ", siste$år),
        icon = icon("percent"),
        color = ifelse(diff_pct >= 0, "green", "red")
      )
    }
  })
  
  #----------------------------
  # Plot: boliger per år
  #----------------------------
  output$plot_boliger <- renderPlotly({
    dat <- filtrert_data()
    
    txt_size <- if (isTRUE(input$high_contrast)) 16 else 12
    axis_col <- if (isTRUE(input$high_contrast)) "#ffffff" else "#111827"
    bg_col   <- if (isTRUE(input$high_contrast)) "#000000" else "#ffffff"
    grid_col <- if (isTRUE(input$high_contrast)) "#374151" else "#e5e7eb"
    
    p <- ggplot(dat, aes(
      x = factor(år), y = value, fill = bygningstype,
      text = paste0(
        "Aar: ", år, "<br>",
        "Bygningstype: ", bygningstype, "<br>",
        "Boliger: ", format(value, big.mark = " ")
      )
    )) +
      geom_col(position = "stack") +
      scale_fill_manual(values = rep(pal_okabe, length.out = length(unique(dat$bygningstype)))) +
      labs(
        x = "Aar",
        y = "Antall boliger",
        title = paste("Antall boliger per aar i", input$region),
        fill = "Bygningstype"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        text = element_text(size = txt_size, colour = axis_col),
        axis.text = element_text(colour = axis_col),
        axis.title = element_text(colour = axis_col),
        plot.title = element_text(colour = axis_col, face = "bold"),
        legend.title = element_text(colour = axis_col),
        legend.text  = element_text(colour = axis_col),
        panel.background = element_rect(fill = bg_col, colour = NA),
        plot.background = element_rect(fill = bg_col, colour = NA),
        panel.grid.major = element_line(colour = grid_col),
        panel.grid.minor = element_line(colour = grid_col)
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        paper_bgcolor = bg_col,
        plot_bgcolor  = bg_col,
        font = list(color = axis_col, size = txt_size)
      )
  })
  
  #----------------------------
  # Plot: endring fra år til år (sum)
  #----------------------------
  output$plot_endring <- renderPlotly({
    dat <- filtrert_data()
    
    oppsummert <- dat |>
      group_by(år) |>
      summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") |>
      arrange(år) |>
      mutate(endring = tot - lag(tot))
    
    txt_size <- if (isTRUE(input$high_contrast)) 16 else 12
    axis_col <- if (isTRUE(input$high_contrast)) "#ffffff" else "#111827"
    bg_col   <- if (isTRUE(input$high_contrast)) "#000000" else "#ffffff"
    grid_col <- if (isTRUE(input$high_contrast)) "#374151" else "#e5e7eb"
    
    p <- ggplot(oppsummert, aes(
      x = factor(år), y = endring,
      text = paste0(
        "Aar: ", år, "<br>",
        "Endring: ", format(endring, big.mark = " ")
      )
    )) +
      geom_col(na.rm = TRUE) +
      labs(
        x = "Aar",
        y = "Endring i antall boliger",
        title = paste("Aar-til-aar-endring i", input$region)
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        text = element_text(size = txt_size, colour = axis_col),
        axis.text = element_text(colour = axis_col),
        axis.title = element_text(colour = axis_col),
        plot.title = element_text(colour = axis_col, face = "bold"),
        panel.background = element_rect(fill = bg_col, colour = NA),
        plot.background = element_rect(fill = bg_col, colour = NA),
        panel.grid.major = element_line(colour = grid_col),
        panel.grid.minor = element_line(colour = grid_col)
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        paper_bgcolor = bg_col,
        plot_bgcolor  = bg_col,
        font = list(color = axis_col, size = txt_size)
      )
  })
  
  #----------------------------
  # Tabell + nedlasting
  #----------------------------
  output$tabell_boliger <- renderDT({
    dat <- filtrert_data()
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("boliger_", input$region, "_", input$aar[1], "-", input$aar[2], ".csv")
    },
    content = function(file) {
      dat <- filtrert_data()
      write.csv(dat, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

#-------------------------------------------------
# 5. Start appen
#-------------------------------------------------

shinyApp(ui = ui, server = server)
