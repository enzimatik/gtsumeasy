# package
library(readxl)
library(shiny)

library(gt)
library(gtsummary)
library(dplyr)
library(jsonlite)

library(magrittr)
library(labelled)

library(flextable)

library(tidyr)
library(dplyr)
library(forcats)

fmt_pvalue_with_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0(style_pvalue(x), "***"),
    x < 0.01 ~ paste0(style_pvalue(x), "**"),
    x < 0.05 ~ paste0(style_pvalue(x), "*"),
    TRUE ~ style_pvalue(x)
  )
}

# gtsummary
gtsummarize <- function(input, data) {
  tbl <- NULL

  # do data empty?
  # do variable selected?
  if (!is.null(data) && !is.null(input$Variables)) {

    # do grouping is used?
    if (!is.null(input$Group)) {

      # is it multi grouping?
      if(!is.null(input$Strata)) {
        labels <- c()
        tbl.append <- NULL
        
        tbl.st1 <- data %>%
          select(input$Variables, input$Group, input$Strata) %>%
          mutate(across(everything(), as.factor)) %>%
          mutate(across(everything(), fct_explicit_na, "Unknown")) %>%
          copy_labels_from(data)  %>%
          tbl_strata(
            strata = input$Strata,
            .tbl_fun =
              ~ .x %>%
              tbl_summary(
                by = input$Group,
                digits = list(all_categorical() ~ c(0, 2))
              ) %>%
              modify_header(label ~ "**Variable**") %>%
              add_p(all_categorical() ~ "fisher.test", pvalue_fun = fmt_pvalue_with_stars) %>%
              # add_overall() %>%
              modify_footnote(p.value ~ "Fisher's exact test *p<0.05; **p<0.01; ***p<0.001"),
            .header = "**{strata}**, N = {n}"
            )
        tbl.st2 <- data %>%
          select(input$Variables, input$Group) %>%
          mutate(across(everything(), as.factor)) %>%
          mutate(across(everything(), fct_explicit_na, "Unknown")) %>%
          copy_labels_from(data)  %>%
          tbl_summary(
            by = input$Group,
            digits = list(all_categorical() ~ c(0, 2))
          ) %>%
          modify_header(label ~ "**Variable**") %>%
          add_p(all_categorical() ~ "fisher.test", pvalue_fun = fmt_pvalue_with_stars) %>%
          modify_footnote(p.value ~ "Fisher's exact test *p<0.05; **p<0.01; ***p<0.001")
        tbl <- tbl_merge(
          tbls = list(tbl.st2, tbl.st1), tab_spanner = NULL
        )
      }
      else{
        if (length(input$Group) > 1) {
          labels <- c()
          tbl.append <- NULL
  
          # loop multi grouping
          for (key in input$Group) {
            tbl.temp <- data %>%
              select(input$Variables, key) %>%
              mutate(across(everything(), as.factor)) %>%
              mutate(across(everything(), fct_explicit_na, "Unknown")) %>%
              copy_labels_from(data)  %>%
              tbl_summary(
                by = key,
                digits = list(all_categorical() ~ c(0, 2))
              ) %>%
              add_p(all_categorical() ~ "fisher.test", pvalue_fun = fmt_pvalue_with_stars) %>%
              add_overall() %>%
              modify_footnote(p.value ~ "Fisher's exact test *p<0.05; **p<0.01; ***p<0.001")
  
            labels <- append(labels, key)
            tbl.append <- append(tbl.append, list(tbl.temp))
          }
          tbl <- tbl_merge(
            tbls = tbl.append,
            tab_spanner = unlist(labels)
          )
        }
  
        # single grouping
        else {
          tbl <- data %>%
            select(input$Variables, input$Group) %>%
            mutate(across(everything(), as.factor)) %>%
            mutate(across(everything(), fct_explicit_na, "Unknown")) %>%
            copy_labels_from(data)  %>%
            tbl_summary(
              by = input$Group,
              digits = list(all_categorical() ~ c(0, 2))
            ) %>%
            modify_header(label ~ "**Variable**") %>%
            add_p(all_categorical() ~ "fisher.test", pvalue_fun = fmt_pvalue_with_stars) %>%
            add_overall() %>%
            modify_footnote(p.value ~ "Fisher's exact test *p<0.05; **p<0.01; ***p<0.001")
        }
      }
    }

    # simple summary
    else {
      tbl <- data %>%
        select(input$Variables, input$Group) %>%
        mutate(across(everything(), as.factor)) %>%
        mutate(across(everything(), fct_explicit_na, "Unknown")) %>%
        copy_labels_from(data)  %>%
        tbl_summary(
          digits = list(all_categorical() ~ c(0, 2))
        )
    }
  }
  return(tbl)
}



# UI PART
ui <- fluidPage(
  titlePanel("gtsumeasy"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        ## SELECT PANEL
        tabPanel(
          "Files",
          fileInput("file", label = "Input", placeholder = "xls or csv files"),
          fileInput("rule", label = "Transformation Rule", placeholder = "JSON files"),
        ),
        tabPanel(
          "Data Clean-up", br(),
          selectInput("varToTransform", "Variable to Transform", ""),
          selectInput("transformType", "Transformation Type",
            choices = list(
              "String to String" = 1,
              "Range" = 2
            ),
          
          ),
          textInput("colname", "Column Name", placeholder = "Column Name"),
          uiOutput("field"),
          br(),
          br(),
          actionButton("transform", label = "Transform"),
          br(),
          br(),
          downloadButton("downloadData", label = "Export Rule as JSON")
        ),
        tabPanel(
          "Analysis",
          selectInput("Variables", "Variables", "", multiple = TRUE),
          # checkboxGroupInput("Variables", "Variables", ""),
          selectInput("Group", "Group By", "", multiple = TRUE),
          selectInput("Strata", "Strata By", "", multiple = TRUE),
          selectInput("select",
            label = "Analysis",
            choices = list(
              "Summary (Fisher Exact)" = 1,
              "Survival (Cox PH)" = 2
            ),
            selected = 1
          ),
          actionButton("action", label = "Analysis"),
          br(),
          br(),
          downloadButton("downloadPlot", label = "Export Plot (.docx)"),
          br(),
          br(),
          actionButton("export", label = "Export Database (.csv)")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        ## SELECT PANEL
        tabPanel(
          "Selected Data", br(),
          verbatimTextOutput("df"),
          verbatimTextOutput("clean")
        ),

        ## TRANSFORM PANEL


        ## PLOT PANEL
        tabPanel("Plot", br(), gt_output("tab"))
      )
    )
  )
)

# SERVER PART
server <- function(input, output, session) {
  # SIDE LAYOUT
  ## DATA
  data.main <- reactiveVal(NULL)
  data.transform <- reactiveVal(NULL)
  counter.string <- reactiveValues(n = 0)
  counter.range <- reactiveValues(n = 0)
  plot.tbl <- reactiveVal(NULL)

  ## LOAD INPUT
  observe({
    file <- input$file
    if (!is.null(file)) {
      ### CHECK INPUT FILE TYPE
      dat <- read_excel(file$datapath, sheet = 1)
      data.main(dat)
    }
  })
  ## FILL VARIABLE OPTIONS
  observe({
    updateSelectInput(session,
      "Variables",
      label = "Variables",
      choices = colnames(data.main())
    )
  })

  ## FILL GROUP BY OPTIONS
  observe({
    updateSelectInput(session,
      "Group",
      label = "Group By",
      choices = colnames(data.main())
    )
  })
  
  observe({
    updateSelectInput(session,
      "Strata",
      label = "Strata By",
      choices = colnames(data.main())
    )
  })
  
  ## GET SELECTED DB
  dataSelected <- reactive({
    data.main()[, c(input$Variables, input$Group)]
  })
  ## SHOW SELECTED DB
  output$df <- renderPrint({
    dataSelected()
  })
  output$clean <- renderPrint({
    unique(dataSelected())
  })
  ## FILL VARIABLE OPTIONS FOR TRANSFORMATION
  observe({
    updateSelectInput(session,
      "varToTransform",
      label = "Variable to Transform",
      choices = colnames(data.main())
    )
  })

  ## SET TRANSFORM TYPE
  observeEvent(input$transformType, {
    counter.string$n <- 0
    counter.range$n <- 0
    removeUI(
      selector = "#field div"
    )
    if (input$transformType == "1") {
      output$field <- renderUI({
        tags$span(
          actionButton("addString", label = "Add String"),
          actionButton("rmString", label = "Remove String")
        )
      })
    } else {
      output$field <- renderUI({
        tags$span(
          actionButton("addRange", label = "Add Range"),
          actionButton("rmRange", label = "Remove Range")
        )
      })
    }
  })

  ## ADD STRING
  observeEvent(input$addString, {
    counter.string$n <- counter.string$n + 1
    id <- counter.string$n
    remove_id <- paste0("remove_", id)
    div_id <- paste0("div_", id)

    insertUI(
      selector = "#addString",
      where = "beforeBegin",
      ui = tags$div(
        id = div_id,
        fluidRow(
          column(
            4,
            textInput(paste0("input_", id), label = NULL, placeholder = "Input")
          ),
          column(
            4,
            textInput(paste0("output_", id), label = NULL, placeholder = "Output")
          ),
          ## still not sure the benefit
          # column(2,
          #        div(style = "text-align:center;",
          #            actionButton(remove_id, "Remove"))
          # )
        )
      )
    )

    ## RM STRING
    observeEvent(input[[remove_id]], {
      removeUI(
        selector = paste0("#", div_id)
      )
    })
  })

  ## RM STRING LAST
  observeEvent(input$rmString, {
    removeUI(
      selector = paste0("#div_", counter.string$n)
    )
    counter.string$n <- counter.string$n - 1
  })

  ## ADD RANGE
  observeEvent(input$addRange, {
    counter.range$n <- counter.range$n + 1
    id <- counter.range$n
    remove_id <- paste0("remove_", id)
    div_id <- paste0("div_", id)

    insertUI(
      selector = "#addRange",
      where = "beforeBegin",
      ui = tags$div(
        id = div_id,
        textInput("cut_", label = NULL, placeholder = "Cut"),
        textInput("input_", label = NULL, placeholder = "Label")
      )
    )
  })

  ## RM RANGE LAST
  observeEvent(input$rmRange, {
    removeUI(
      selector = paste0("#div_", counter.range$n)
    )
    counter.range$n <- counter.range$n - 1
  })


  ## READ TRANSFORM INPUT
  observeEvent(input$transform, {
    temp <- NULL
    temp$id <- unbox(input$varToTransform)
    temp$colname <- unbox(input$colname)
    if (input$transformType == "1") {
      temp$type <- unbox("string")
      for (i in 1:counter.string$n) {
        # print(typeof(input[[paste0("input_", i)]]))
        
        if(length(input[[paste0("input_", i)]]) > 0){
          temp$input <- append(temp$input, input[[paste0("input_", i)]])
          temp$output <- append(temp$output, input[[paste0("output_", i)]])
        }
      }
    } else if (input$transformType == "2") {
      temp$type <- unbox("range")
      # print(typeof(input$cut_))
      if(length(input$cut_) > 0){
        temp$cut <- input$cut_
        temp$input <- input$input_
      }
    }
    data.transform(append(data.transform(), list(temp))) 
    print('setting transform')
    print(tail(data.transform(), n=1))
    # exp(data.transform())
    applytransform()
  })

  ## APPLY TRANSFORM
  applytransform <- function(x) {
    data.temp <- data.main()
    for (rule in data.transform()) {
      id <- toString(rule["id"])
      type <- toString(rule["type"])
      
      if(id %in% colnames(data.temp)){
        
        colname <- paste(toString(rule["colname"]), "_coded", sep = "")
        data.temp[[colname]] <- data.temp[[id]]
        var_label(data.temp[[colname]]) <- toString(rule["colname"])
        coded <- data.temp[[colname]]
        
        if (type == "string") {
          input_ <- rule[["input"]]
          output_ <- rule[["output"]]
          
          if(colname %in% colnames(data.temp)){
  
            for (key in 1:length(input_)) {
              if(length(input_) > 0){
                coded <- gsub(paste("(?i)", input_[[key]], sep = ""), output_[[key]], coded)
              }
            }
            print(coded)
            data.temp[[colname]] <- coded
          }
        } else if(type == "range") {
          cut_ <- rule[["cut"]]
          input_ <- rule[["input"]]
          
          coded <- as.numeric(data.temp[[colname]])
          if(length(cut_) > 0){
            coded <- cut(coded,
             breaks = as.numeric(unlist(strsplit(cut_, split = ","))),
             labels = unlist(strsplit(input_, split = ","))
            )
          }
          print(coded)
          data.temp[[colname]] <- coded
        }
      }
    }
    data.main(data.temp)
  }

  ## EXPORT
  # exp <- function(data){
    output$downloadData <- downloadHandler(
      
      filename = function() {
        paste("data", sep = "")
      },
      content = function(con) {
        print('exporting transform')
        print(tail(data.transform(), n=1))
        saveRDS(data.transform(), con)
      }
    )
  # }

  ## LOAD RULE
  observeEvent(input$rule, {
    file <- input$rule
    if (!is.null(file)) {
      dat <- readRDS(file$datapath)
      data.transform(dat)
      applytransform()
    }
  })


  ## ANALYSIS
  observeEvent(input$action, {
    tbl <- gtsummarize(input, data.main())

    plot.tbl(tbl)
    output$tab <- render_gt(
      expr = plot.tbl() %>% as_gt(),
      height = px(1024),
      width = px(1024)
    )
  })

  # EXPORT
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot.docx", sep = "")
    },
    content = function(con) {
      plot.tbl() %>%
        as_flex_table() %>%
        flextable::save_as_docx(path = con)
    }
  )
}

shinyApp(ui, server)
