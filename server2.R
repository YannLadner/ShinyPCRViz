

# Read in excel file ------------------------------------------------------

df_PCR <- reactive({
  
  
  inFile <- input$file1

  if(is.null(inFile))
    return(NULL)

  df_PCR <- readxl::read_excel(path = inFile$datapath) %>%
    # df_PCR <- readxl::read_excel(path = "C:/Private Files/yladner/Downloads/Template_ShinyPCRViz.xlsx") %>%
    mutate(
      across(.cols = last_col(), ~as.numeric(.x)),
      across(.cols = -last_col(), ~as.factor(.x)),
      across(.cols = -last_col(), ~fct_relevel(.x, gtools::mixedsort)))
  
  
  # (deprecated) or if I want the people to define their own variables 
  {
    # column_types <- read_excel("C:/Private Files/yladner/Downloads/Template_ShinyPCRViz - Copy.xlsx",
    #                            range = "A2:E2", col_names = F) %>% 
    #   unlist %>% as.vector()
    # 
    # 
    # 
    # df_PCR_test <- read_excel("C:/Private Files/yladner/Downloads/Template_ShinyPCRViz - Copy.xlsx") %>% 
    #   .[-1, ] 
    # 
    # for (i in 1:length(column_types)){
    #   
    #   var_name <- names(df_PCR_test[,i])
    #   
    #   if (column_types[i] == "categorical"){
    #     
    #     df_PCR_test <- df_PCR_test %>% 
    #       mutate(
    #         "{var_name}" := as.factor(!!as.symbol(var_name)) %>% 
    #           fct_relevel(., gtools::mixedsort)
    #         # "{var_name}" := as.factor(.data[[,1]])
    #       )
    #   } else {
    #     
    #     df_PCR_test <- df_PCR_test %>% 
    #       mutate(
    #         "{var_name}" := as.numeric(!!as.symbol(var_name))
    #         # "{var_name}" := as.factor(.data[[,1]])
    #       )
    #     
    #   }
    # }
    # df_PCR <- df_PCR_test
    # # df_PCR_test %>% str
    # # df_PCR_test$Condition
    # # 
    # # df_PCR_test$"Condition"
    # # names(df_PCR_test[,1])
    }
  
  return(df_PCR)
})


# 
# eventReactive(df_PCR(),{
#   df_PCR() <- df_PCR()
# }
# )



# Render var_ui -----------------------------------------------------------


output$var_ui <- renderUI({
  req(df_PCR())
  
  varnames <- names(df_PCR())
  
  box( title = "Parameters", width = 3, 
       selectInput('x_var', HTML(paste0('Select <i>', varnames[1], "</i>")), 
                   choices = df_PCR()[[varnames[[1]]]] %>% levels(),
                   selected = df_PCR()[[varnames[[1]]]] %>% levels(), multiple = T),
       # choices = df_PCR()$Condition %>% levels(),
       # selected = df_PCR()$Condition %>% levels(), multiple = T),
       selectInput('color_var', HTML(paste0('Select <i>', varnames[2], "</i>")),
                   choices = df_PCR()[[varnames[[2]]]] %>% levels(),
                   selected = df_PCR()[[varnames[[2]]]] %>% levels(), multiple = T),
       selectInput('facet_var_col', HTML(paste0('Select <i>', varnames[3], "</i>")),
                   choices = df_PCR()[[varnames[[3]]]] %>% levels(),
                   selected = df_PCR()[[varnames[[3]]]] %>% levels(), multiple = T),
       selectInput('facet_var_row', HTML(paste0('Select <i>', varnames[4], "</i>")),
                   choices = df_PCR()[[varnames[[4]]]] %>% levels(),
                   selected = df_PCR()[[varnames[[4]]]] %>% levels() %>% .[1], multiple = T),
  )
})



# Renderplot --------------------------------------------------------------


output$placeholder1 <- renderUI({
  req(input$facet_var_row)
  box(
    # title = paste0("Gene"),
    plotOutput("contents"),
    # status = "primary",
    # solidHeader = TRUE,
    collapsible = F,
    width = 9,
    
    # change also the height in the plotOutput (renderPlot) that belongs to this placeholder
    height = 250 + length(input$facet_var_row) * 75
    # height = "auto"
    
    # height = 700
  )
})

output$contents <- renderPlot(
  height = function() {
  (200 + length(input$facet_var_row) * 75)},
    
    {
  
  if(is.null(df_PCR()))
    return(NULL)
  
  req(input$x_var)
  
  
  varnames <- names(df_PCR())
  
  # browser()
  df_PCR() %>% 
    
    filter(.data[[varnames[[4]]]] %in% input$facet_var_row,
           .data[[varnames[[3]]]] %in% input$facet_var_col,
           .data[[varnames[[1]]]] %in% input$x_var,
           .data[[varnames[[2]]]] %in% input$color_var,
           ) %>%
    # filter(`Gene name` %in% input$facet_var_row,
    #        `Time point` %in% input$facet_var_col,
    #        Condition %in% input$x_var,
    #        Donor %in% input$color_var,
    #        ) %>% 
    ggplot(aes(.data[[varnames[[1]]]], Rq))+
    # geom_point()+
    facet_grid(rows = vars(.data[[varnames[[4]]]]),
               cols = vars(.data[[varnames[[3]]]]),
               # labeller = labeller(`Time point` = label_both),
               scales = "free", space = "free_x"
    ) +
    coord_trans(y = "log10") +
    scale_y_continuous(
    breaks = log_breaks(),
    labels = function(df) prettyNum(df, big.mark = "'", scientific = F), 
    expand = expansion(mult = 0.15))+
    theme(
      axis.text.x =
      #   if(any(df_PCR()$Condition %>% levels() %>% str_length() > 6)){
      #     element_text(angle = 90, vjust = 0.5, hjust = 1)
      #   }
      if(any(input$x_var  %>% str_length() > 6)){
        element_text(angle = 90, vjust = 0.5, hjust = 1)
      } else if(any(input$x_var  %>% length() > 6)){
        element_text(angle = 90, vjust = 0.5, hjust = 1)
      }
      ,
      legend.position = "bottom",
      strip.text.y = element_text(face = "italic")
    ) +
    scale_colour_manual(varnames[2], values=cbPalette)+
    labs(x = "", fill = "") +
    # connecting_line() +
    geom_point(aes(color = .data[[varnames[[2]]]]),
               position = position_jitterdodge(
                 dodge.width = 0.2,
                 jitter.width = 0,
                 jitter.height = 0,
                 seed = 12
               ),
               size = 3
    ) +
  # stat_summary(fun = mean, geom = "line", aes(group = 1),
  #             alpha = 0.1,
  #      size = 5, )+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    alpha = 0.2,
    stroke = 3,
    size = 5,
    aes(fill = "Overall mean")
  ) 
  
  
  
  
})



# Renderplot2 --------------------------------------------------------------


output$placeholder2 <- renderUI({
  
  
  message("placeholder2")
  # req(input$facet_var_row_sel)
  # # req(input$facet_var_col_sel)
  # req(input$x_var_fil)
  # req(input$x_var_sel)
  # req(x_var_fil$
  
  
  box(
    # title = paste0("Gene"),
    plotOutput("contents2"),
    # status = "primary",
    # solidHeader = TRUE,
    collapsible = F,
    width = 9,
    
    # change also the height in the plotOutput (renderPlot) that belongs to this placeholder
    height = 350 + length(input$facet_var_row_fil) * 75
    # height = "auto"
    
    # height = 700
  )
})

output$contents2 <- renderPlot(
  height = function() {
  (300 + length(input$facet_var_row_fil) * 75)},
    
    {
  
  #     
  # if(is.null(df_PCR()))
  #   return(NULL)
  # 
  #     req(nrow(df_PCR()) > 0)  
      shiny::validate(
        need((input$facet_var_row_fil != input$facet_var_col_fil), "Facet row and facet column variables cannot be the same!")
        
      )
      
      
  if(is.null(df_new()))
    return(NULL)
  
      req(nrow(df_new()) > 0)  
  
      # validate(
      #   need(input$x_var_fil != "", message = "test")
      # )
      
      
      # shiny::validate(
      #   need(input$facet_var_row_sel == input$facet_var_col_sel, "Row and column variable cannot be the same")
      #   
      # )
      
      
      # req(new_df_PCR)
      df_new() %>%
      # df_PCR() %>%
    
      # filter for GO terms
      # filter(
      #   if (!is.null(input$GO_scatter2_select)){
      #     `Gene name` %in% 
      #       (GO_terms() %>%
      #       filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`)
      #   }else{
      #     !`Gene name` %in% ""
      #       
      #   }
      # ) %>% 
        
        
    # filter(
    #   # `Gene name` %in% input$facet_var_row_sel,
    #        # `Time point` %in% input$facet_var_col_sel,
           # !!as.symbol(input$x_var_sel) %in% input$x_var_fil
    #        # Donor %in% input$color_var_sel,
    #        ) %>%
    ggplot(aes(!!as.symbol(input$x_var_sel), Rq))+
    # geom_point()+
    facet_grid(
               row =  if (input$facet_var_row_sel == "No row facet"){
                 vars(NULL)
                 } else {
                   vars(!!as.symbol(input$facet_var_row_sel))
               },
               cols =  if (input$facet_var_col_sel == "No column facet"){
                 vars(NULL)
                 } else {
                   vars(!!as.symbol(input$facet_var_col_sel))
               },
               # cols = vars(!!as.symbol(input$facet_var_col_sel)),
               # labeller = labeller(`Time point` = label_both),
               # labeller = labeller(.cols = label_both),
               # labeller = labeller(vars(!!as.symbol(input$facet_var_col_sel)) == label_both),
               scales = "free", space = "free_x"
    ) +
    coord_trans(y = "log10") +
    scale_y_continuous(
    breaks = log_breaks(),
    labels = function(df) prettyNum(df, big.mark = "'", scientific = F), 
    expand = expansion(mult = 0.15))+
    theme(
      axis.text.x = 
        if(any(input$x_var_fil  %>% str_length() > 6)){
          element_text(angle = 90, vjust = 0.5, hjust = 1)
        } else if(any(input$x_var_fil  %>% length() > 6)){
        element_text(angle = 90, vjust = 0.5, hjust = 1)
      }
        # if(any(df_new()$Condition %>% levels() %>% str_length() > 6)){
        #   element_text(angle = 90, vjust = 0.5, hjust = 1)
        # }
      ,
      legend.position = "bottom",
      strip.text.y = element_text(face = "italic")
    ) +
    scale_colour_manual(input$color_var_sel, values=cbPalette)+
    # connecting_line() +
    geom_point(aes(color = !!as.symbol(input$color_var_sel)),
               position = position_jitterdodge(
                 dodge.width = 0.2,
                 jitter.width = 0,
                 jitter.height = 0,
                 seed = 12
               ),
               size = 3
    ) +
  # stat_summary(fun = mean, geom = "line", aes(group = 1),
  #             alpha = 0.1,
  #      size = 5, )+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    alpha = 0.2,
    stroke = 3,
    size = 5,
    aes(fill = "Overall mean")
  ) +
    labs(x = "", fill = "") 
  
  
      
      
})



# Render parameters -------------------------------------------------------





# Render var_ui2 -----------------------------------------------------------



# To make the reactive filters react to the selection, one needs the following:
# 1. The different selectInput() for the selection (ending in "_sel") and also 
# the filter (ending in "_fil").
# For the filters, one just can write anything, since this will be overwritten
# by the updateSelectInputs() within the observeEvents() and then the subsequent
# eventReactive()
# 2. The previously mentioned observeEvents() that will update the selections
# and choices.
# 3. The eventReactive() that will result in a new data frame called df_new()
# 4. The df_new() will have to be the one that will be needed for the plots in 
# output$contents2


# NOT NEEDED ANYMORE, SINCE SELECTINPUT HAD TO BE SERVERSIDE, OTHERWISE PROBLEMS
# WITH SWITCHING BETWEEN FILES AFTER FILE UPLOAD
{
# observeEvent(input$file1, {
# output$var_ui2 <- renderUI({
#   req(df_PCR())
#   
#   message("does it work?")
#   # req(input$x_var_sel)
#   box( title = "Parameters", width = 3, 
#   # selectInput('x_var_sel', 'Select x variable', 
#   #             choices = df_PCR() %>% select(where(is.factor)) %>% names,
#   #             selected = df_PCR() %>% select(where(is.factor)) %>% names %>% .[1]),
#   # selectInput('x_var_fil', NULL,
#   #             # choices = df_PCR()[!!get(input$x_var_sel)],
#   #             choices = "OC", selected = "OC",
#   #             # choices = df_PCR()[input$x_var_sel], selected = df_PCR()[input$x_var_sel] %>% unlist,
#   #             # selected = input$x_var_sel,
#   #             multiple = T),
#   #             # selected = df_PCR()[, input$x_var_sel] %>% unique , multiple = T),
#   # selectInput('color_var_sel', 'Select color variable', 
#   #             choices = df_PCR() %>% select(where(is.factor)) %>% names,
#   #             selected = df_PCR() %>% select(where(is.factor)) %>% names %>% .[2]),
#   
#   
#   # selectInput('facet_var_col_sel', 'Select facet column variable', 
#   #             choices = c(df_PCR() %>% select(where(is.factor)) %>% names, "No column facet"),
#   #             selected = df_PCR() %>% select(where(is.factor)) %>% names %>% .[3]),
#   # selectInput('facet_var_col_fil', NULL,
#   #             # choices = df_PCR()[!!get(input$x_var_sel)],
#   #             # choices = "1",
#   #             selected = "1",
#   #             choices = df_PCR()[input$facet_var_col_sel], 
#   #             # selected = df_PCR()[input$facet_var_col_sel] %>% unlist,
#   #             # selected =  df_PCR() %>% dplyr::select(.data[[input$facet_var_col_sel]]),
#   #             # selected = input$x_var_sel,
#   #             multiple = T),
#   
#   
#   # selectInput('facet_var_row_sel', 'Select facet row variable', 
#   #             choices = c(df_PCR() %>% select(where(is.factor)) %>% names, "No row facet"),
#   #             selected = df_PCR() %>% select(where(is.factor)) %>% names %>% .[4]),
#   # selectInput('facet_var_row_fil', NULL,
#   #             # choices = df_PCR()[!!get(input$x_var_sel)],
#   #             choices = "1", selected = "1",
#   #             # choices = df_PCR()[input$facet_var_row_sel], selected = df_PCR()[input$facet_var_row_sel] %>% unlist,
#   #             # selected = input$x_var_sel,
#   #             multiple = T),
#   )
# })
# })
}


observeEvent(df_PCR(), {
  req(df_PCR())
  choices <- df_PCR() %>% select(where(is.factor)) %>% names
  updateSelectInput(inputId = "x_var_sel", choices = choices) 
})

observeEvent(df_PCR(), {
  req(df_PCR())
  choices <- df_PCR() %>% select(where(is.factor)) %>% names
  selected <- choices[2]
  updateSelectInput(inputId = "color_var_sel", choices = choices, selected = selected) 
})



observeEvent(c(input$x_var_sel, df_PCR(), input$GO_scatter2_select), {
  req(df_PCR())
  req(input$x_var_sel)
  choices <- df_PCR() %>% 
  filter(
    if (!is.null(input$GO_scatter2_select)){
      `Gene name` %in% 
        (GO_terms() %>%
           filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`)
    }else{
      !`Gene name` %in% ""
      
    }
  ) %>% .[input$x_var_sel]
  selected <- df_PCR()[input$x_var_sel] %>% pull()
  updateSelectInput(inputId = "x_var_fil", choices = choices, selected = selected)
})


observeEvent(df_PCR(), {
  req(df_PCR())
  choices <- df_PCR() %>% select(where(is.factor)) %>% names
  selected <- choices[3]
  updateSelectInput(inputId = "facet_var_col_sel", choices = c(choices, "No column facet"), selected = selected) 
})

observeEvent(c(input$facet_var_col_sel, df_PCR(), input$GO_scatter2_select), {
  
  if (input$facet_var_col_sel != "No column facet"){
  req(df_PCR())
  req(input$facet_var_col_sel)
  choices <- df_PCR() %>% 
  filter(
    if (!is.null(input$GO_scatter2_select)){
      `Gene name` %in% 
        (GO_terms() %>%
           filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`)
    }else{
      !`Gene name` %in% ""
      
    }
  ) %>% .[input$facet_var_col_sel]
  selected <- df_PCR()[input$facet_var_col_sel] %>% pull()
  updateSelectInput(inputId = "facet_var_col_fil", choices = choices, selected = selected)
  }
})


observeEvent(df_PCR(), {
  req(df_PCR())
  choices <- df_PCR() %>% select(where(is.factor)) %>% names
  selected <- choices[4]
  updateSelectInput(inputId = "facet_var_row_sel", choices = c(choices, "No row facet"), selected = selected) 
})

observeEvent(c(input$facet_var_row_sel, df_PCR(), input$GO_scatter2_select), {
  
  if (input$facet_var_row_sel != "No row facet"){
  req(df_PCR())
  req(input$facet_var_row_sel)
  choices <- df_PCR() %>% 
    filter(
      if (!is.null(input$GO_scatter2_select)){
        `Gene name` %in% 
          (GO_terms() %>%
             filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`)
      }else{
        !`Gene name` %in% ""
        
      }
    ) %>% .[input$facet_var_row_sel]
  selected <- df_PCR()[input$facet_var_row_sel] %>% pull()
  updateSelectInput(inputId = "facet_var_row_fil", choices = choices, selected = selected)
  }
})


# NOT NEEDED ANYMORE, SINCE SELECTINPUT HAD TO BE SERVERSIDE, OTHERWISE PROBLEMS
# WITH SWITCHING BETWEEN FILES AFTER FILE UPLOAD
{
# observeEvent(c(input$x_var_sel), {
#   # will update too fast for ggplot and throw out errors, so I just suppressed all errors
#   # message("Table event observed")
#   # selected <- df_PCR()[input$x_var_sel] %>% pull()
#   selected <- df_PCR()[input$x_var_sel] %>% pull()
#   choices <- df_PCR()[input$x_var_sel] %>% pull()
#   updateSelectInput(session, "x_var_fil", choices = choices, selected = selected)
# }, priority = 3)

# 
# observeEvent(input$facet_var_row_sel, {
#   # will update too fast for ggplot and throw out errors, so I just suppressed all errors
# 
#   if (input$facet_var_row_sel == "No row facet"){
#     # return(NULL)
#     message("No row facet")
#     
#   } else {
#   
#   selected <- df_PCR()[input$facet_var_row_sel] %>% pull()
#   choices <- df_PCR()[input$facet_var_row_sel] %>% pull()
#   updateSelectInput(session, "facet_var_row_fil", choices = choices, selected = selected )
#   }
# }, priority = 2)


# observeEvent(input$facet_var_col_sel, {
#   # message("Table event observed")
#   
#   if (input$facet_var_col_sel == "No column facet"){
#     # return(NULL)
#   message("No column facet")
#     
#   } else {
#   # selected <- df_PCR()[input$facet_var_col_sel] %>% pull()
#   selected <- df_PCR()[input$facet_var_col_sel] %>% pull()
#   choices <- df_PCR()[input$facet_var_col_sel] %>% pull()
#   updateSelectInput(session, "facet_var_col_fil", choices = choices, selected = selected )
#   }
#   
# }, priority = 2)
}



df_new <- eventReactive(c(df_PCR(), 
                          input$x_var_fil, input$facet_var_col_fil, input$facet_var_row_fil), {
  
  
  if (input$facet_var_col_sel == "No column facet" &
      input$facet_var_row_sel != "No row facet"){
    # return(NULL)
    # message("nocol-eR")
  df_PCR() %>%
    filter(!!as.symbol(input$x_var_sel) %in% .env$input$x_var_fil,
           # !!as.symbol(input$facet_var_col_sel) %in% input$facet_var_col_fil,
           !!as.symbol(input$facet_var_row_sel) %in% .env$input$facet_var_row_fil
    )
    
  
    
  } else if (input$facet_var_row_sel == "No row facet" & 
             input$facet_var_col_sel != "No column facet"){
  
    # return(NULL)
    # message("norow-eR")
  df_PCR() %>%
    filter(!!as.symbol(input$x_var_sel) %in% .env$input$x_var_fil,
           !!as.symbol(input$facet_var_col_sel) %in% .env$input$facet_var_col_fil
           # !!as.symbol(input$facet_var_row_sel) %in% input$facet_var_row_fil
    )
    
    
    
  } else if (input$facet_var_col_sel == "No row facet" & 
             input$facet_var_col_sel == "No column facet"){
  
    # return(NULL)
    # message("norow-eR")
  df_PCR() %>%
    filter(!!as.symbol(input$x_var_sel) %in% .env$input$x_var_fil
           # !!as.symbol(input$facet_var_col_sel) %in% input$facet_var_col_fil
           # !!as.symbol(input$facet_var_row_sel) %in% input$facet_var_row_fil
    )
    
    
    
  } else {
    # message("norow-eR1")
  df_PCR() %>%
    filter(!!as.symbol(input$x_var_sel) %in% .env$input$x_var_fil,
           !!as.symbol(input$facet_var_col_sel) %in% .env$input$facet_var_col_fil,
           !!as.symbol(input$facet_var_row_sel) %in% .env$input$facet_var_row_fil
    )
  }
  
  
  
})



# Genemania and genecard links --------------------------------------------



output$URLS_scatter1 <- renderUI({
  
  req(df_PCR())
  req(input$facet_var_row)
  
  
  genefilt <- (df_PCR()$`Gene name` %>% levels())[
    (df_PCR()$`Gene name` %>% levels()) %in% input$facet_var_row]
  
  
  
  url <- paste0(
    
    genefilt
    
    , collapse = "/")
  str1 <- a(HTML(paste0("Link to genemania with selected genes", "<br/>")),
            href = paste("https://genemania.org/search/homo-sapiens", url, sep = "/"), target = "_blank"
  )
  
  strlist <- list()
  for (i in 
       
       genefilt
       
       ) {
    url2 <- paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", i)
    str2 <- a(HTML(paste0("Link to genecard: ", i, "<br/>")),
              href = paste0(url2),
              target = "_blank"
    )
    
    strlist[[length(strlist) + 1]] <- str2
  }
  
  # tagList(str1, strlist)
  
  box(
    #   
      # HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i>"),
      solidHeader = F,
      width = 4, HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i><br>"), tagList(str1, strlist)
    )
  
  
 
})



output$URLS_scatter2 <- renderUI({
  
  req(df_new())
  req(input$x_var_fil)
  req(input$facet_var_col_fil)
  req(input$facet_var_row_fil)
  
  
  genefilt <- 
    (df_new()$`Gene name` %>% levels())[
      (df_new()$`Gene name` %>% levels()) %in% input$x_var_fil |
        (df_new()$`Gene name` %>% levels()) %in% input$facet_var_col_fil |
        (df_new()$`Gene name` %>% levels()) %in% input$facet_var_row_fil]
  
  
  url <- paste0(
    
    genefilt
    
    , collapse = "/")
  str1 <- a(HTML(paste0("Link to genemania with selected genes", "<br/>")),
            href = paste("https://genemania.org/search/homo-sapiens", url, sep = "/"), target = "_blank"
  )
  
  strlist <- list()
  for (i in 
       
       genefilt
       
       ) {
    url2 <- paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", i)
    str2 <- a(HTML(paste0("Link to genecard: ", i, "<br/>")),
              href = paste0(url2),
              target = "_blank"
    )
    
    strlist[[length(strlist) + 1]] <- str2
  }
  
  # tagList(str1, strlist)
  box(
    #   
    # HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i>"),
    solidHeader = F,
    width = 4, HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i><br>"), tagList(str1, strlist)
  )
 
})




# Go terms ----------------------------------------------------------------



GO_terms <- eventReactive(input$actbutt ,{
  req(df_PCR())
  
  withProgress(message = 'Establishing connection to Ensembl',
               detail = 'This may take a while...', value = 0, {
                 # for (i in 1:15) {
                 #   incProgress(1/15)
                 #   Sys.sleep(0.25)
                 # }
  
  mart <- useMart("ENSEMBL_MART_ENSEMBL")
  incProgress(3/15)
  mart <- useDataset("hsapiens_gene_ensembl", mart)
    incProgress(8/15)
  
  GO_df <- getBM(
    mart = mart,
    attributes = c(
      'external_gene_name',
      'name_1006'
    ),
    filter = 'external_gene_name',
    values = df_PCR()$`Gene name` %>% unique,
    uniqueRows = T, ) %>% filter(!name_1006 %in% "")
  
  
  # GO_df <-  data.frame(a = c("ACAN", "COL2A1"), b = c("hey", "WHAT")) # to test
  
  names(GO_df) <- c("Gene name", "GO term")
               })
  
  
  return(GO_df)
  # data.frame(a = c(1, 2), b = c("hey")) # to test
  
})



output$GO_scatter2 <- renderDataTable({
  
  req(df_PCR())
  req(GO_terms())
  
  
  
  
  GO_terms()
  
})


observeEvent(GO_terms(), {

  
  message("GO terms ready")
  selected <- GO_terms()$`GO term` %>% unique %>% .[1]
  choices <- GO_terms()$`GO term` %>% unique
  updateSelectInput(session, "GO_scatter2_select", choices = choices, selected = selected )

}, priority = 2)


# output$testingthings <- renderDataTable({
# # output$testingthings <- renderText({
#     
#   df_PCR() %>% 
#     filter(
#       `Gene name` %in% (GO_terms() %>%
#                           filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`
#         
#       )
#       
#     ) %>% .$`Gene name` %>% as.data.frame
#   # GO_terms() %>%
#   #   filter(`GO term` %in% input$GO_scatter2_select) %>% .$`Gene name`
# })