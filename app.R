#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages ---------------------------------------------------------------
library(shiny)
### library(shinydashboard)
### library(shinyWidgets)
library(rhandsontable)
library(tidyverse)
library(ggplot2)
library(stringr)
library(vroom)
source("R/rhandsonscript_options.R")
source("R/grade_values.R")

# create tabs for conditional UI ----------------------------------------------
parameter_tabs <- tagList(
    tags$style("#params { display:none; }"),
    tabsetPanel(id = "params",
                tabPanel("Example"),
                tabPanel("Blank"),
                tabPanel("Upload",
                         fileInput("file", NULL, accept = c(".csv"))
                         )
                )
)

download_tab <- tagList(
    tags$style("#dlbutton { display:none; }"),
    tabsetPanel(id = "dlbutton",
                tabPanel("nodownload"),
                tabPanel("download",
                         div(
                             downloadButton("dl",
                                            label = "Download Data",
                                            icon = icon("download"),
                                            style = "width: 100%;"),
                             style = "padding-top: 18px;"
                         )
                )
    )
)

# define UI for the application -----------------------------------------------
ui <- fluidPage(
    includeCSS("www/styles.css"),
    ### useShinydashboard(),
    # modify tabpanels --------------------------------------------------------
    tags$style(
        # make the active tabpanel's label text bold
        HTML("
        .tabbable > .nav > li[class=active] > a {font-weight: bold;}
        "),
        # float 4th tabpanel to the far right
        HTML("
             .tabbable ul li:nth-child(4) { float: right; }
        "),
        HTML("
            .tabbable .nav > li > a[data-value='Donate']
             {color: black; background-color: #ffd700;
             border-color: #e3e3e3; border-bottom-color: white;}
        ")
    ),
    ### possible themes: readable, simplex, spacelab
    ### theme = shinythemes::shinytheme("yeti"),
    # create title panel ------------------------------------------------------
    titlePanel(title = "GPA Calculator"),
    # create sidebar ----------------------------------------------------------
    sidebarLayout(
        sidebarPanel(
            # gpa scale selector
            selectInput("gpa_scale",
                        label = "Select GPA Scale",
                        choices = c("4.0", "4.33", "Custom")
                        ),
            # show gpa scale legend checkbox
            checkboxInput("show_legend", label = "Show GPA Scale Legend",
                          FALSE),
            # conditional gpa scale legend
            conditionalPanel("input.show_legend == 1 ||
                             input.gpa_scale == 'Custom'",
                             p("Note: values are editable when set to Custom."),
                             rHandsontableOutput("gpa_legend")
            ),
            # data input selector
            div(
                selectInput("selector",
                            "Select Data",
                            choices = c("Example", "Blank", "Upload")),
                style = "padding-top: 12px;"
            ),
            # conditional UI for file upload
            parameter_tabs,
            # subject filter input
            div(
                textInput("filter_subject",
                          label = "Filter by Subject"),
                style = "padding-top: 12px;"
            ),
            # date range header
            div(strong("Filter by Date Range"),
                style = "margin-top: 6px;
                         margin-bottom: 6px;
                         font-size: 14px;
                         user-select: none;"
            ),
            # date range inputs
            splitLayout(style = "padding-right: 7px;",
                cellWidths = c("44%", "12%", "44%"),
                textInput("date_min",
                          label = NULL,
                          placeholder = "Term Year"),
                div(strong("to"),
                    style = "text-align: center;
                             padding: 6px 0;
                             margin: auto;
                             vertical-align: middle;"),
                textInput("date_max",
                          label = NULL,
                          placeholder = "Term Year")
            ),
            # calculate button
            div(
                actionButton("calculate",
                             label = "Calculate GPA",
                             icon = icon("calculator"),
                             width = "100%"),
                style = "padding-top: 24px;"
            ),
            # conditional download button
            download_tab,
            developed_by()
        ),
    # create main panel -------------------------------------------------------
    mainPanel(
        tabsetPanel(id = "tabset",
            tabPanel("Input",
                     div(
                         rHandsontableOutput("transcript"),
                         style = "padding-top: 12px;"
                     )
                     ),
            tabPanel("Output",
                     fluidRow(
                         column(width = 6,
                                div(
                                    htmlOutput("result"),
                                    style = "padding-top: 12px;"
                                )
                         ),
                         column(width = 3,
                                div(
                                    htmlOutput("credits"),
                                    style = "padding-top: 12px;"
                                )
                         ),
                         column(width = 3,
                                div(
                                    htmlOutput("grade_points"),
                                    style = "padding-top: 12px;"
                                ),
                         )
                     ),
                     fluidRow(
                         div(
                             selectInput("graph",
                                         label = "Change Graph",
                                         choices = c("Cumulative GPA",
                                                     "Grade Totals",
                                                     "Credit Totals")),
                             style = "padding-left: 16px; margin-bottom: -11px;"
                         ),
                     ),
                     fluidRow(
                         conditionalPanel("input.graph == 'Cumulative GPA'",
                                          div(
                                              plotOutput("cumgpa",
                                                         height = plot_height_line),
                                              style = plot_padding_line
                                          )
                         ),
                         conditionalPanel("input.graph == 'Grade Totals'",
                                          div(
                                              plotOutput("grades",
                                                         height = plot_height_bar),
                                              style = plot_padding_bar
                                          )
                         ),
                         conditionalPanel("input.graph == 'Credit Totals'",
                                          div(
                                              plotOutput("credit_totals",
                                                         height = plot_height_bar),
                                              style = plot_padding_bar
                                          )
                         ),
                     )
                     ),
            tabPanel("About",
                    withMathJax(includeMarkdown("RMarkdown/about.md")),
            ),
            tabPanel("Donate",
                     htmlOutput("donate")
            )
        )
        )
    )
)
# define server logic required to calculate gpa -------------------------------
server <- function(input, output, session) {

    # create reactive gpa scale vector based on the selected gpa scale --------
    gpa_scale <- eventReactive({
        input$gpa_scale
        input$show_legend}, {
        if (input$gpa_scale == "4.0") {
            return(gpa_4.0)
        } else if (input$gpa_scale == "4.33") {
            return(gpa_4.33)
        } else if (input$gpa_scale == "Custom") {
            return(Custom)
        }
    })
    
    # create hot gpa scale legend dataframe -----------------------------------
    hot_gpa_scale_legend <- eventReactive({
        input$gpa_scale
        input$show_legend}, {
        tibble("Grade" = grades, "Value" = as.numeric(gpa_scale()))
    })
    
    # render the gpa scale legend ---------------------------------------------
    output$gpa_legend <- renderRHandsontable({
        outputOptions(output, "gpa_legend", suspendWhenHidden = FALSE)
        if (input$gpa_scale != "Custom") {
            rhandsontable(hot_gpa_scale_legend(),
                          rowHeaders = NULL,
                          stretchH = "all",
                          overflow = "visible") %>%
                hot_col(col = "Grade", readOnly = TRUE) %>%
                hot_col(col = "Value", readOnly = TRUE)
        } else {
            rhandsontable(gpa_scale_legend(),
                          rowHeaders = NULL,
                          stretchH = "all") %>%
                hot_col(col = "Grade", readOnly = TRUE)
        }
    })
    
    # convert hot gpa scale legend to R object --------------------------------
    gpa_scale_legend <- eventReactive({
        input$calculate
        input$gpa_scale}, {
        hot_to_r(input$gpa_legend)
    })
    
    # automatically switch selector controls when the selector changes --------
    observeEvent(input$selector, {
        updateTabsetPanel(session, "params", selected = input$selector)
    }) 
    
    # select transcript dataframe ---------------------------------------------
    hot_transcript <- reactive({
        if (input$selector == "Blank") {
            return(blank_transcript)
        } else if (input$selector == "Example") {
            return(example_transcript)
        } else if (input$selector == "Upload") {
            req(input$file)
            upload_transcript <- read.csv(input$file$datapath)
            as_tibble(upload_transcript)
        }
    })

    # render excel-like table for data input ----------------------------------
    output$transcript <- renderRHandsontable({
        outputOptions(output, "transcript", suspendWhenHidden = FALSE)
        rhandsontable(hot_transcript(), minSpareRows = 1, stretchH = "all",
                      overflow = "visible") %>%
            # format columns
            hot_col(col = "Course",
                    type = "autocomplete", source = courses) %>%
            hot_col(col = "Grade",
                    type = "autocomplete", source = grades, strict = TRUE) %>%
            hot_col(col = "Credits",
                    type = "numeric", format = "0.") %>%
            hot_col(col = "Year",
                    type = "numeric", format = "O") %>%
            hot_col(col = "Term",
                    type = "autocomplete", source = season, strict = TRUE) %>%
            # set column widths
            hot_cols(colWidths = "80") %>%
            # configure right-click context menu
            hot_context_menu(
                allowColEdit = FALSE,
                # add Download to .csv option
                customOpts = list(csv = csv))
        })
    
    # create list of courses for transcript autocomplete ----------------------
    #courses <- reactive({
    #    req(input$transcript)
    #    coursess <- hot_to_r(input$transcript)
    #    coursess <- slice(coursess, 1:(n()-1))
    #    coursess <- coursess %>%
    #        select("Course") %>%
    #        as.vector()
    #}) ### DOES NOT WORK
        
    # convert handsontable data to R object -----------------------------------
    transcript <- eventReactive(input$calculate, {
        # convert handsontable data to R object
        transcript <- hot_to_r(input$transcript)
        # remove spare row at end of dataframe
        transcript <- slice(transcript, 1:(n()-1))
        # assign grade point values
        transcript <- transcript %>%
            mutate("grade_point_values" = case_when(
                Grade == "A+" ~ gpa_scale_legend()[1, 2],
                Grade == "A"  ~ gpa_scale_legend()[2, 2],
                Grade == "A-" ~ gpa_scale_legend()[3, 2],
                Grade == "AB" ~ gpa_scale_legend()[4, 2],
                Grade == "B+" ~ gpa_scale_legend()[5, 2],
                Grade == "B"  ~ gpa_scale_legend()[6, 2],
                Grade == "B-" ~ gpa_scale_legend()[7, 2],
                Grade == "BC" ~ gpa_scale_legend()[8, 2],
                Grade == "C+" ~ gpa_scale_legend()[9, 2],
                Grade == "C"  ~ gpa_scale_legend()[10, 2],
                Grade == "C-" ~ gpa_scale_legend()[11, 2],
                Grade == "CD" ~ gpa_scale_legend()[12, 2],
                Grade == "D+" ~ gpa_scale_legend()[13, 2],
                Grade == "D"  ~ gpa_scale_legend()[14, 2],
                Grade == "D-" ~ gpa_scale_legend()[15, 2],
                Grade == "DF" ~ gpa_scale_legend()[16, 2],
                Grade == "F"  ~ gpa_scale_legend()[17, 2],
                TRUE ~ as.numeric(as.factor("grade_point_values")))
            ) %>%
            # calculate grade points
            mutate("grade_points" = (Credits * grade_point_values)) %>%
            # convert year and term to date
            mutate("Date" = as.Date(as.factor(semester_to_date(Term, Year)))
            )
        # apply any filters that have been filled out in the ui
        if (input$filter_subject != "" & input$date_min != ""
            & input$date_max != "") {
            # subject and date range filter
            transcript <- transcript %>%
                filter(Course == input$filter_subject) %>%
                filter(Date >= date_min() & Date <= date_max())
        } else if (input$date_min != "" | input$date_max != "") {
            # date range filter
            transcript <- transcript %>%
                filter(Date >= date_min() & Date <= date_max())
        } else if (input$filter_subject != "") {
            # subject filter
            transcript <- transcript %>%
                filter(Course == input$filter_subject)
        } else {
            # no filter
            transcript <- transcript
        }
    })
    
    # create date_min object from filter by date input ------------------------
    date_min <- eventReactive(input$calculate, {
        term <- extract_string(input$date_min, season)
        year <- extract_string(input$date_min, years)
        date_min <- as.Date(as.factor(semester_to_date(term, year)))
        return(date_min)
    })
    
    # create date_max object from filter by date input ------------------------
    date_max <- eventReactive(input$calculate, {
        term <- extract_string(input$date_max, season)
        year <- extract_string(input$date_max, years)
        date_max <- as.Date(as.factor(semester_to_date(term, year)))
        return(date_max)
    })
    
    # calculate total credits -------------------------------------------------
    total_credits <- eventReactive(input$calculate, {
        transcript() %>%
            sum_col_variable(Credits)
    })
    
    # calculate total grade points --------------------------------------------
    total_grade_points <- eventReactive(input$calculate, {
        transcript() %>%
            sum_col_variable(grade_points)
    })
    
    ### Maybe add ifs for possible error states
    # calculate overall gpa ---------------------------------------------------
    # DECIDE IF THIS SHOULD HIDE WHEN OTHER INPUTS CHANGE #####################
    # calculate gpa when calculate button is clicked --------------------------
    result <-eventReactive(input$calculate, {
        req(input$transcript)
        result <- format(round(calc_gpa(total_grade_points(), total_credits()),
                     digits = 2), nsmall = 2)
        return(result)
    })
    
    # create result message based on ui inputs
    result_message <- eventReactive(input$calculate, {
        req(input$transcript)
        # outputs
        if (input$filter_subject != "" & input$date_min == input$date_max &
            input$date_min != "" & input$date_max != "") {
            # if subject and date are filtered
            filter_subject <- italic(input$filter_subject)
            date_min <- italic(input$date_min)
            str1 <- paste("Your", filter_subject, "GPA for",
                          date_min, "is...",
                          sep = " ")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>")
            )
        } else if (input$filter_subject != "" &
                   input$date_min != "" & input$date_max != "") {
            # if subject and date are filtered
            filter_subject <- italic(input$filter_subject)
            date_min <- italic(input$date_min)
            date_max <- italic(input$date_max)
            str1 <- paste("Your", filter_subject, "GPA from",
                          date_min, "to", date_max, "is...",
                          sep = " ")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>")
            )
        } else if (input$date_min == input$date_max &
                   input$date_min != "" & input$date_max != "") {
            # if date is filtered to a single semester
            date_min <- italic(input$date_min)
            str1 <- paste("Your GPA for", date_min, "is...",
                          sep = " ")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>")
            )
        } else if (input$date_min != "" & input$date_max != "") {
            # if date is filtered
            date_min <- italic(input$date_min)
            date_max <- italic(input$date_max)
            str1 <- paste("Your GPA from",
                          date_min, "to", date_max, "is...",
                          sep = " ")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>")
            )
        } else if (input$filter_subject != "") {
            # if subject is filtered
            filter_subject <- italic(input$filter_subject)
            str1 <- paste("Your", filter_subject, "GPA is...",
                          sep = " ")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>"))
        } else {
            # if no filter
            str1 <- paste("Your GPA is...", sep = "")
            str2 <- result_gpa(result())
            HTML(paste(str1, str2, sep = "<br/>"))
        }
    })
    
    # render gpa message ------------------------------------------------------
    output$result <- renderUI({
        # validations (add validations)
        #if (input$date_min != "") {
        #    validate(need(date_min(),
        #                  "Please enter the minimum date in the format:
        #                  Term Year (e.g., Fall 2015)"))
        #}
        #if (input$date_max != "") {
        #    validate(need(date_max(),
        #                  "Please enter the maximum date in the format:
        #                  Term Year (e.g., Winter 2018)"))
        #}
        result_message()
        })
    
    # render credits message --------------------------------------------------
    output$credits <- renderUI({
        total_credits <- p(total_credits(),
                                style = "font-size: 48px; font-weight: bold;
                                         margin-top: -6px;
                                         margin-bottom: 0px;")
        HTML(paste("Credits:", total_credits, sep = "<br/>"))
    })
    
    # render grade points message ---------------------------------------------
    output$grade_points <- renderUI({
        total_grade_points <- p(total_grade_points(),
                                style = "font-size: 48px; font-weight: bold;
                                         margin-top: -6px;
                                         margin-bottom: 0px;")
        HTML(paste("Grade points:", total_grade_points, sep = " "))
    })
    
    # go to output tab when calculate button is clicked -----------------------
    observeEvent(input$calculate, {
        updateTabsetPanel(session, "tabset", selected = "Output")
    })
    
    # DECIDE IF THIS SHOULD HIDE WHEN OTHER INPUTS CHANGE #####################
    # hide download button if transcript is edited ----------------------------
    observeEvent(input$transcript, {
        updateTabsetPanel(session, "dlbutton", selected = "nodownload")
    }) 
    
    # show download button after calculation button has been clicked ----------
    observeEvent(input$calculate, {
        updateTabsetPanel(session, "dlbutton", selected = "download")
    }) 
    
    # download calculated transcript data -------------------------------------
    output$dl <- downloadHandler(
        filename = "unofficial_transcript.csv",
        content = function(file) {
            transcript <- transcript() %>%
                select(Course, Grade, Credits, Year, Term)
            write.csv(transcript, file)
        }
    )
    
    ### PLOTTING ### ----------------------------------------------------------
    
    ### add text counts to bars
    # render grade totals plot ------------------------------------------------
    output$grades <- renderPlot({
        transcript <- transcript()
        # reorder grades so F is on the left and A+ is on the right
        transcript$Grade <- fct_rev(factor(transcript$Grade, levels = grades))
        # plot frequency distribution of grades
        ggplot(data = transcript, aes(grades)) +
            geom_bar(mapping = aes(Grade), fill = "steelblue3") +
            geom_text(mapping = aes(label = ..count.., x = Grade),
                      stat = "count",
                      position=position_dodge(width=0.9), vjust=-0.5) +
            scale_x_discrete(drop=FALSE) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "Grade", y = "Total") +
            theme_classic(base_size = 14) +
            theme(plot.margin = margin(t = 14, unit = "pt")) +
            coord_cartesian(clip = "off")

    })
    
    # calculate cumulative gpa over time --------------------------------------
    cumulative_result <- eventReactive(input$calculate, {
        req(input$transcript)
        transcript <- transcript() %>%
            mutate("Semester" = paste(Term, Year, sep = " ")) %>%
            group_by(Date, Semester)
        
        df_credits <- transcript %>% 
            summarise(sum(Credits))
        
        df_grade_points <- transcript %>%
            summarise(sum(grade_points))
        
        transcript <- full_join(df_credits, df_grade_points,
                                by = c("Date", "Semester")) %>%
            select(Date, Semester, "grade_points" = `sum(grade_points)`,
                   "Credits" = `sum(Credits)`) %>%
            mutate("GPA" = grade_points/Credits) %>%
            ungroup() %>%
            mutate(cumcredits = cumsum(Credits)) %>%
            mutate(cumpoints = cumsum(grade_points)) %>%
            mutate(cumgpa = cumpoints/cumcredits)
    })
    
    # create cumulative gpa plot ----------------------------------------------
    plot_cumgpa <- eventReactive(input$calculate, {
        req(input$transcript)
        transcript <- cumulative_result()
        # order semesters by their order in the dataset (low to high)
        transcript$Semester <- factor(transcript$Semester,
                                      levels=unique(transcript$Semester))
        # plot the graph
        ggplot(transcript, aes(x = Semester, y = cumgpa, group = 1)) +
            geom_line() +
            geom_point() +
            {if (input$gpa_scale == "4.0")
                scale_y_continuous(limits = c(0.0, 4.0),
                                   breaks = c(0.0, 1.0, 2.0, 3.0, 4.0))} +
            {if (input$gpa_scale == "4.33")
                scale_y_continuous(limits = c(0.0, 4.33),
                                   breaks = c(0.0, 1.33, 2.33, 3.33, 4.33))} +
            scale_x_discrete(labels = function(x){sub("\\s", "\n", x)}) +
            labs(y = "GPA") +
            theme_classic(base_size = 14)
    })
    
    # render cumulative gpa plot ----------------------------------------------
    output$cumgpa <- renderPlot({
        # plot the graph
        plot_cumgpa()
        
    })
    
    # calculate total credits by subject --------------------------------------
    credit_totals <- eventReactive(input$calculate, {
        req(input$transcript)
        transcript <- transcript() %>%
            group_by(Course) %>%
            summarise(sum(Credits)) %>%
            select(Course, "Credits" = `sum(Credits)`) %>%
            mutate(Course = as.character(Course)) %>%
            arrange(Course)
        
    })
    
    ### make it so you can see the number on top of the highest bar
    # create credit totals plot -----------------------------------------------
    plot_credit_totals <- eventReactive(input$calculate, {
        req(input$transcript)
        transcript <- credit_totals()
        # plot the graph
        ggplot(transcript, aes(Course)) +
            geom_bar(mapping = aes(weight = Credits), fill = "steelblue3") +
            geom_text(mapping = aes(label = Credits, y = Credits),
                      position=position_dodge(width=0.9), vjust=-0.5) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "Course", y = "Credits") +
            theme_classic(base_size = 14) +
            theme(plot.margin = margin(t = 14, unit = "pt")) +
            coord_cartesian(clip = "off")
        
    })
    
    # render credit totals plot -----------------------------------------------
    output$credit_totals <- renderPlot({
        # plot the graph
        plot_credit_totals()
        
    })
    
    # render donate button ----------------------------------------------------
    output$donate <- renderUI({
        tagList(tags$h4("Support Me"),
             tags$p(paste0("If you would like to support my work you can make",
             " a small donation to me. Your donation will go toward improving",
             " the quality and quantity of content I can produce for you,",
             " and will help me pay for any recurring costs I have running",
             " this website. If these costs have been covered by other",
             " donations then your donation will go toward improving my",
             " quality of life and helping me support the people I love.")),
             tags$p(paste0("If you cannot afford to make a donation but",
             " would still like to support my work, you can share it with",
             " others. Donations always help, but ultimately it is",
             " creating connections with other people and",
             " helping them that keeps me going.")),
             tags$p(("Thank you, Michael McCarthy")),
             HTML("<a href='https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=M77S2BN3BFCDA&amp;source=url'
                  class='btn cta'>
                  <i class='fab fa-paypal pr-1' aria-hidden='true'></i> Donate with Paypal</a>")
             )
    })
}
# run the application ---------------------------------------------------------
shinyApp(ui, server)