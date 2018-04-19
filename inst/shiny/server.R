library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(stringr)
library(markdown)
library(grbrowser)

groupingColumns = NULL

shinyServer(function(input,output,session) {
    update_browse_selector(session,c())
    observeEvent(input$dataSet, {
        if(!is.null(input$dataSet)) {
            urlvalue = paste0("http://www.grcalculator.org/grbrowser/?dataset=", gsub("^data_\\d+_(.*?)\\.json$", "\\1", input$dataSet), collapse = "")
            print(input$dataSet)
            updateTextInput(session, "bookmark_input", label = "test",
                            value = urlvalue)
        }
    })

                                        # output$bookmark <- renderText({
                                        #   paste0("http://www.grcalculator.org/grbrowser/?dataset=",
                                        #          gsub("^data_\\d+_(.*?)\\.json$", "\\1", input$dataSet), collapse = "")
                                        # })
    observe({
        query <- parseQueryString(session$clientData$url_search)
                                        # Add underscores to dataset names for url

        URLnames = gsub(" ", "_", (names(dataset_choices)))
        print('URLnames')
        print(URLnames)
        if(length(query) > 0) {
            if (!is.null(query[['dataset']]) & query[['dataset']] %in% URLnames) {
                                        # Take out underscores from dataset name in URL
                URLchoice = gsub("_", " ", query[['dataset']])
                print('URLchoice')
                print(URLchoice)
                updateRadioButtons(session, "dataSet",
                                   choices = dataset_choices,
                                   selected = dataset_choices[[URLchoice]]
                                   )
            }
        }
    })

    observeEvent(input$dataSet, {
        print('dataSet')
        print(input$dataSet)
    })

    boxplot_data_global = NULL

    values <- reactiveValues(config=c(),data=c(),showtabs=0, showtab_drc=1, sub_data = NULL)

    output$input_table <- DT::renderDataTable(DT::datatable({
        x<-values$data
        print(colnames(x))
        data.frame(x)
    }, rownames= FALSE))

    observe({
        toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-data]")
        toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-gr]")
        toggle(condition = values$showtabs_drc, selector = "#tabs li a[data-value=tab-drc]")
    })

    observeEvent(input$dataSet, {
        if (!is.null(input$dataSet) && input$dataSet != "") {
            inFile <- input$dataSet

            if (is.null(inFile)) {return(NULL)}

            cat('file ', inFile, ' received\n')
            json_data <- readLines(paste0("json/", inFile))

            values$config <- fromJSON(json_data)

            output$datasetTitle <- renderUI(
                tags$div(tags$h3(actionLink('dataset_title', values$config$title)))
            )

            output$datasetInfo <- renderUI(
                HTML(markdownToHTML(text=values$config$description, options=c('fragment_only')))
            )

            values$data <- read.table(values$config$datafile, sep="\t", header=TRUE, check.names=FALSE, fill=TRUE, stringsAsFactors = F)

            if (length(values$config$filterColumns)) { values$data <- values$data[values$config$filterColumns] }
            if (length(values$config$renameColumns)) {
                colnames(values$data)[which(colnames(values$data) %in% names(values$config$renameColumns))] <- unlist(values$config$renameColumns)
            }
            if (length(values$config$groupableColumns)==0) { values$config$groupableColumns = colnames(values$data) }

            updateSelectizeInput(session, 'doseresponsegrid_choiceVar', choices = values$config$groupableColumns, server = TRUE, selected=values$config$doseresponse$defaultChoicevar)
            updateSelectizeInput(session, 'doseresponsegrid_groupingVars', choices = values$config$groupableColumns, server = TRUE, selected=values$config$doseresponse$defaultGroupingVars)

            doseresponsegrid_hideselector <- values$config$doseresponse$hideselector
            if (is.null(doseresponsegrid_hideselector)) { doseresponsegrid_hideselector <- 0; }
            updateSelectizeInput(session, 'doseresponsegrid_hideselector', selected=doseresponsegrid_hideselector)

            full_data <- extractData(input, output, values,
                                     values$config$doseresponse$defaultChoicevar,
                                     values$config$doseresponse$defaultGroupingVars)
            subset_data <- full_data

            if(!input$dataSet %in% c("data_6_Cancer_Therapeutics_Response_Portal_(CTRP).json")) {
                values$showtabs_drc = 1
                output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
                    input="",
                    xmin = -4,
                    xmax = 2,
                    factors=c(paste(values$config$doseresponse$defaultGroupingVars,collapse = '_'), values$config$doseresponse$defaultChoicevar),
                    toggle=values$config$doseresponse$toggle,
                    data=subset_data
                )
                groupingColumns <- values$config$groupableColumns
                values$showtabs=1
            } else {
                values$showtabs_drc = 0
                updateTabsetPanel(session,"tabs",selected="tab-gr")
            }
        }
    })

                                        #========== Main dose-response grid =============
    observeEvent(input$'dose-response-grid-main', {
        q = parseLabel(input, values, subset_data)
        if (input$'dose-response-grid-main' != '' && str_count(input$'dose-response-grid-main', '=') == 1) {
            output$graphPopupPlot <- renderPlotly({
                                        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
                ggplotly(q) %>%
                    layout(
                        xaxis = list(range = c(-4,2),
                                     tickmode = 'linear',
                                     tick0 = -5,
                                     dtick = 1)
                    )
            })
            toggleModal(session,"graphPopup")
        }

    })

                                        #========== Download button for scatterplot images =======
    output$downloadScatter = downloadHandler(
        filename = function() {
            if(input$box_scatter_choice == "Scatter plot") {
                type = '_scatter'
            } else {
                type = '_box'
            }
            return(paste(sub("^(.*)[.].*", "\\1", input$dataSet), type, input$scatterImageType, sep=''))
        },
        content = function(filename) {
            if(input$scatterImageType == '.pdf') {
                ggsave(filename = filename, plot = plotScatter_box, device = "pdf")
            } else {
                ggsave(filename = filename, plot = plotScatter_box, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
            }
        }
    )

                                        #========== Download button data tables =======
    output$downloadData <- downloadHandler(
        filename = function() {
            return(paste(sub("^(.*)[.].*", "\\1", input$dataSet), ".", input$download_type, sep=''))
        },
        content = function(filename) {
            data_output = values$data
            if(input$download_type == "tsv") {
                write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T)
            } else if(input$download_type == "csv") {
                write.table(data_output, file = filename, quote = F, sep = ',', row.names = F, col.names = T)
            }

        }
                                        #,
                                        #contentType = paste('text/', input$download_type, sep = "")
    )

                                        #=========================================

    observeEvent(input$plot_scatter, {
        output$plotlyScatter1 <- renderPlotly({
            plot1 = isolate(drawScatter(input, values))
            print(1.1)
                                        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
            ggplotly(plot1) %>%
                layout(plot1,
                       margin = list(
                           r = 10,
                           t = 80,
                           b = 60,
                           l = 100)
                       )
            print(1.22)
        })
    })

    print(1.5)
    observeEvent(input$plot_scatter, {
        drawScatter(input, values)
        output$plotlyScatter1 <- renderPlotly({
            plot1 = isolate(drawScatter(input, values))
            ggplotly(plot1) %>%
                layout(plot1,
                       margin = list(
                           r = 10,
                           t = 80,
                           b = 60,
                           l = 100)
                       )

        })
    })

                                        # Make scatterplot reactive to "pick_parameter" after first plot.
    observeEvent(input$pick_parameter, {
        if(input$plot_scatter > 0) {
            output$plotlyScatter1 <- renderPlotly({
                plot1 = isolate(drawScatter(input, values))
                ggplotly(plot1) %>%
                    layout(plot1,
                           margin = list(
                               r = 10,
                               t = 80,
                               b = 60,
                               l = 100)
                           )
            })
        }
    })

                                        #=========== update select boxes for boxplot ============

    observeEvent(input$dataSet, {
        updateSelectInput(
            session, 'pick_box_x',
            choices = values$config$groupableColumns
        )
        updateSelectInput(
            session, 'pick_box_point_color',
            choices = values$config$groupableColumns
        )
    })

    observeEvent(c(input$dataSet, input$pick_box_x, input$tabs, values$sub_data), {
        if(!is.null(input$pick_box_x)) {
            print("update pick_box_factors")
            picks = sort(unique(subset_data[[input$pick_box_x]]))
            updateSelectizeInput(
                session, 'pick_box_factors',
                choices = picks,
                selected = picks[1:min(10, length(picks))]
            )
        }
    })

    observeEvent(input$box_scatter_choice, {
        if(!is.null(input$pick_box_x)) {
            picks = sort(unique(subset_data[[input$pick_box_x]]))
            updateSelectizeInput(
                session, 'pick_box_factors',
                choices = picks,
                selected = picks[1:min(10, length(picks))]
            )
        }
    })

    observeEvent(input$factorA, ignoreNULL = FALSE, {
        print("refresh B")
        picks = sort(input$pick_box_factors)
        picks1 = setdiff(picks, input$factorA)
        updateSelectizeInput(
            session, 'factorB',
            choices = picks1,
            selected = input$factorB
        )
    })

    observeEvent(input$factorB, ignoreNULL = FALSE, {
        print("refresh A")
        picks = sort(input$pick_box_factors)
        picks2 = setdiff(picks, input$factorB)
        updateSelectizeInput(
            session, 'factorA',
            choices = picks2,
            selected = input$factorA
        )
    })

    observeEvent(c(input$box_scatter_choice, input$pick_box_x, input$pick_box_factors), {
        if(!is.null(input$pick_box_x)) {
            print(input$pick_box_factors)
            print(typeof(input$pick_box_factors))
            print(class(input$pick_box_factors))
            picks = sort(input$pick_box_factors)
            updateSelectizeInput(
                session, 'factorA',
                                        #choices = unique(values$GR_table[[input$pick_box_x]]),
                choices = picks,
                selected = NULL
            )
            updateSelectizeInput(
                session, 'factorB',
                                        #choices = unique(values$GR_table[[input$pick_box_x]]),
                choices = picks,
                selected = NULL
            )
        }
    })

                                        #===== update select boxes for scatterplot ==========

    observeEvent(input$dataSet, {
        updateSelectInput(
            session, 'pick_var',
            choices = values$config$groupableColumns
        )
        print(values$config$groupableColumns)
        print(values$config$renameColumns)
        print(values$config$filterColumns)
    })

    observeEvent(input$pick_var, {
        updateSelectInput(
            session, 'x_scatter',
            choices = sort(unique(subset_data[[input$pick_var]])),
            selected = NULL
        )
        updateSelectizeInput(
            session, 'y_scatter',
            choices = sort(unique(subset_data[[input$pick_var]])),
            selected = NULL
        )
    })

                                        #===== Boxplot drawing =========

    observeEvent(c(input$dataSet, input$plot_height), {
        output$boxplot <- renderPlotly({
            box = redrawPlotlyBox(input, values, full_data)
            if(!is.null(box)) {
                box
            } else {stop()}
        })
    })

    output$subset_selectize <- renderUI({
        n <- length(values$config$groupableColumns)
        if (n>0) {
            subset_cols = values$config$groupableColumns
            code_output_list <- lapply(1:n, function(i) {
                codeOutput <- paste("subset__", subset_cols[i], sep="")
                subset_choices = sort(unique(full_data[,subset_cols[i]]))
                selectizeInput(codeOutput, subset_cols[i], choices = subset_choices, multiple = TRUE, width = "90%")
            })
        } else code_output_list <- list()
                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
        do.call(tagList, code_output_list)
    })

    output$grmetric_plot_ui <- renderUI({
        if(input$box_scatter_choice == "Box plot") {
            plotlyOutput('boxplot', height = input$plot_height)
        } else {
            plotlyOutput("plotlyScatter1", height = input$plot_height)
        }
    })

    output$scatter <- renderUI({
        if(input$box_scatter_choice == "Scatter plot") {
            fluidRow(
####### Change selectize options to specified in json
                selectInput('pick_parameter', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'Hill', 'GR_AOC', 'IC50')),
                selectInput('pick_var', 'Select variable', choices = values$config$groupableColumns),
                selectInput('x_scatter', 'Select x-axis value', choices = unique(full_data[[input$pick_box_x]])),
                selectizeInput('y_scatter', 'Select y-axis value', choices = unique(full_data[[input$pick_box_x]])),
                bsButton('plot_scatter', 'Add', size = 'small'),
                bsButton('clear', 'Clear', size = 'small')
            )
        } else {
            fluidRow(
###### Change "pick_box_y" metrics to inherit from json for each dataset?
###### Add IC (and GI?) metrics to pick_box_y
                selectInput('pick_box_y', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'h_GR', 'GR_AOC', 'IC50', 'Emax', 'Einf', 'h', 'AUC')),
                selectInput('pick_box_x', 'Select grouping variable', choices = values$config$groupableColumns),
                selectInput('pick_box_point_color', 'Select additional point coloring', choices = values$config$groupableColumns),
                selectizeInput('pick_box_factors', 'Show/hide data', choices = c(), multiple = T),
                actionLink('wilcox_panel', 'Compare boxplots'),
                conditionalPanel(condition = "input.wilcox_panel%2==1",
                                 selectizeInput('factorA', 'Wilcoxon rank-sum test', choices = c(), multiple = T),
                                 selectizeInput('factorB', '', choices = c(), multiple = T),
                                 radioButtons('wilcox_method', label = "",choices = c("One-sided", "Two-sided"), selected = "Two-sided", inline = F),
                                 textOutput("wilcox")
                                 )

            )
        }
    })

    observeEvent(c(input$factorA, input$factorB, input$pick_box_y,
                   input$wilcox_method), {
                       wil_data = subset_data
                       if(!is.null(input$factorA) & !is.null(input$factorB)) {
                           print(input$pick_box_x)
                           rowsA = wil_data[[input$pick_box_x]] %in% input$factorA
                           rowsB = wil_data[[input$pick_box_x]] %in% input$factorB
                           wil_dataA = wil_data[rowsA,input$pick_box_y]
                           wil_dataB = wil_data[rowsB,input$pick_box_y]
                           print(head(wil_dataA))
                           print(head(wil_dataB))
                           wil_less = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "less")
                           wil_greater = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "greater")
                           wil_two_sided = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "two.sided")$p.value
                           wil_one_sided = min(wil_less$p.value,wil_greater$p.value)
                           wil_pval = ifelse(input$wilcox_method == "One-sided", wil_one_sided, wil_two_sided)
                           values$wilcox = prettyNum(wil_pval, digits = 2)
                           output$wilcox = renderText({
                               paste("P-value:", prettyNum(wil_pval, digits = 2))
                           })
                       } else {
                           output$wilcox = renderText({
                               paste("P-value: ")
                           })
                           values$wilcox = NULL
                       }
                   })

                                        #==== Clear scatterplot on "browse" =========

    observeEvent(input$dataSet, {
        output$plotlyScatter1 <- renderPlotly({
            parameter_choice = input$pick_parameter
            if(parameter_choice == 'GR50') {
                parameter_choice = 'log10(GR50)'
            }
            if(parameter_choice == 'Hill') {
                parameter_choice = 'log2(h_GR)'
            }
            padding = 0.05
            scatter_values = subset_data[,parameter_choice]
            finite_values = which(is.finite(scatter_values))
            scatter_values = scatter_values[finite_values]
            x_min = min(scatter_values, na.rm = T)
            x_max = max(scatter_values, na.rm = T)
            y_min = min(scatter_values, na.rm = T)
            y_max = max(scatter_values, na.rm = T)
            all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
            all_range = 2*all_max
            all_max = all_max + padding*all_range
            all_min = -all_max
                                        #plug in a filler data frame
            p = ggplot(data = mtcars, aes(x = mpg, y = wt)) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()

            df_full <- NULL
            print(3)
                                        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
            ggplotly(p) %>%
                layout(p, hovermode = FALSE)
        })
    })

                                        #===== Clear button ========
    observeEvent(input$clear, {
        output$plotlyScatter1 <- renderPlotly({
            parameter_choice = input$pick_parameter
            if(parameter_choice == 'GR50') {
                parameter_choice = 'log10(GR50)'
            }
            if(parameter_choice == 'Hill') {
                parameter_choice = 'log2(h_GR)'
            }
            padding = 0.05
            scatter_values = subset_data[,parameter_choice]
            finite_values = which(is.finite(scatter_values))
            scatter_values = scatter_values[finite_values]
            x_min = min(scatter_values, na.rm = T)
            x_max = max(scatter_values, na.rm = T)
            y_min = min(scatter_values, na.rm = T)
            y_max = max(scatter_values, na.rm = T)
            all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
            all_range = 2*all_max
            all_max = all_max + padding*all_range
            all_min = -all_max

            p = ggplot(data = df_sub, aes(x = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub)), y = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub)))) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()

            df_full <- NULL
            print(3)
                                        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
            ggplotly(p) %>%
                layout(p, hovermode = FALSE)

        })

    })
                                        # cancel.onSessionEnded <- session$onSessionEnded(function() {
                                        #   graphics.off()
                                        #   print('devices off')
                                        # })
})
