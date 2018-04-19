#' @export
redrawPlotlyBox <- function(input, values, subset_data) {
    all_inputs <- names(input)
    for(col_name in all_inputs[grep("^subset__", all_inputs)]) {
        if (length(input[[col_name]])>0) {
            sel_values_list <- input[[col_name]]
            df_colname <- gsub("^subset__(.*)","\\1",col_name)
            subset_data <- subset_data[which(subset_data[[df_colname]] %in% sel_values_list),]
        }
    }
    values$sub_data = subset_data

    parameter_choice = input$pick_box_y
    print(parameter_choice)
    parameter_choice_format = parameter_choice
                                        #print(df_sub)
    if(parameter_choice == 'GR50') {
        parameter_choice = 'log10(GR50)'
        parameter_choice_format = "log<sub>10</sub>(GR<sub>50</sub>)"
    }
    if(parameter_choice == 'Hill') {
        parameter_choice = 'log2(h_GR)'
        parameter_choice_format = "log<sub>2</sub>(h<sub>GR</sub>)"
    }
    if(input$dataSet %in% c("data_5_Genentech_Cell_Line_Screening_Initiative_(gCSI).json")) {
        if(parameter_choice == 'IC50') {
            parameter_choice = 'log10(IC50)'
            parameter_choice_format = "log<sub>10</sub>(IC<sub>50</sub>)"

        }
    }
    boxplot_data = subset_data[subset_data[[ input$pick_box_x ]] %in% input$pick_box_factors,]
    boxplot_data = boxplot_data[is.finite(boxplot_data[[parameter_choice]]),]
    boxplot_data[[ input$pick_box_x ]] = factor(boxplot_data[[ input$pick_box_x ]])

    if(!is.null(input$factorB) & !is.null(input$factorA)) {
        for(i in 1:length(input$factorB)) {
            boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorB[i])
        }
        for(i in 1:length(input$factorA)) {
            boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorA[i])
        }
    }

    x_factor = factor(get(input$pick_box_x, envir = as.environment(boxplot_data)))
    y_variable = get(parameter_choice, envir = as.environment(boxplot_data))
    point_color = factor(get(input$pick_box_point_color, envir = as.environment(boxplot_data)))

    unit_label = gsub("nanomolar", "nM", input$add_units)
    unit_label = gsub("micromolar", paste0("&#956;", "M"), unit_label)

    if(dim(boxplot_data)[1] > 0) {
        p <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable)) +
            geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) +
            geom_jitter(width = 0.2, show.legend = F, aes(colour = point_color)) +
            eval(parse(text = input$theme_select)) + theme(
                                                         axis.text = element_text(size = input$axis_label_size),
                                                         axis.title = element_text(size = input$axis_title_size),
                                                         plot.title = element_text(size = input$plot_title_size),
                                                         plot.margin = unit(c(5.5, 5.5, input$bottom_margin, 5.5), "points"),
                                        #top, right, bottom, left
                                                         axis.text.x = element_text(angle = input$label_rotate,
                                                                                    hjust = 0.5*(1 - sin((-input$label_rotate)*pi/180)),
                                                                                    vjust = 0.5*(1 + cos((-input$label_rotate)*pi/180)))
                                                     ) +
            labs(title = input$plot_title, x = input$x_label)

        p_plotly = plotly_build(p)
        test_box <- p_plotly
                                        # Get y range:
        if(is.null(p_plotly$layout)) {
            top_y = p_plotly$x$layout$yaxis$range[2]
            bottom_y = p_plotly$x$layout$yaxis$range[1]
        } else {
            top_y = p_plotly[[2]]$yaxis$range[2]
            bottom_y = p_plotly[[2]]$yaxis$range[1]
        }
        total_y_range = top_y - bottom_y

        if(!is.null(values$wilcox)) {
                                        # Get top of boxplot whiskers
            whiskers = NULL
                                        #for(i in 1:length(levels(x_factor))) {
            len = length(input$factorA) + length(input$factorB)
            for(i in 1:len) {
                if(is.null(p_plotly$data)) {
                    whiskers[i] = fivenum(p_plotly$x$data[[i]]$y)[5]
                } else {
                    whiskers[i] = fivenum(p_plotly[[1]][[i]]$y)[5]
                }
            }
            top_whisker = max(whiskers, na.rm = TRUE)
            y_range = (top_y - top_whisker)/total_y_range
            if(y_range < .25) {
                top_y = top_whisker + .25*total_y_range
                                        #y_range = top_y - top_whisker
            }
            lh = top_whisker + total_y_range*(.1)
            bump = total_y_range*(.05)
            ll = lh - bump
            lenA = length(input$factorA)
            lenB = length(input$factorB)

            if(lenA == 1 & lenB == 1) {
                p = p + annotate("text", x = 1.5, y = lh + bump/2, label = paste("p =",values$wilcox)) +
                    geom_segment(x = 1, y = lh, xend = 2, yend = lh) +
                    geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
                    geom_segment(x = 2, y = ll, xend = 2, yend = lh)
            } else if(lenA > 1 & lenB == 1) {
                p = p + annotate("text", x = ((lenA + 1) + ((lenA+1)/2))/2, y = lh + 2*bump,
                                 label = paste("p =",values$wilcox)) +
                    geom_segment(x = 1, y = lh, xend = lenA, yend = lh) +
                    geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
                    geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
                    geom_segment(x = (lenA+1)/2, y = lh + bump, xend = lenA + 1, yend = lh + bump) +
                    geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh + bump) +
                    geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh + bump)
            } else if(lenA == 1 & lenB > 1) {
                p = p + annotate("text", x = 1.25 + .25*lenB, y = lh + 2*bump, label = paste("p =",values$wilcox)) +
                    geom_segment(x = 1, y = lh+bump, xend = .5*lenB + 1.5, yend = lh+bump) +
                    geom_segment(x = 1, y = ll, xend = 1, yend = lh+bump) +
                    geom_segment(x = 1.5+.5*lenB, y = lh, xend = 1.5+.5*lenB, yend = lh+bump) +
                    geom_segment(x = 2, y = lh, xend = lenB + 1, yend = lh) +
                    geom_segment(x = 2, y = ll, xend = 2, yend = lh) +
                    geom_segment(x = lenB+1, y = ll, xend = lenB+1, yend = lh)
            } else if(lenA > 1 & lenB > 1) {
                p = p + annotate("text", x = .25*(lenB-1)+.75*(lenA+1), y = lh + 2*bump,
                                 label = paste("p =",values$wilcox)) +
                    geom_segment(x = 1, y = lh, xend = lenA, yend = lh) +
                    geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
                    geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
                    geom_segment(x = lenA+1, y = lh, xend = lenA+lenB, yend = lh) +
                    geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh) +
                    geom_segment(x = lenA+lenB, y = ll, xend = lenA+lenB, yend = lh) +
                    geom_segment(x = (lenA+1)/2, y = lh+bump, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump) +
                    geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh+bump) +
                    geom_segment(x = (lenA+1)+((lenB-1)/2), y = lh, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump)
            }
            p_plotly = p + labs(y = paste(parameter_choice_format, unit_label))
            if(input$add_units == "micromolar") {
                p_ggplot = p + labs(y = bquote(.(parameter_choice) ~ .(quote(mu)) * "M"))
            } else if(input$add_units == "nanomolar") {
                p_ggplot = p + labs(y = paste(parameter_choice, "nM"))
            } else {
                p_ggplot = p + labs(y = parameter_choice)
            }

            plotScatter_box <- p_ggplot

            p_plotly = plotly_build(p_plotly)
            if(is.null(p_plotly$layout)) {
                p_plotly$x$layout$yaxis$range[2] = top_y
            } else {
                p_plotly[[2]]$yaxis$range[2] = top_y
            }
        } else {
            p_plotly = p + labs(y = paste(parameter_choice_format, unit_label))
            if(input$add_units == "micromolar") {
                p_ggplot = p + labs(y = bquote(.(parameter_choice) ~ .(quote(mu)) * "M"))
            } else if(input$add_units == "nanomolar") {
                p_ggplot = p + labs(y = paste(parameter_choice, "nM"))
            } else {
                p_ggplot = p + labs(y = parameter_choice)
            }

            plotScatter_box <- p_ggplot
            p_plotly = plotly_build(p_plotly)
        }

                                        # Current CRAN version of plotly (3.6.0) uses p_plotly$data
                                        # Latest github version of plotly (4.3.5) uses p_plotly$x$data
        if(is.null(p_plotly$data)) {
            for(i in 1:length(p_plotly$x$data)) {
                if(!is.null(p_plotly$x$data[[i]]$text)) {
                    p_plotly$x$data[[i]]$text = gsub('x_factor', input$pick_box_x, p_plotly$x$data[[i]]$text)
                    p_plotly$x$data[[i]]$text = gsub('y_variable', parameter_choice, p_plotly$x$data[[i]]$text)
                }
            }
        } else {
            for(i in 1:length(p_plotly$data)){
                if(!is.null(p_plotly$data[[i]]$text)) {
                    p_plotly$data[[i]]$text = gsub('x_factor', input$pick_box_x, p_plotly$data[[i]]$text)
                    p_plotly$data[[i]]$text = gsub('y_variable', parameter_choice, p_plotly$data[[i]]$text)
                }
            }
        }
        test_box <- p_plotly
        return(p_plotly)
    }
}
