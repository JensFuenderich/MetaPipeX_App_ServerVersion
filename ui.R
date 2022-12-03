### UI

ui <- shiny::navbarPage(

  # make it pretty
  theme = shinythemes::shinytheme("flatly"),

  "MetaPipeX Shiny Application",

  shiny::tabPanel(
    "Upload Data",

    shiny::sidebarPanel(
      shiny::selectInput(inputId = "select_upload",
                         label = "Choose the type of data you want to use in the app from the dropdown menu:",
                         choices = c("Individual Participant Data" = "IPD",
                                     "Replication Summaries" =  "ReplicationSum",
                                     "Merged Replication Summaries" = "MergedReplicationSum",
                                     "MetaPipeX (Meta-Analysis & Replication Summaries)" = "MetaPipeX"),
                         selected = "MetaPipeX"
      ),
      shiny::fluidRow(
        column(6,align="left",uiOutput("confirm_upload2"))
      ),
      shiny::p("For more information on the MetaPipeX framework, please refer to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX", "github documentation."))
    ),

    mainPanel(

      ## panel for upload of IPD
      shiny::conditionalPanel(condition = "input.select_upload == 'IPD'",
                              h3("Individual Participant Data"),
                              h5("Please provide at least one .csv/.sav/.rds file. The ",
                                 tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/1_Individual_Participant_Data/codebook_for_individual_participant_data.csv", "codebook on github."),
                                 "describes the 5 columns that are needed for the analysis. The names do not have to be the same as in this codebook, but they should be consistent across the .csv files. If only data from a single multi-lab or a single replication project (or targer-effect) is uploaded, a placeholder for the name needs to be provided. It is possible to create such a placeholer by clicking the corresponding checkbox."),
                              fileInput("IPD", "choose .csv/.sav/.rds file with individual participant data",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv",
                                                   ".sav",
                                                   ".rds")),
                              h5("The MetaPipeX needs to know which columns of the data should be used. Select them accordingly:"),
                              shiny::selectInput(inputId = "multilab_col",
                                                 label = "MultiLab:",
                                                 choices = ""),
                              shiny::checkboxInput(inputId = "create_custom_multilab_col",
                                                   label = "Create a MultiLab column"),
                              uiOutput("out_custom_multilab_col"),
                              shiny::selectInput(inputId = "replicationproject_col",
                                                 label = "ReplicationProject:",
                                                 choices = ""),
                              shiny::checkboxInput(inputId = "create_custom_replicationproject_col",
                                                   label = "Create a ReplicationProject column"),
                              uiOutput("out_custom_replicationproject_col"),
                              shiny::selectInput(inputId = "replication_col",
                                                 label = "Replication:",
                                                 choices = ""),
                              shiny::selectInput(inputId = "DV_col",
                                                 label = "DV:",
                                                 choices = ""),
                              shiny::selectInput(inputId = "group_col",
                                                 label = "Group:",
                                                 choices = ""),
                              shiny::checkboxInput(inputId = "filter_question",
                                                   label = "Do you need to filter data?"),
                              shiny::selectInput(inputId = "filter_col",
                                                 label = "Filter Variable:",
                                                 choices = ""),
                              tags$style("#expr-container label {font-weight: 400;}"),
                              tags$div(id = "expr-container",
                                       uiOutput("out_filter_identifier")),
                              h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
      ),

      ## panel for upload of Replication summaries
      shiny::conditionalPanel(condition = "input.select_upload == 'ReplicationSum'",
                              h3("Replication Level Data"),
                              h5("Please provide at least one .csv that has been produced by MetaPipeX::create_replication_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/2_Replication_Summaries/Replication_Summaries_template.csv", "template on github.")),
                              fileInput("ReplicationSum", "choose file(s) from local drive",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
      ),


      ## panel for upload of merged Replication summaries

      shiny::conditionalPanel(condition = "input.select_upload == 'MergedReplicationSum'",
                              h3("Merged Replication Level Data"),
                              h5("Please provide a single .csv that has been produced by MetaPipeX::merge_replication_summaries() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/3_Merged_Replication_Summaries/Merged_Replication_Summaries_template.csv", "template on github.")),
                              fileInput("MergedReplicationSum", "choose a single .csv file with merged replication level data",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              h5("Hit the button 'Provide MetaPipeX data format to the app.' in order for the MetaPipeX package to run its analyses.")
      ),

      ## panel for upload of data from MetaPipeX
      shiny::conditionalPanel(condition = "input.select_upload == 'MetaPipeX'",
                              h3("MetaPipeX Data"),
                              h5("Please provide a single .csv that has been produced by MetaPipeX::full_pipeline() or is arranged according to the", tags$a(href="https://github.com/JensFuenderich/MetaPipeX/blob/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv", "template on github.")),
                              fileInput("MetaPipeX", "choose .csv file with MetaPipeX data",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              h5("Hit the button 'Provide MetaPipeX data format to the app.' and go to the Data Selection tab.")
      )
    )
  ),

  ## tab for Data Selection

  shiny::tabPanel(
    "Data Selection",

    shiny::sidebarLayout(

      shiny::sidebarPanel(
        h3("Data Subset"),
        shinyWidgets::materialSwitch(inputId = "Level",
                                     label = "Reduce to meta-analytical Data?",
                                     status = "success"),
        shiny::selectInput(inputId = "MultiLab",
                           label = "MultiLab",
                           choices = ""
        ),
        shiny::selectInput(inputId = "ReplicationProject",
                           label = "ReplicationProject",
                           choices = ""
        ),
        shiny::selectInput(inputId = "Replication",
                           label = "Replication",
                           choices = c("all", unique(MetaPipeX_data_full$Replication))
        ),
        shinyWidgets::prettyCheckboxGroup(inputId = "Statistics",
                                          label = h3("Replication Statistics"),
                                          choices = Variables_List$Statistics,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        tags$head(
          tags$style(HTML("input[name=Statistics][value='exclude'] { display: none }"))
        ),
        # h3("Exclude Further Information"),
        shinyWidgets::materialSwitch(inputId = "Stat_SE",
                                     label = "Exclude Standard Error of Replication Level Statistic?",
                                     status = "success"),
        shinyWidgets::prettyCheckboxGroup(inputId = "AnalysisResults",
                                          label = h3("Meta-analysis results (MD & SMD)"),
                                          choices = Variables_List$AnalysisResults,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        tags$head(
          tags$style(HTML("input[name=AnalysisResults][value='exclude'] { display: none }"))
        ),
        shinyWidgets::prettyCheckboxGroup(inputId = "SampleSize",
                                          label = h3("Sample Size Information"),
                                          choices = Variables_List$Sample_Size,
                                          selected = "exclude",
                                          animation = "pulse",
                                          shape = "curve"
        ),
        tags$head(
          tags$style(HTML("input[name=SampleSize][value='exclude'] { display: none }"))
        ),
        h3("Exclude Non-Effects?"),
        shiny::sliderInput(inputId = "exclude_effects",
                           label = "Exlcude replication projects with a model estimate for |g| lower than...",
                           min = 0,
                           max = 3,
                           value = 0,
                           step = 0.1)

      ),
      mainPanel(
        DT::DTOutput("selected_data"),
        downloadButton("downloadData", "Download Table Data"),
        uiOutput("out_zip_download")
      )
    )
  ),


  ## tab for Data Exclusion

  shiny::tabPanel(
    "Data Exclusion",

    shiny::sidebarLayout(

      shiny::sidebarPanel(
        h3("Exclude Data"),
        shiny::selectInput(inputId = "MultiLab_Exclusion",
                           label = "MultiLab",
                           choices = ""
        ),
        shiny::selectInput(inputId = "ReplicationProject_Exclusion",
                           label = "ReplicationProject",
                           choices = ""
        ),
        shiny::selectInput(inputId = "Replication_Exclusion",
                           label = "Replication",
                           choices = c("all", unique(MetaPipeX_data_full$Replication))
        ),
        shiny::actionButton(inputId = "exclusion",
                            label = "Exclude!"
        ),
        h3("Remove Exclusion"),
        shiny::selectInput(inputId = "Remove_MultiLab_Exclusion",
                           label = "MultiLab",
                           choices = ""
        ),
        shiny::selectInput(inputId = "Remove_ReplicationProject_Exclusion",
                           label = "ReplicationProject",
                           choices = ""
        ),
        shiny::selectInput(inputId = "Remove_Replication_Exclusion",
                           label = "Replication",
                           choices = c("all", unique(MetaPipeX_data_full$Replication))
        ),
        shiny::actionButton(inputId = "remove_exclusion",
                            label = "Remove Exclusion!"
        ),
      ),
      mainPanel(
        h3("All Exclusions"),
        DT::DTOutput("excluded_data"),
        h3("Remaining Data"),
        DT::DTOutput("remaining_data")
      )
    )
  ),


  ## tab for Kernel Density Estimations

  shiny::tabPanel("Kernel Density Estimations",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_kernel_density_est",
                                          label = "Upload Data"),
                      shiny::varSelectInput(inputId = "kernel_density_est_data_est",
                                            label = "choose a replication statistic of interest",
                                            data = data()),
                      shiny::varSelectInput(inputId = "kernel_density_est_data_model_est",
                                            label = "choose the model estimate",
                                            data = data()),
                      shiny::varSelectInput(inputId = "kernel_density_est_data_Tau",
                                            label = "choose the according tau",
                                            data = data())
                    ),
                    mainPanel(
                      h4("Kernel Density Estimations for selected statistics"),
                      uiOutput("kernel_density_estmary_out"),
                      shiny::downloadLink("download_kernel_density_est", "Download Kernel Density Estimations")
                    )
                  )
  ),

  ## tab for Histograms

  shiny::tabPanel("Histograms",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_hist",
                                          label = "Upload Data"),
                      shiny::varSelectInput(inputId = "hist_data1",
                                            label = "choose a statistic for the histogram",
                                            data = data()),
                      checkboxInput(inputId = "hist_include_variable2",
                                    label = "Include a second Variable"),
                      shiny::varSelectInput(inputId = "hist_data2",
                                            label = "choose a statistic for the histogram",
                                            data = data()),
                      checkboxInput(inputId = "hist_include_variable3",
                                    label = "Include a third Variable"),
                      shiny::varSelectInput(inputId = "hist_data3",
                                            label = "choose a statistic for the histogram",
                                            data = data())
                    ),
                    mainPanel(
                      h4("Histogram for selected statistics"),
                      plotOutput(outputId = "histogram",
                                 hover = "hist_hover"),
                      shiny::downloadLink("download_hist", "Download Histogram"),
                      DT::DTOutput("hist_data_table")
                    )
                  )
  ),

  ## tab for Violin Plots

  shiny::tabPanel("Violin Plots",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_violin",
                                          label = "Upload Data"),
                      shiny::radioButtons(inputId = "include_violins",
                                          h3("Number of Violins"),
                                          choices = list("1" = 1,
                                                         "2" = 2,
                                                         "3" = 3,
                                                         "4" = 4,
                                                         "5" = 5,
                                                         "6" = 6),
                                          selected = 1
                      ),
                      shiny::varSelectInput(inputId = "violin_1",
                                            label = "choose a statistic for violin 1",
                                            data = data()),
                      shiny::varSelectInput(inputId = "violin_2",
                                            label = "choose a statistic for violin 2",
                                            data = data()),
                      shiny::varSelectInput(inputId = "violin_3",
                                            label = "choose a statistic for violin 3",
                                            data = data()),
                      shiny::varSelectInput(inputId = "violin_4",
                                            label = "choose a statistic for violin 4",
                                            data = data()),
                      shiny::varSelectInput(inputId = "violin_5",
                                            label = "choose a statistic for violin 5",
                                            data = data()),
                      shiny::varSelectInput(inputId = "violin_6",
                                            label = "choose a statistic for violin 6",
                                            data = data()),
                      shiny::checkboxInput(inputId = "violin_include_point_size",
                                           label = "Point Size"),
                      shiny::varSelectInput(inputId = "violin_point_size",
                                            label = "choose a statistic for the point size",
                                            data = data())


                    ),
                    mainPanel(
                      h4("Vioin Plot for selected statistics"),
                      plotOutput(outputId = "violin_plot",
                                 hover = "violin_hover"),
                      shiny::downloadLink("download_violin", "Download Violin Plot"),
                      DT::DTOutput("violin_data_table")
                    )
                  )


  ),

  ## tab for Scatter Plots

  shiny::tabPanel("Scatter Plots",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_scatter", label = "Upload Data"),
                      shiny::varSelectInput(inputId = "x_plot", label = "choose a statistic for x", data = data()),
                      shiny::varSelectInput(inputId = "y_plot", label = "choose a statistic for y", data = data()),
                      checkboxInput(inputId = "include_point_size", label = "Point Size"),
                      shiny::varSelectInput(inputId = "size_plot", label = "choose a statistic for the point size", data = data()),
                      checkboxInput(inputId = "include_point_color", label = "Point Color"),
                      shiny::varSelectInput(inputId = "color_plot", label = "choose a statistic for the point color", data = data()),
                      checkboxInput(inputId = "include_custom_lims", label = "Use Custom Axis Limits (essentially zooming in or out) and update correlation"),
                      numericInput(inputId = "x_min_plot", label = "Minimum of X-Axis", value = 0),
                      numericInput(inputId = "x_max_plot", label = "Maximum of X-Axis", value = 100),
                      numericInput(inputId = "y_min_plot", label = "Minimum of Y-Axis", value = 0),
                      numericInput(inputId = "y_max_plot", label = "Maximum of Y-Axis", value = 100)
                    ),
                    mainPanel(
                      h4("Scatter Plot for selected statistics"),
                      plotOutput(outputId = "scatter_plot",
                                 hover = "scatter_hover"),
                      shiny::downloadLink("download_scatter", "Download Scatter Plot"),
                      DT::DTOutput("scatter_data_table")
                    )
                  )
  ),

  ## tab for Forest Plots

  shiny::tabPanel("Forest Plots",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_forest",
                                          label = "Upload Data"),
                      shiny::varSelectInput(inputId = "forest_data_statistics",
                                            label = "choose a replication statistic of interest",
                                            data = data()),
                      shiny::varSelectInput(inputId = "forest_data_SE",
                                            label = "choose the according standard error",
                                            data = data()),
                      shiny::varSelectInput(inputId = "forest_data_replication",
                                            label = "choose information on aggregation (likely the replication)",
                                            data = data())
                    ),
                    mainPanel(
                      h4("Forest Plot for selected statistics"),
                      shiny::plotOutput(outputId = "forest_plot"),
                      shiny::downloadLink("download_forest", "Download Forest Plot")
                    )
                  )
  ),

  ## tab for Funnel Plots

  shiny::tabPanel("Funnel Plots",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_funnel",
                                          label = "Upload Data"),
                      shiny::varSelectInput(inputId = "funnel_data_est",
                                            label = "choose a replication statistic of interest",
                                            data = data()),
                      shiny::varSelectInput(inputId = "funnel_data_SE",
                                            label = "choose the according standard error",
                                            data = data()),
                      shiny::varSelectInput(inputId = "funnel_data_model_est",
                                            label = "choose the model estimate",
                                            data = data())
                    ),
                    mainPanel(
                      h4("Funnel Plot for selected statistics"),
                      shiny::plotOutput(outputId = "funnel_plot",
                                        hover = "funnel_hover"),
                      DT::DTOutput("funnel_data_table"),
                      shiny::plotOutput(outputId = "funnel_CE_plot"),
                      shiny::downloadLink("download_funnel", "Download Funnel Plot")
                    )
                  )
  ),

  ## tab for Meta Plots

  shiny::tabPanel("Meta Plots",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::actionButton(inputId = "upload_metaplot",
                                          label = "Upload Data"),
                      shiny::varSelectInput(inputId = "metaplot_data_est",
                                            label = "choose a replication statistic of interest",
                                            data = data()),
                      shiny::varSelectInput(inputId = "metaplot_data_SE",
                                            label = "choose the according standard error",
                                            data = data()),
                      shiny::varSelectInput(inputId = "metaplot_data_t_n",
                                            label = "choose treatment group n",
                                            data = data()),
                      shiny::varSelectInput(inputId = "metaplot_data_c_n",
                                            label = "choose control group n",
                                            data = data()),
                      h5("For details on how to interpret the meta-plot, please refer to the preprint by van Assen et al. (2020):"),
                      h5("https://psyarxiv.com/cwhnq/")
                    ),
                    mainPanel(
                      h4("Meta Plot for selected statistics"),
                      shiny::plotOutput(outputId = "metaplot"),
                      shiny::downloadLink("download_meta", "Download Meta Plot")
                    )
                  )
  ),

  ## tab for Codebook Display

  shiny::tabPanel("Codebook Display",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      h3("How to use this codebook:"),
                      shiny::p(codebook_text_vec)
                    ),
                    mainPanel(
                      h4("Tabular Codebook"),
                      DT::DTOutput("codebook"),
                      downloadButton("downloadCodebook", "Download Codebook"),
                    )
                  )
  )

)
