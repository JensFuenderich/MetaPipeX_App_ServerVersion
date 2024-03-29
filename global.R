# ### install packages
#
# install.packages(c("devtools",
#                    "DT",
#                    "ggplot2",
#                    "grDevices",
#                    "haven",
#                    "janitor",
#                    "magrittr",
#                    "puniform",
#                    "readr",
#                    "shiny",
#                    "shinythemes",
#                    "shinyWidgets",
#                    "stats")
# )
#
# devtools::install_github("JensFuenderich/MetaPipeX/R-Package",
#                          force = TRUE)

# ### load packages
# lapply(c(#"ggplot2",
#          "grDevices",
#          "magrittr",
#          "metafor",
#          "readr",
#          "shiny",
#          "shinythemes",
#          "shinyWidgets",
#          ), library, character.only = TRUE)
library("ggplot2")
library("grDevices")
library("magrittr")
library("metafor")
library("readr")
library("shiny")
library("shinythemes")
library("shinyWidgets")

library("DT", include.only = 'renderDT') # include one function
library("DT", include.only = 'DTOutput') # include one function
library("haven", include.only = 'read_sav') # include one function
library("janitor", include.only = 'compare_df_cols') # include one function
library("MetaPipeX", include.only = 'create_replication_summaries') # include one function
library("MetaPipeX", include.only = 'merge_replication_summaries') # include one function
library("MetaPipeX", include.only = 'meta_analyses') # include one function
library("MetaPipeX", include.only = 'full_pipeline') # include one function
library("puniform", include.only = 'meta_plot') # include one function
library("stats", include.only = 'na.omit') # include one function
library("stats", include.only = 'cor') # include one function

### general imports

MetaPipeX_data_full <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/MetaPipeX_template.csv"))
codebook <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/5_MetaPipeX/codebook_for_meta_pipe_x_data.csv"))

codebook_text_vec <- "This tabular codebook serves to inform the abbreviations used in this shiny app.
If you are trying to understand a column in the data frame, just consult the appropriate line in the codebook.
If you are trying to look for the abbreviation of a term, eg. standard deviation,
just type it in the Search field and all lines containing that word will be displayed."

### helpers

# create a list for checkboxes, etc (in "Reactive Data Table" tab)
Variables_List <- list(
  AnalysisResults = list("Model Estimates (Est)" = "Est",
                         "Tau2" = "__Tau2_",
                         "SE of Tau2" = "SE_Tau2",
                         "Tau" = "Tau_",
                         "Coefficient of Variation" = "CoeffVar",
                         "I2" = "I2_",
                         "H2" = "H2_",
                         "QE" = "QE_",
                         "QEp" = "QEp_",
                         " " = "exclude"
  ),
  Statistics = list("Control Mean" = "C_M",
                    "Treatment Mean" = "T_M",
                    "Control SD" = "C_SD",
                    "Treatment SD" = "T_SD",
                    "pooled SD" = "pooled_SD",
                    "MD" = "_MD",
                    "SMD" = "_SMD",
                    " " = "exclude"
  ),
  Sample_Size = list("N" = "_N",
                     "K" = "_K",
                     " " = "exclude"
  )
)


