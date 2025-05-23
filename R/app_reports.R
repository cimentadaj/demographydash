#' Generate Demographic Projection Report
#'
#' Creates a PDF report summarizing the demographic projection results, including various plots.
#'
#' @param plot_list A named list of ggplot objects generated by `create_all_report_plots`.
#' @param country The name of the country for which the report is generated.
#' @param start_year The starting year of the projection.
#' @param end_year The ending year of the projection.
#' @param i18n An i18n translation object from the shiny.i18n package.
#' @return The file path to the generated PDF report.
#' @importFrom rmarkdown render
#' @importFrom glue glue
#' @export
generate_demography_report <- function(plot_list, country, start_year, end_year, i18n) {
  # Create a temporary directory for report generation
  report_dir <- tempfile("report")
  dir.create(report_dir)

  # Create the R Markdown template path
  rmd_file <- file.path(report_dir, "demography_report.Rmd")

  # Generate the R Markdown content
  rmd_content <- create_demography_rmd_template(plot_list, country, start_year, end_year, i18n)
  writeLines(rmd_content, rmd_file)

  # Render the report
  output_file <- file.path(report_dir, paste0("Demographic_Projection_Report_", gsub(" ", "_", country), "_", start_year, "-", end_year, ".pdf"))

  print("rmd_file")
  print(rmd_file)
  
  tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_file = output_file,
      output_format = "pdf_document",
      quiet = TRUE
    )
    return(output_file)
  }, error = function(e) {
    message(paste("Error generating report:", e$message))
    # In case of error, copy Rmd to a known location for debugging if desired
    # debug_rmd_path <- file.path(tempdir(), "debug_demography_report.Rmd")
    # file.copy(rmd_file, debug_rmd_path, overwrite = TRUE)
    # message(paste("Rmd file saved to:", debug_rmd_path))
    return(NULL) # Return NULL or handle error as appropriate
  })
}

#' Create R Markdown Template for Demographic Report
#'
#' Generates the R Markdown content string for the demographic report.
#'
#' @param plot_list A named list of ggplot objects.
#' @param country The name of the country.
#' @param start_year The starting year of the projection.
#' @param end_year The ending year of the projection.
#' @param i18n An i18n translation object.
#' @return A character string containing the R Markdown template.
#' @importFrom glue glue
#' @noRd
create_demography_rmd_template <- function(plot_list, country, start_year, end_year, i18n) {

  # # Helper function to translate with fallback to original text
  # translate <- function(text_key, ...) {
  #   if (!is.null(i18n)) {
  #     # Pass additional arguments to i18n$t if provided
  #     translated_text <- tryCatch({
  #       i18n$t(text_key, ...)
  #     }, error = function(e) {
  #       text_key # Fallback to original text key if translation fails
  #     })
  #     return(translated_text)
  #   } else {
  #     return(text_key) # Fallback if i18n object is NULL
  #   }
  # }

  report_title <- paste0(i18n$t("Demographic Projection Report: "), country, " ", start_year, "-", end_year)
  executive_summary_title <- i18n$t("Executive Summary")

  executive_summary_content <- paste0(
    i18n$t("This report presents a demographic projection for"), 
    country, 
    " ", 
    start_year, 
    "-", 
    end_year, 
    " ", 
    i18n$t("including various visualizations of key indicators.")
  )

  plots_section_title <- i18n$t("Projection Visualizations")

  # YAML Header - including setup chunk directly here
  yaml_header_and_setup <- glue::glue(
'---
    title: "{report_title}"
    output:
      pdf_document:
        toc: true
        toc_depth: 2
        number_sections: true
        latex_engine: xelatex
    header-includes: |
        \\usepackage{{booktabs}}
        \\usepackage{{longtable}}
        \\usepackage{{float}}
        \\usepackage{{array}}
        \\usepackage{{placeins}}
        \\setlength{{\\aboverulesep}}{{0.2em}}
        \\setlength{{\\belowrulesep}}{{0.2em}}
        \\setlength{{\\extrarowheight}}{{.5em}}
---

```{{r setup, include=FALSE}}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 5.5
)
library(ggplot2)
library(dplyr)
# Add any other libraries needed by the plots themselves if not globally available
```')

  # Introduction Section
  intro_section <- glue::glue('

# {executive_summary_title}

{executive_summary_content}

\\clearpage

# {plots_section_title}')

  # Dynamically create plot sections
  plot_chunks <- ""
  if (!is.null(plot_list) && length(plot_list) > 0) {
    for (plot_name in names(plot_list)) {
      # Sanitize plot_name for use as a chunk label (alphanumeric and underscores only)
      chunk_label <- gsub("[^a-zA-Z0-9_]", "", plot_name)
      
      # Create a more descriptive title from the plot_name
      # Replace underscores with spaces and capitalize
      plot_title_text <- gsub("_", " ", plot_name)
      plot_title_text <- tools::toTitleCase(plot_title_text)
      translated_plot_title <- i18n$t(plot_title_text)

      plot_chunks <- paste0(plot_chunks, glue::glue(
'

## {translated_plot_title}

```{{r {chunk_label}_plot, fig.cap="{translated_plot_title}"}}
# The plot_list variable is passed from the rmarkdown::render call environment
print(plot_list[["{plot_name}"]]$gg)
```

\\FloatBarrier'))
    }
  } else {
    plot_chunks <- paste0("\n\n", i18n$t("No plots available to display."))
  }

  # Combine all parts
  rmd_full_content <- paste0(yaml_header_and_setup, intro_section, plot_chunks)
  
  return(rmd_full_content)
} 