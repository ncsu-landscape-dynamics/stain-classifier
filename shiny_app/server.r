library(shinytest)
library(shiny)
library(poppr)
library(reshape2)
library(dplyr)
library(magrittr)

bruvos_match_threshold <-  0.09929323
k <- 1

# TODO: Low Priority: Consider setting some columns as factors
database <-
  read.table("data/reference_data.txt", header = TRUE, encoding = "UTF-8",
             stringsAsFactors = FALSE, colClasses = c(rep("character", 20)))
descriptions <-
  read.table("data/descriptions.txt", header = FALSE, sep = "\t",
             stringsAsFactors = FALSE)

 load_user_csv <- function(user_csv) {
   tryCatch(read.csv(user_csv, strip.white = TRUE, stringsAsFactors = FALSE,
                     colClasses = c(rep("character", 20)), header = TRUE),
            warning = function(cond) {
                       output_warn <-
                       print("We were unable to parse your CSV file. \n
                              This could be caused by a few different things:
                              \t - Check for extraneous commas in your file.
                              \t - Make sure there is a new line at the end of your file.
                              \t   To do this, open your CSV in a text editor and hit enter
                              \t   (return) after the last line, then save your file and
                              \t   try again.")
                       validate(
                         need(output_warn == "", print(output_warn))
                       )
                     }
   )
 }

format_input_bruvos <- function(data) {
   metadata <-
     select(data, "Sample", "Genotype", "Region", "Country", "Date", "Host",
            "Missing", "Present")
   tryCatch(
    data %>%
      select("D13", "PinfSSR8", "PinfSSR4", "Pi04", "Pi70", "PinfSSR6", "Pi63",
             "PiG11", "Pi02", "PinfSSR11", "PinfSSR2", "Pi4B") %>%
      df2genind(ploidy = 3, sep = "/", ind.names = metadata[, 1],
                pop = metadata[, 2]),
    warning = function(cond) {
     output_warn <- print("Ploidy is incorrect.")
     validate(
       need(output_warn == "", print(output_warn))
     )
    }
   )
 }

# A matrix of bruvos distances
get_bruvos <- function(query, reference) {
  withProgress(message = "Calculating Bruvos Distance", value = 0.4,
               detail = "This usually takes 5 minutes, please be patient.", {
    return(
      bruvo.between(query, reference,
                    replen = c(2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2),
                    add = TRUE, loss = TRUE) %>%
      as.matrix() %>%
      melt(varnames = c("InputSample", "ReferenceSample"))
    )
  })
}

get_best_match <- function(bruvo_df, user_input) {
  withProgress(message = "Calculating Bruvos Distance", value = 1,
               detail = "This usually takes 5 minutes, please be patient.", {
    return(
     bruvo_df %>%
        filter(InputSample %in% user_input$Sample) %>%
        filter(!(ReferenceSample %in% user_input$Sample)) %>%
        group_by(InputSample) %>%
        slice_min(value, n = k, with_ties = FALSE)
    )
  })
}

# TODO: Split into functions that obey single responsibility principle
clean_up_best_match_table <- function(bestmatch) {
  bestmatch %>%
    left_join(select(database, "Sample", "Genotype", "Region", "Country",
                     "Date", "Host", "Missing", "Present"),
              by = c("ReferenceSample" = "Sample")) %>%
    rename(c("Input Sample" = "InputSample", "Matching Genotype" = "Genotype",
             "Bruvo's Distance" = "value")) %>%
    select("Input Sample", "ReferenceSample", "Matching Genotype", "Region",
           "Country", "Date", "Host", "Bruvo's Distance")
}

weak_bruvos_matches <- function(bruvos_distances, threshold) {
  return(
    bruvos_distances %>%
      filter(`Bruvo's Distance` > threshold)
  )
}

strong_bruvos_matches <- function(bruvos_distances, threshold) {
  return(
    bruvos_distances %>%
      filter(`Bruvo's Distance` < threshold)
  )
}

as_no_matches_string <- function(bruvos_distances) {
  bruvos_strings <-
  bruvos_distances %>%
    # Paste the name and the rest of the string
    mutate(no_match_string =
             paste("No close matches found for sample", `Input Sample`, "."))
  # join the elements of the column with a newline
  return(paste(bruvos_strings$no_match_string, sep = "\n", collapse = "\n"))
}



shinyServer(
  function(input, output) {
    calculation <- reactive({

      input_file <- input$SSRinput

      validate(
        need(!is.null(input_file), "Please select a data set.")
      )

      user_input <- load_user_csv(input_file$datapath)
      validate(
        need(identical(c("Sample", "Genotype", "Region", "Country", "Date",
                         "Host", "D13", "PinfSSR8", "PinfSSR4", "Pi04", "Pi70",
                         "PinfSSR6", "Pi63", "PiG11", "Pi02", "PinfSSR11",
                         "PinfSSR2", "Pi4B", "Missing", "Present"),
                       colnames(user_input)), "Header is incorrect.")
      )

      if (is.null(input_file))
        return(NULL)

      # Run calculation
      reference <- format_input_bruvos(database)
      validate(
          need(format_input_bruvos(user_input), "Ploidy is incorrect.")
      )
      query <- format_input_bruvos(user_input)
      return(
        get_bruvos(query, reference) %>%
        get_best_match(user_input) %>%
        clean_up_best_match_table() %>%
          right_join(user_input, by = c("Input Sample" = "Sample")) %>%
          select("Input Sample", "Matching Genotype", "Genotype",
                 "Bruvo's Distance")
      )
    })

    output$closestmatches <- renderTable({
      calculation() %>%
        strong_bruvos_matches(bruvos_match_threshold)
      })

    output$no_match <- renderText({
      calculation() %>%
        weak_bruvos_matches(bruvos_match_threshold) %>%
        as_no_matches_string
      })


    output$detail <- renderTable({
      # Table that has columns: sample name, description
      # The sample name comes from: calculation(), a reactive variable, in that
      # object the column is called `Input Sample`. `Matching_Genotype`
      # description comes from descriptions df, matching column is `V1` in
      # descriptions. We are matching genotype from these two tables.

      if (is.null(calculation())) {
        return(NULL)
      } else {
        return(
          descriptions %>%
            # Combine columns
            mutate(Description = paste(V3, V4, sep = " ")) %>%
            # Join tables based on matching genotypes
            inner_join(calculation() %>%
                         strong_bruvos_matches(bruvos_match_threshold),
                       by = c("V1" = "Matching Genotype"), copy = TRUE) %>%
            # Subset table to get only rows of interest
            select(`Input Sample`, `Description`)
        )
      }
    })
  })
