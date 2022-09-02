#' Function to perform and update resarch for resources on open government base on certain keywords
#'
#' This function is used to research resources on the canadian open government portal
#' (https://open.canada.ca/en) using the `rgovcan` package. It allows the user to perform a
#' using a series of keywords and store the results. The results can then be marked as verified
#' so that the next research will discard them automatically and either show only unverified
#' resources or new resources available on the portal since the last research.
#'
#' For now, verified resources must be added manually in './inst/extdata/verified.csv'.
#'
#' @return A list of resources
#'
#' @export
#'
#' @export
keysearch <- function() {
  # Setup rgovcan
  rgovcan::govcan_setup()

  # Load lists
  key <- load_key()
  verif <- load_verif()
  chk <- load_check()

  # Perform and export searches
  srch <- list()
  for (i in 1:nrow(key)) {

    # Search
    dat <- rgovcan::govcan_search(
      keywords = key$keywords[i],
      records = 1e6,
      format = TRUE
    )

    # Filter
    dat <- cbind(
      dplyr::select(dat, id, title, jurisdiction),
      dat$organization$title
    ) |>
      dplyr::arrange(
        jurisdiction,
        title
      ) |>
      dplyr::rename(
         organization = "dat$organization$title"
      )

    # Update keywords dataset
    key$count_old[i] <- key$count_new[i]
    key$count_new[i] <- nrow(dat)
    key$timestamp[i] <- timestamp()

    # Store in list 
    srch[[i]] <- dat
    
    # Export research
    write.csv(
      dat,
      file = glue::glue("data/data-search/search-{key$key_id[i]}.csv"),
      row.names = FALSE
    )

    # Check which ones still have to be verified
    chk <- data.frame(id = c(chk$id, dat$id)) |>
           unique() |>
           dplyr::filter(!id %in% verif$id)
  }

  # Export updated keywords
  key$count_old[key$count_old == ""] <- "0"
  write.csv(
    key,
    file = "data/data-search/search_keywords.csv",
    row.names = FALSE
  )

  # Export updated data to check
  srch <- dplyr::bind_rows(srch) |>
          unique()
  chk <- dplyr::left_join(chk, srch, by = "id")
  write.csv(
    chk,
    file = "data/data-search/check.csv",
    row.names = FALSE
  )

  # Messages
  nw <- sum(as.numeric(key$count_new))
  old <- sum(as.numeric(key$count_old))
  gp <- nw - old
  if (gp > 0) {
    msgInfo(glue::glue("The search yielded {gp} new resources."))
  }
  if (nrow(chk > 0)) {
    msgInfo(glue::glue("There are {nrow(chk)} resources to check."))
  }
}

# Load keywords dataset
load_key <- function() {
  read.csv(
    "data/data-search/search_keywords.csv",
    colClasses = "character"
  )
}

# Load verified datasets
load_verif <- function() {
  read.csv(
    "data/data-search/verified.csv",
    colClasses = "character"
  )
}

# Load datasets that need to be verified
load_check <- function() {
  read.csv(
    "data/data-search/check.csv",
    colClasses = "character"
  )
}
