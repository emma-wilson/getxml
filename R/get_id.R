#' Get PMCID from DOI
#'
#' @import rcrossref
#' @import dplyr
#' @param doi a vector or dataframe column containing digital object identifier(s)
#' @return a dataframe containing the original DOI(s) and the new IDs (PMID and PMCID)
#' @export

get_id <- function(doi){

  # Check input is a vector or dataframe column
  # Vectors and dataframe columns have dim() == NULL
  if (is.null(dim(doi)) == FALSE){
    stop("The data you are trying to parse is not a vector or dataframe column.")
  }

  # Check input for duplicates
  if (length(doi) != length(unique(doi))){
    stop(paste0("Vector of length ", length(doi), " only contains ", length(unique(doi)), " unique DOIs. Please remove duplicates and try again."))
  }

  # Initiate results dataframe
  pmcid <- data.frame(pmcid = as.character(),
                      pmid = as.character(),
                      doi = as.character())

  # Use crossref to get PMCID from DOI
  for (i in 1:length(doi)){
    # Get the PMID from Crossref
    tryCatch(result <- rcrossref::id_converter(doi[i]))
    # Get the results from the list
    result_df <- result$records
    # Check retrieved
    if("status" %in% colnames(result_df)){
      result_error <- data.frame(pmcid = NA, pmid = NA, doi = doi[i])
      pmcid <- rbind(pmcid, result_error)
      print(paste0("(", i, "/", length(doi), ") Could not find ID for DOI: ", doi[i]))
    }else{
      # Bind to results to the existing dataframe
      result_df <- result_df %>% select(pmcid, pmid, doi)
      pmcid <- rbind(pmcid, result_df)
      print(paste0("(", i, "/", length(doi), ") Found ID for DOI: ", doi[i]))
    }
    # Sleep for 2 seconds
    Sys.sleep(2)
  }

  # Return dataframe
  return(pmcid)

}
