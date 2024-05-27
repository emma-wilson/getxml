#' Get XML from EuropePMC
#'
#' @import dplyr
#' @import europepmc
#' @import xml2
#' @param path a file path to the folder where XMLs should be saved
#' @param pmcid a vector or dataframe column containing PMCIDs
#' @return no objects returned, XML files saved to path specified
#' @export


get_xml_ft <- function(path, pmcid){

  # Check input is a vector or dataframe column
  # Vectors and dataframe columns have dim() == NULL
  if (is.null(dim(pmcid)) == FALSE){
    stop("The data you are trying to parse is not a vector or dataframe column.")
  }

  # Check input for duplicates
  if (length(pmcid) != length(unique(pmcid))){
    stop(paste0("Vector of length ", length(pmcid), " only contains ", length(unique(pmcid)), " unique PMCIDs. Please remove duplicates and try again."))
  }

  # Check PMCIDs begin with PMC
  if(all(startsWith(pmcid, "PMC")) != TRUE){
    stop("Some of the data you are trying to parse may not be PMCIDs. Check and try again.")
  }

  # Print where XL files will be saved
  print(paste0("XML files will be saved to to: ", getwd(), "/", path))

  # Create a folder for XML files if does not already exist
  if(!dir.exists(path)){
    print(paste0("Creating: ", getwd(), "/", path))
  }

  # Retrieve XML full texts
  for (i in 1:length(pmcid)){
    # Try to download XML
    tryCatch({
      xml_result <- europepmc::epmc_ftxt(ext_id = pmcid[i])
      # Save as file if it exists
      xml2::write_xml(xml_result, paste0("xml/",pmcid[i],".xml"))
      # Remove from environment
      rm(xml_result)
      # Print message
      print(paste0("(", i, "/", length(pmcid), ") Downloaded XML file for pmcid:", pmcid[i]))
    }, error = function(e) {
      # Print a message if there's an error
      print(paste0("(", i, "/", length(pmcid), ") Could not download XML file for pmcid:", pmcid[i]))
    })
    # Sleep for 6 seconds
    Sys.sleep(6)
  }

}


