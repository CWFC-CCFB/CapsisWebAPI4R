#'
#' A list of plots that can be passed to Capsis Web API
#'
#' @docType data
#'
#' @usage data(STR_RE2_70)
#'
#' @keywords datasets
#'
#' @examples
#' data(STR_RE2_70)
"STR_RE2_70"

VersionWarningStr <- "VersionWarning"
MessageToClientStr <- "MessageToClient"
ShortLicenseStr <- "ShortLicense"

host <- "http://repicea.dynu.net/CapsisSimulation"

#host <- "http://localhost:5300/CapsisSimulation" #for debugging

.onAttach <- function(libname, pkgname) {
  tryCatch(
    {
      status <- CapsisStatus()
      if (VersionWarningStr %in% names(status)) {
        warning(status[[VersionWarningStr]])
      }
      if (MessageToClientStr %in% names(status)) {
        packageStartupMessage(status[[MessageToClientStr]])
      }
      if (ShortLicenseStr %in% names(status)) {
        packageStartupMessage(status[[ShortLicenseStr]])
      }
    },
    error = function(cond) {
      warning("R is unable to connect to Capsis Web API! \n It may be that your internet connection is not working or your firewall does not allow for http requests.")
    }
  )

}


.processHttpRequest <- function(url, myQuery = list()) {
  r <- httr::GET(url, query = myQuery);
  if (r$status_code != 200) {
    stop(httr::content(r, "text"))
  }
  result <- httr::content(r, "text")
  resultJSON <- jsonlite::fromJSON(result)
}

#'
#' Provide the List of Variants
#'
#' @return a vector of characters
#'
#' @export
CapsisGetVariantList <- function() {
  url <- paste(host, "VariantList", sep="/")
  result <- .processHttpRequest(url)
  return (result)
}


#'
#' Provide the species for
#'
#' @param variant a valid variant (string)
#' @param type either All, Broadleaved, or Coniferous
#'
#' @return a vector of characters
#'
#' @export
CapsisGetVariantSpecies <- function(variant, type = "All") {
  if (is.null(variant)) {
    stop("The variant argument cannot be null. See the CapsisGetVariantList function for the possible variants.")
  }
  if (is.null(type)) {
    stop("The type argument cannot be null. It can take the values All, Broadleaved, or Coniferous.")
  }
  url <- paste(host, "VariantSpecies", sep="/")
  result <- .processHttpRequest(url, list(variant = variant, type = type))
  return(result)
}


#'
#' Provide the Different Outputs of Capsis Variant
#'
#' @param variant a character string
#'
#' @return a vector with the different outputs
#' @seealso [CapsisGetVariantList()] for the list of available variants
#'
#' @export
CapsisGetOutputRequestTypes <- function(variant) {
  if (is.null(variant)) {
    stop("The variant argument cannot be null. See the CapsisGetVariantList function for the possible variants.")
  }
  url <- paste(host, "OutputRequestTypes", sep="/")
  result <- .processHttpRequest(url, list(variant = variant))
  return (result)
}

#'
#' Provide a List of Fields Required by a Variant
#'
#' @param variant a valid variant (string)
#' @return a list of data.frame instances containing the information on
#' the fields
#'
#' @export
CapsisGetVariantFields <- function(variant) {
  if (is.null(variant)) {
    stop("The variant argument cannot be null. See the CapsisGetVariantList function for the possible variants.")
  }
  url <- paste(host, "VariantFields", sep="/")
  result <- .processHttpRequest(url, list(variant = variant))
  return(result)
}

#'
#' Provide the Status of OSM Web API
#'
#' @return a list of data.frame instances containing the information on
#' the fields
#'
#' @export
CapsisStatus <- function() {
  url <- paste(host, "Status", sep="/")
  result <- .processHttpRequest(url, list(clientversion = as.character(utils::packageVersion("CapsisWebAPI4R"))))
  return(result)
}


.convertDataFrameToCSVString <- function(dataFrameInstance) {
  outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
  outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
  outputString <- paste(outputVector, collapse = "\r\n")
  return(outputString)
}

#'
#' Provide the Scope of a Variant
#'
#' @param variant a valid variant (string)
#' @return a list of variables with their scope
#'
#' @export
CapsisGetVariantScope <- function(variant) {
  if (is.null(variant)) {
    stop("The variant argument cannot be null. See the CapsisGetVariantList function for the possible variants.")
  }
  url <- paste(host, "VariantScope", sep="/")
  result <- .processHttpRequest(url, list(variant = variant))
  return (result)
}

#'
#' Run a Simulation with Capsis
#'
#'
#' @param data a string in CSV format that represents the input data to run the
#' simulation on. The colnames should match the required fieldnames.
#' @param outputRequestList an object of type OutputRequestList that contains
#' the output data requested
#' @param variant a string containing the variant name to use for simulation
#' @param years an int containing the number of years the simulation should use
#' @param initialYear the initial date of the simulation
#' @param isStochastic a logical (true to enable stochastic simulation)
#' @param nbRealizations an integer standing for the number of realizations in
#' case of stochastic simulation [1-500]
#' @param climateChange astring indicating the climate change scenario. One
#' among these: NO_CHANGE, RCP2_6, RCP4_5, RCP6_0, or RCP8_5
#' @param applicationScale a string indicating the scale of the simulation.
#' Either Stand or FMU.
#'
#' @return a SimulationResult instance
#'
#' @export
CapsisSimulate <- function(data, outputRequestList, variant, years, initialYear, isStochastic, nbRealizations, climateChange, applicationScale) {
  if (is.null(variant)) {
    stop("The variant argument cannot be null. See the CapsisGetVariantList function for the possible variants.")
  }
  if (is.null(outputRequestList) | !methods::is(outputRequestList, "OutputRequestList")) {
    stop("The outputRequestList argument must be an instance of OutputRequestList class!\r Use the CFSCommonGYModelWebAPI4R::new_OutputRequestList() function.")
  }
  outputRequestListJSON <- outputRequestList$toJSONString()
  fieldMatches <- .GetFieldMatches(data, CapsisGetVariantFields(variant))
  fieldMatchesJSON <- jsonlite::toJSON(fieldMatches, auto_unbox=TRUE)
  csvData <- .convertDataFrameToCSVString(data)

  url <- paste(host, "Simulate", sep="/")
  r <- httr::POST(url,
                  query = list(years = as.character(years),
                                variant = variant,
                                initialYear = as.character(initialYear),
                                isStochastic=as.logical(isStochastic),
                                nbRealizations = as.integer(nbRealizations),
                                climateChange = as.character(climateChange),
                                applicationScale = as.character(applicationScale),
                                fieldMatches=as.character(fieldMatchesJSON)),
                  body = list(data=csvData, output=outputRequestListJSON),
                  encode = "multipart" )

  # if the CapsisWebAPI cannot launch this request, it informs us by returning code HTTP error 429 "too many requests".  in this case, return NULL
  if (r$status_code == 429) {
    return (NULL)
  }

  if (r$status_code != 200) {
    stop(httr::content(r, "text"))
  }
  result <- httr::content(r, "text")

  taskId <- jsonlite::fromJSON(result)

  status <- CapsisGetTaskStatus(taskId)
  firstTime <- T
  while (status$code == "IN_PROGRESS")
  {
    if (firstTime) {
      messageStr <- status$code
      firstTime <- F
    } else {
      messageStr <- "."
    }
    message(messageStr, appendLF = F)
    Sys.sleep(2)
    status <- CapsisGetTaskStatus(taskId)
  }

  message(paste(status$code))

  if (status$code != "COMPLETED") {
    stop(paste("Error while processing with error code:", status$result))
  }

  return(status$result)
}

.GetFieldMatches <- function(dataFrameObj, fieldList) {
  availableFields <- colnames(dataFrameObj)
  matches <- list()
  for (i in 1:nrow(fieldList)) {
    fieldName <- fieldList[i,"name"]
    index <- which(availableFields == fieldName)
    if (length(index) == 0) {
      if (!fieldList[i, "isOptional"]) {
        stop(paste("Field", fieldName, "cannot be found! Check the required field using capsis$VariantFields(variant) function."))
      }
    } else {
      matches[[fieldName]] <- availableFields[index]
    }
  }
  return(matches)
}

CapsisGetTaskStatus <- function(taskID) {
  if (is.null(taskID)) {
    stop("The taskID argument cannot be null.")
  }
  url <- paste(host, "TaskStatus", sep="/")
  result <- .processHttpRequest(url, list(taskID = taskID))
  simResult <- NULL

  if (result$status == "COMPLETED") {
    simResult <- CFSCommonGYModelWebAPI4R::new_SimulationResult(result$result)
  } else if (result$status == "ERROR") {
    return(list(code=result$status, result = result$errorMessage))
  }

  return(list(code=result$status, result = simResult))
}
