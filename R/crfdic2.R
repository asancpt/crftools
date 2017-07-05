#' Compare variable names between CRF and data dictionary 2
#'
#' \code{crfdic} uses CRF-derived csv file and a data dictionary excel to compare
#' variable names of each file
#'
#' @param CRFcsv A filename of CRF csv file, exported from PDFCRF
#' @param Dictionaryxlsx A filename of data dictionary xlsx file mandatorily containing tabs of List, SUFFIX, EXCEPT
#' @param FocusCol Column name of data dictionary which focuses analysis
#'
#' @export
#' @importFrom tidyr separate
#' @import dplyr
#' @import xlsx
#' @import readr
#'
#' @return List of output data of comparison of variables between a CRF-derived csv file and a data dictionary
#' @examples
#'\dontrun{
#'crfdic(CRFcsv = "foo.csv", Dictionaryxlsx = "foo.xlsx", Focus = NULL)
#'}

crfdic2 <- function(CRFcsv, Dictionaryxlsx, FocusCol = NULL){

  # Blank variables
  Output <- list()
  Domain.data <- data.frame()

  ## Data dictionary
  Domain.list <- read.xlsx(Dictionaryxlsx, sheetName = "List", startRow=2,
                           stringsAsFactors = FALSE, encoding="UTF-8", header = FALSE) %>%
    rename(Num = X1, Domain2 = X2, KoreanDomain = X3) %>%
    mutate(Domain2 = trimws(gsub("\\(.*$", "", Domain2))) %>%
    filter(!is.na(Domain2)) %>%
    mutate(Domain = paste(Num, Domain2, sep = "."))

  for (i in 1:nrow(Domain.list)){
    DID <- Domain.list$Domain[i]
    print(paste0("Reading data dictionary xlsx file - Tab ", DID))
    DomainRaw <- read.xlsx(Dictionaryxlsx, sheetName = DID, startRow = 1, stringsAsFactors = FALSE, encoding="UTF-8")
    DomainRaw$VAR <- trimws(DomainRaw$VAR)

    if (is.null(FocusCol)) {
      DomainRaw <- DomainRaw %>% mutate(Focus = "V")
    }  else {
      names(DomainRaw)[names(DomainRaw) == FocusCol] <- "Focus"
    }

    Raw <- DomainRaw %>%
      select(VAR, Scope = Focus, VARLABEL) %>%
      filter(Scope == "V")
    Domain.data <- rbind(Domain.data, data.frame(DOMAIN = DID, Raw))
  }

  # Exceptions
  DomainEXCEPT <- read.xlsx(Dictionaryxlsx, sheetName = "EXCEPT", startRow = 1,
                            stringsAsFactors = FALSE, encoding="UTF-8")
  if (is.null(FocusCol)) {
    DomainEXCEPT <- DomainEXCEPT %>% mutate(Focus = "V")
  }  else {
    names(DomainEXCEPT)[names(DomainEXCEPT) == FocusCol] <- "Focus"
  }
  EXCEPT <- DomainEXCEPT %>%
    select(VAR, Scope = Focus) %>%
    filter(Scope == "V")

  # Subset -
  Variable <- bind_rows(
    Domain.data %>% select(VAR),
    EXCEPT %>% select(VAR)
  ) %>%
    ## EXCEPTION ; This should be resolved. 2017-04-21 Commented out due to error
    mutate(VAR = sub(pattern = "COVAL\\.", replacement = "COVAL_", VAR)) %>%
    mutate(VAR = sub(pattern = "\\.", replacement = "#", VAR)) %>%
    filter(!is.na(VAR)) %>% t() %>% as.vector()

    # mutate(VAR = gsub(pattern = "\\.5h$", replacement = "_5h", VAR)) %>%
    # mutate(VAR = gsub(pattern = "\\.25h$", replacement = "_25h", VAR)) %>%
    # mutate(VAR = gsub(pattern = "\\.75h$", replacement = "_75h", VAR)) %>%

  # PDF-derived VAR ----

  pvarnames <- c(names(read_csv(CRFcsv[1])), names(read_csv(CRFcsv[2])))
  PDF.variable.raw <- tibble(PVAR = pvarnames) %>%
    mutate(PVAR = sub(pattern = "COVAL\\.", replacement = "COVAL_", PVAR)) %>%
    mutate(PVAR = sub(pattern = "\\.", replacement = "#", PVAR)) %>%
    filter(PVAR != "X1") # X1 is converted from blank column name.

    # EXCEPTION ; This should be resolved. 2017-04-21 Commented out due to error
    # mutate(PVAR = gsub(pattern = "\\.5h$", replacement = "_5h", PVAR)) %>%
    # mutate(PVAR = gsub(pattern = "\\.25h$", replacement = "_5h", PVAR)) %>%
    # mutate(PVAR = gsub(pattern = "\\.75h$", replacement = "_5h", PVAR))

  PDF.variable <- suppressWarnings(
    PDF.variable.raw %>%
      filter(PVAR != "") %>%
      tidyr::separate(col = PVAR, into = c("PVAR", "At"), sep = "#"))
      #tidyr::separate(col = PVAR, into = c("PVAR", "At"), sep = "\\."))

  # Suffix and variables from PDF-derived VAR

  DomainSuffix <- read.xlsx(Dictionaryxlsx, sheetName = "SUFFIX", startRow=1,
                            stringsAsFactors = FALSE, encoding="UTF-8") %>%
    mutate(Suffix = gsub(pattern = "COVAL\\.", replacement = "COVAL_", Suffix))

  ## EXCEPTION ; This should be resolved.
  #mutate(Suffix = sub(pattern = "\\.", replacement = "##", Suffix))
  #mutate(Suffix = gsub(pattern = "\\.5h$", replacement = "_5h", Suffix))
  #mutate(Suffix = gsub(pattern = "\\.25h$", replacement = "_25h", Suffix)) %>%
  #mutate(Suffix = gsub(pattern = "\\.75h$", replacement = "_75h", Suffix))

  if (is.null(FocusCol)) {
    DomainSuffix <- DomainSuffix %>% mutate(Focus = "V")
  }  else {
    names(DomainSuffix)[names(DomainSuffix) == FocusCol] <- "Focus"
  }

  Suffix <- DomainSuffix %>%
    select(Suffix, Scope = Focus) %>%
    filter(Scope == "V")

  Suffix.df1 <- sub(pattern = "#", replacement = "\\.", unique(na.omit(PDF.variable$At)))
  Suffix.df2 <- sub(pattern = "#", replacement = "\\.", unique(na.omit(Suffix$Suffix)))

  Output$CRF.only.Suffix <- setdiff(Suffix.df1, Suffix.df2)
  Output$Dictionary.only.Suffix <- setdiff(Suffix.df2, Suffix.df1)
  Output$Suffix.Summary <-  data.frame(
    Table = "Suffix",
    Parameter = c("CRF only", "Dictionary only", "Intersect", "Union"),
    Value = c(length(setdiff(Suffix.df1,Suffix.df2)),
              length(setdiff(Suffix.df2,Suffix.df1)),
              length(intersect(Suffix.df1,Suffix.df2)),
              length(union(Suffix.df1,Suffix.df2)))
  )

  df1 = unique(na.omit(PDF.variable$PVAR))
  df2 = unique(Variable)

  Output$CRF.only.Variable <- sub(pattern = "#", replacement = "\\.", setdiff(df1, df2))
  Output$Dictionary.only.Variable <- sub(pattern = "#", replacement = "\\.", setdiff(df2, df1))

  #   Output$Dictionary.only.Variable.Label <- Domain.data[Domain.data$VARLABEL %in% setdiff(df2, df1), ]

  Output$Variable.Summary <-  data.frame(
    Table = "Variable",
    Parameter = c("CRF only", "Dictionary only", "Intersect", "Union"),
    Value = c(length(setdiff(df1,df2)),
              length(setdiff(df2,df1)),
              length(intersect(df1,df2)),
              length(union(df1,df2)))
  )
  return(Output)
}
