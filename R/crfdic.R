#' Compare variable names between CRF and data dictionary
#'
#' \code{crfdic} uses CRF-derived csv file and a data dictionary excel to compare
#' variable names of each file
#'
#' @param CRFcsv A filename of CRF csv file, exported from PDFCRF
#' @param Dictionaryxlsx A filename of data dictionary xlsx file
#' @param Focus Column name of data dictionary which focuses analysis
#' @importFrom tidyr separate
#' @import dplyr
#' @import xlsx
#' @export
#' @return List of output data of comparison of variables between a CRF-derived csv file and a data dictionary
#' @examples
#'\dontrun{
#'crfdic(CRFcsv = "foo.csv", Dictionaryxlsx = "foo.xlsx", Focus = NULL)
#'}

crfdic <- function(CRFcsv, Dictionaryxlsx, FocusCol){
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

    for (i in 1:dim(Domain.list)[1]){
        DID = Domain.list$Domain[i]
        print(paste0("Reading data dictionary xlsx file - Tab ", DID))
        DomainRaw <- read.xlsx(Dictionaryxlsx, sheetName = DID, startRow = 1, stringsAsFactors = FALSE, encoding="UTF-8")
        DomainRaw$VAR <- trimws(DomainRaw$VAR)
        names(DomainRaw)[names(DomainRaw) == FocusCol] <- "Focus"
        Raw <- DomainRaw %>%
            select(VAR, Scope = Focus) %>%
            filter(Scope == "V")
        Domain.data <- rbind(Domain.data, data.frame(DOMAIN = DID, Raw))
    }

    # Exception
    DomainEXCEPT <- read.xlsx(Dictionaryxlsx, sheetName = "EXCEPT", startRow = 1,
                        stringsAsFactors = FALSE, encoding="UTF-8")
    names(DomainEXCEPT)[names(DomainEXCEPT) == FocusCol] <- "Focus"
    EXCEPT <- DomainEXCEPT %>%
        select(VAR, Scope = Focus) %>%
        filter(Scope == "V")

    ## PDFCSV
    transposeCRFcsv <-  t(read.csv(CRFcsv, stringsAsFactors = FALSE, header = FALSE))[,1]
    PDF.variable.raw <- data.frame(row.names = NULL, PVAR = transposeCRFcsv)

    # Subset
    Variable <- c(Domain.data %>% select(VAR) %>% filter(!is.na(VAR)) %>% t() %>% as.vector(),
                  EXCEPT %>% select(VAR) %>% filter(!is.na(VAR)) %>% t() %>% as.vector())

    PDF.variable <- suppressWarnings(PDF.variable.raw %>% filter(PVAR != "") %>%
        tidyr::separate(col = PVAR, into = c("PVAR", "At"), sep = "\\."))

    DomainSuffix = read.xlsx(Dictionaryxlsx, sheetName = "SUFFIX", startRow=1,
                       stringsAsFactors = FALSE, encoding="UTF-8")
    names(DomainSuffix)[names(DomainSuffix) == FocusCol] <- "Focus"
    Suffix <- DomainSuffix %>%
        select(Suffix, Scope = Focus) %>%
        filter(Scope == "V")

    Suffix.df1 = unique(na.omit(PDF.variable$At))
    Suffix.df2 = unique(na.omit(Suffix$Suffix))

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

    Output$CRF.only.Variable <- setdiff(df1, df2)
    Output$Dictionary.only.Variable <- setdiff(df2, df1)
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
