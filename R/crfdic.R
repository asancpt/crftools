#' Compare variable names between CRF and data dictionary
#'
#' \code{crfdic} uses CRF-derived csv file and a data dictionary excel to compare
#' variable names of each file
#'
#' @param CRFcsv A filename of CRF csv file, usually exported from PDFCRF
#' @param Dictionaryxlsx A filename of data dictionary xlsx file
#' @importFrom tidyr separate
#' @import dplyr
#' @import xlsx
#' @export
#' @return List of output data of comparison between CRF and data dictionary
#' @examples
#'\dontrun{
#'crfdic(CRFcsv = "foo.csv", Dictionaryxlsx = "foo.xlsx")
#'}

crfdic <- function(CRFcsv, Dictionaryxlsx){
    Output <- list()
    ## Data dictionary CRFcsv
    Domain.list <- read.xlsx(Dictionaryxlsx, sheetName = "List", startRow=2,
                             stringsAsFactors = FALSE, encoding="UTF-8", header = FALSE) %>%
        rename(Num = X1, Domain2 = X2, KoreanDomain = X3) %>%
        mutate(Domain2 = trimws(gsub("\\(.*$", "", Domain2))) %>%
        filter(!is.na(Domain2)) %>%
        mutate(Domain = paste(Num, Domain2, sep = "."))

    Domain.data <- data.frame()
    for (i in 1:dim(Domain.list)[1]){
        DID = Domain.list$Domain[i]
        print(paste0("Reading data dictionary xlsx file - Tab ", DID))
        Raw <- read.xlsx(Dictionaryxlsx, sheetName = DID, startRow = 1, stringsAsFactors = FALSE, encoding="UTF-8", colIndex = 1:6)
        Raw$VAR <- trimws(Raw$VAR)
        Domain.data <- rbind(Domain.data, data.frame(DOMAIN = DID, Raw))
    }

    EXCEPT <- read.xlsx(Dictionaryxlsx, sheetName = "EXCEPT", startRow = 1,
                        stringsAsFactors = FALSE, encoding="UTF-8", colIndex = 1:5) %>%
        select(VAR = 1, Prev1 = 2, Prev2 = 3, Focus = 4, VARLABEL = 5) %>%
        filter(Focus == "V")

    ## PDF CSV
    Cohort13raw <-  t(read.csv(CRFcsv, stringsAsFactors = FALSE, header = FALSE))[,1]
    PDF.variable.raw <- data.frame(row.names = NULL, PVAR = Cohort13raw)

    # Consider exception
    #Domain.data %>% View()

    # Subset
    Variable <- c(Domain.data %>% select(VAR) %>% filter(!is.na(VAR)) %>% t() %>% as.vector(),
                  EXCEPT %>% select(VAR) %>% filter(!is.na(VAR)) %>% t() %>% as.vector())

    PDF.variable <- suppressWarnings(PDF.variable.raw %>% filter(PVAR != "") %>%
        tidyr::separate(col = PVAR, into = c("PVAR", "At"), sep = "\\."))

    df1 = unique(na.omit(PDF.variable$PVAR))
    df2 = unique(Variable)

    ## ----include = FALSE-----------------------------------------------------
    Suffix = read.xlsx(Dictionaryxlsx, sheetName = "SUFFIX", startRow=1,
                       stringsAsFactors = FALSE, encoding="UTF-8", colIndex = 1:6) %>%
        select(Section = 1, Prev1 = 2, Prev2 = 3, Focus = 4, Category = 5, Suffix = 6) %>%
        filter(Focus == "V")
    Suffix.df1 = unique(na.omit(PDF.variable$At))
    Suffix.df2 = unique(na.omit(Suffix$Suffix))

    Output$CRF.only.Suffix <- setdiff(Suffix.df1, Suffix.df2)

    ### Dictionary에는 있는데 PDFCRF에 없는 suffix (Total : `r length(setdiff(Suffix.df2,Suffix.df1))`)

    Output$Dictionary.only.Suffix <- setdiff(Suffix.df2, Suffix.df1)

    ### 공통적으로 있는 suffix: 교집합 (Total : `r length(intersect(Suffix.df1, Suffix.df2))`)

    intersect(Suffix.df1, Suffix.df2)

    ### 모든 suffix: 합집합 (Total : `r length(union(Suffix.df1, Suffix.df2))`)
    union(Suffix.df1, Suffix.df2)

    ### Suffix 현황
    Output$Suffix.Summary <- paste("CRF.only =", length(setdiff(Suffix.df1,Suffix.df2)),
                                     "/ Dictionary only = ", length(setdiff(Suffix.df2,Suffix.df1)),
                                     "/ Intersect = ", length(intersect(Suffix.df1,Suffix.df2)),
                                     "/ Union = ", length(union(Suffix.df1,Suffix.df2)))
    ### PDFCRF에 있는데 Dictionary에는 없는 Variable (Total : `r length(setdiff(df1,df2))`)
    Output$CRF.only.Variable <- setdiff(df1, df2)

    ### Dictionary에는 있는데 PDFCRF에 없는 Variable (Total : `r length(setdiff(df2,df1))`)
    Output$Dictionary.only.Variable <- setdiff(df2, df1)

    ### 공통적으로 있는 Variable: 교집합 (Total : `r length(intersect(df1, df2))`)
    intersect(df1, df2)

    ### 모든 variable: 합집합 (Total : `r length(union(df1, df2))`)
    union(df1, df2)
    Output$Variable.Summary <- paste("CRF.only =", length(setdiff(df1,df2)),
                    "/ Dictionary only = ", length(setdiff(df2,df1)),
                    "/ Intersect = ", length(intersect(df1,df2)),
                    "/ Union = ", length(union(df1,df2)))
    return(Output)
}
