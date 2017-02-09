# R package: crftools

![](https://i.imgur.com/SPXkoJE.jpg)

> Put your heart, mind, and soul into even your smallest acts. This is the secret of success. - *Swami Sivananda*

이것은 R 패키지 `crftools` 입니다. 

## Description

- 현재는 하나의 function만 존재하며 CRF에서 추출한 csv와 data dictionary의 비교를 통해 서로 기술한 variables를 갖고 있는지를 평가합니다.
- Dictionary 파일 형식은 xlsx 만 지원하며 필수적으로 가져야 하는 Tab 이름은 `List`, `SUFFIX`, `EXCEPT`입니다.
    - `List`: CDISC SDTM Domain 명을 2nd column에 기록합니다.
    - `SUFFIX`: CRF의 variable name의 마침표 이후의 suffix를 `Suffix` column에 나열합니다.
    - `EXCEPT`: CRF에 들어가는 variable name이지만 dictionary에서 상세히 기술될 필요가 없는 것에 대한 예외를 `VAR` column에 나열합니다.

## Installation

```r
#install.packages("devtools") # if you haven't installed devtools, run this line after deleting the first #
devtools::install_github("asancpt/crftools")
```

## How to use

```r
library(crftools)

CRFcsv = "foo.csv"
Dictionaryxlsx = "bar.xlsx"
FocusCol <- "Cohort1"

(Results <- crfdic(CRFcsv, Dictionaryxlsx, FocusCol))
```

## Direction

- CRF와 관련된 다양한 기능을 추가할 예정입니다. 
- CRAN에 올라가지는 않을 것입니다.

