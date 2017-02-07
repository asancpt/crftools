# R package: crftools

> When people think about computer science, they imagine people with pocket protectors and thick glasses who code all night.
> - *Marissa Mayer*

이 패키지는 crftools입니다. 

## Description

- 현재는 하나의 function만 존재하며 CRF에서 추출한 csv와 data dictionary의 비교를 통해 서로 적합한 variables를 기술하고 있는지를 평가합니다.

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

crfdic(CRFcsv, Dictionaryxlsx)
```

## Direction

- CRF와 관련된 다양한 기능을 추가할 예정입니다. 
- CRAN에 올라가지는 않을 것입니다.

