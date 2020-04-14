## ----setup,echo=FALSE----------------------------------------------------
# setup chunk
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")),"true")
knitr::opts_chunk$set(purl = NOT_CRAN)
library(rwebstat)

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(kableExtra)
library(magrittr)
library(htmltools)
library(prettydoc)

## ----htmlTemplate, echo=FALSE--------------------------------------------
if (require(htmltools)) {
img <- htmltools::img(src = knitr::image_uri("logo_webstat.png"),
               alt = 'logo',
               style = 'position:absolute; top:0; right:0; padding:10px;')

img_0 <- htmltools::img(src = knitr::image_uri("Nouveau_logo_Banque_de_France.jpg"),
               alt = 'logo',
               style = 'position:absolute; top:0; left:0; padding:10px',
               width = '180px',
               height = '85px;')

htmlhead <- paste0('
<script>
document.write(\'<div class="logos">',img,img_0,'</div>\')
</script>
')

readr::write_lines(htmlhead, path = "header.html") }

## ---- message=FALSE,warning=FALSE,eval=FALSE-----------------------------
#  webstat_client_ID <- "3141592-65359-26535"

## ----message=FALSE, warning=FALSE, include=FALSE,eval=NOT_CRAN-----------
webstat_client_ID <- "40f07568-b031-455d-8eed-2ea2fb642db8"

## ----message=FALSE, warning=FALSE, include=FALSE,eval=NOT_CRAN-----------
proxy_bdf()

## ---- message=FALSE,warning=FALSE,eval=FALSE-----------------------------
#  proxy_bdf()

## ---- message=FALSE,warning=FALSE,eval=FALSE-----------------------------
#  install.packages("rwebstat")

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
datasets <- w_datasets("en") # function call

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(datasets) <- NULL
datasets %>% 
        kable(row.names=NA) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
EXR_series <- w_series_list("EXR") # function call

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(EXR_series) <- NULL
EXR_series %>% 
          head(5) %>%
          kable(row.names=NA) %>% 
          kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
CPP_series_data <- w_data("CPP") # CPP is the smallest Dataset - 2 Series only

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(CPP_series_data) <- NULL
CPP_series_data %>% 
                head(10) %>%
                kable(row.names=NA) %>% 
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
USD_EUR <- w_data(dataset_name = "EXR", series_name = "M.USD.EUR.SP00.E") # exchange rate USD/EUR
USD_EUR <- w_data("EXR.M.USD.EUR.SP00.E")

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(USD_EUR) <- NULL
USD_EUR %>% 
        head(10) %>%
        kable(row.names=NA) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
s1 <- w_search(keyword="monetary",language="en")

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(s1) <- NULL
s1 %>% 
  head(5) %>%
  kable(row.names=NA) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
s2 <- w_search(keyword="\\wary",language="en") # use regexp to capture everything finising with "ary"

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(s2) <- NULL
s2 %>% 
  head(5) %>%
  kable(row.names=NA) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
s3 <- w_search("EXR",keyword="dollar",fixed=TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(s3) <- NULL
s3 %>% 
   head(5) %>%
   kable(row.names=NA) %>% 
   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
USD_EUR <- w_data("EXR.M.USD.EUR.SP00.E",language="fr")
USD_EUR_meta <- w_meta(USD_EUR)

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(USD_EUR_meta) <- NULL
USD_EUR_meta %>% 
                kable(row.names=NA) %>% 
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
USD_EUR <- w_data("EXR.M.USD.EUR.SP00.E",language="en")
USD_EUR_meta <- w_meta(USD_EUR)

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(USD_EUR_meta) <- NULL
USD_EUR_meta %>% 
                kable(row.names=NA) %>% 
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
EXR_STRUCT <- w_structure("EXR",language="en")
class(EXR_STRUCT)

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
names(EXR_STRUCT)

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
EXR_STRUCT_dimensions <- EXR_STRUCT$keyFamily$dimensions[,1]

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(EXR_STRUCT_dimensions) <- NULL
EXR_STRUCT_dimensions %>% 
                      kable(row.names=NA) %>%
                      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
Series_dollar <- w_search("EXR",keyword="dollar",language="fr",fixed=TRUE)
dim(Series_dollar)

## ----include = FALSE-----------------------------------------------------
len <- 24

## ----include = FALSE,eval=NOT_CRAN---------------------------------------
len <- dim(Series_dollar)[1]

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN--------------------------
Series <- Series_dollar$SeriesKey

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN--------------
rownames(Series) <- NULL
Series_p = data.frame(Series_dollar$SeriesKey,Series_dollar$Title)
Series_p %>% 
          kable(row.names=NA) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=FALSE-----------------------------
#  Series_Data_list <-  lapply(Series,w_data)

