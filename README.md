MACRO REPORT
================
Paulo Ferreira Naibert

Clone this Repository:

``` bash
git clone https://github.com/pfnaibert/macro-report.git
```

## Overview

This repository contains the functions to webscrape data from brazilian
public data of economic variables and automatically make reports based
on that data.

## Packages

``` bash
pacman -S r pandoc pandoc--citeproc
```

``` r
install.packages(c("stargazer", "xtable", "dygraphs", "rmarkdown"))
```

## Functions

The functions can be found [here](./funs/)

## URLS

  - [ipea](http://www.ipea.gov.br/portal/)
  - [ibge](https://www.ibge.gov.br/)
  - [fee](https://www.fee.rs.gov.br/)

BACEN - [bacen](https://www.bcb.gov.br/pt-br/) -
[bacen-estatisticas](https://www.bcb.gov.br/estatisticas) -
[bacen-tabelas-especiais](https://www.bcb.gov.br/estatisticas/tabelasespeciais)
- [bacen-ri](https://www.bcb.gov.br/publicacoes/ri)

## Databases

  - [sgs](https://www3.bcb.gov.br/sgspub/)
  - [sidra](https://sidra.ibge.gov.br/home/)
  - [ipeadata](http://ipeadata.gov.br/)

## Variables

| Code | Description                                  | Unity                                       | frequency | Initial Date | Final Date | Source       |
| ---- | -------------------------------------------- | ------------------------------------------- | --------- | ------------ | ---------- | ------------ |
| 7    | Bovespa - índice                             | Pontos                                      | D         | 29/12/1989   | 26/02/2018 | BM\&FBOVESPA |
| 8    | Bovespa - Volume                             | R$ (milhões)                                | D         | 29/12/1989   | 26/02/2018 | BM\&FBOVESPA |
| 7845 | Bovespa - índice mensal                      | Pontos                                      | M         | 31/07/1983   | jan/2018   | BM\&FBOVESPA |
| 7849 | Valor das empresas listadas na Bovespa       | u.m.c. (milhões)                            | M         | 31/01/1996   | jan/2018   | BM\&FBOVESPA |
| 7850 | Quantidade de companhias listadas na Bovespa | Unidades                                    | M         | 31/12/1996   | jan/2018   | BM\&FBOVESPA |
| 12   | Taxa de juros - CDI                          | % a.d.                                      | D         | 06/03/1986   | 26/02/2018 | Cetip        |
| 4389 | Taxa de juros - CDI                          | anualizada base 252 % a.a.                  | D         | 06/03/1986   | 26/02/2018 | BCB-Demab    |
| 4391 | Taxa de juros - CDI                          | acumulada no mês % a.m.                     | M         | 31/07/1986   | fev/2018   | BCB-Demab    |
| 4392 | Taxa de juros - CDI                          | acumulada no mês anualizada base 252 % a.a. | M         | 31/07/1986   | fev/2018   | BCB-Demab    |

## USAGE

See [DEMO](./DEMOS/) files.

# TODO

  - Document Functions (R and html nb)
