MACRO REPORT
================
Paulo Ferreira Naibert

Overview
--------

This repository contains the functions to webscrape data from brazilian public data of economic variables and automatically make reports based on that data.

Functions
---------

The functions can be found [here](./funs/)

URLS
----

-   [bacen](https://www.bcb.gov.br/pt-br/)
-   [ipea](http://www.ipea.gov.br/portal/)
-   [ibge](https://www.ibge.gov.br/)
-   [fee](https://www.fee.rs.gov.br/)

Databases
---------

-   [sgs](https://www3.bcb.gov.br/sgspub/)
-   [sidra](https://sidra.ibge.gov.br/home/)
-   [ipeadata](http://ipeadata.gov.br/)

Variables
---------

<table style="width:100%;">
<colgroup>
<col width="6%" />
<col width="15%" />
<col width="26%" />
<col width="11%" />
<col width="11%" />
<col width="14%" />
<col width="14%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Code</th>
<th align="left">Description</th>
<th align="left">Unity</th>
<th align="left">frequency</th>
<th align="left">Initial Date</th>
<th align="left">Final Date</th>
<th align="left">Source</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">7</td>
<td align="left">Bovespa - índice</td>
<td align="left">Pontos</td>
<td align="left">D</td>
<td align="left">29/12/1989</td>
<td align="left">26/02/2018</td>
<td align="left">BM&amp;FBOVESPA</td>
</tr>
<tr class="even">
<td align="left">8</td>
<td align="left">Bovespa - Volume</td>
<td align="left">R$ (milhões)</td>
<td align="left">D</td>
<td align="left">29/12/1989</td>
<td align="left">26/02/2018</td>
<td align="left">BM&amp;FBOVESPA</td>
</tr>
<tr class="odd">
<td align="left">7845</td>
<td align="left">Bovespa - índice mensal</td>
<td align="left">Pontos</td>
<td align="left">M</td>
<td align="left">31/07/1983</td>
<td align="left">jan/2018</td>
<td align="left">BM&amp;FBOVESPA</td>
</tr>
<tr class="even">
<td align="left">7849</td>
<td align="left">Valor das empresas listadas na Bovespa</td>
<td align="left">u.m.c. (milhões)</td>
<td align="left">M</td>
<td align="left">31/01/1996</td>
<td align="left">jan/2018</td>
<td align="left">BM&amp;FBOVESPA</td>
</tr>
<tr class="odd">
<td align="left">7850</td>
<td align="left">Quantidade de companhias listadas na Bovespa</td>
<td align="left">Unidades</td>
<td align="left">M</td>
<td align="left">31/12/1996</td>
<td align="left">jan/2018</td>
<td align="left">BM&amp;FBOVESPA</td>
</tr>
<tr class="even">
<td align="left">12</td>
<td align="left">Taxa de juros - CDI</td>
<td align="left">% a.d.</td>
<td align="left">D</td>
<td align="left">06/03/1986</td>
<td align="left">26/02/2018</td>
<td align="left">Cetip</td>
</tr>
<tr class="odd">
<td align="left">4389</td>
<td align="left">Taxa de juros - CDI</td>
<td align="left">anualizada base 252 % a.a.</td>
<td align="left">D</td>
<td align="left">06/03/1986</td>
<td align="left">26/02/2018</td>
<td align="left">BCB-Demab</td>
</tr>
<tr class="even">
<td align="left">4391</td>
<td align="left">Taxa de juros - CDI</td>
<td align="left">acumulada no mês % a.m.</td>
<td align="left">M</td>
<td align="left">31/07/1986</td>
<td align="left">fev/2018</td>
<td align="left">BCB-Demab</td>
</tr>
<tr class="odd">
<td align="left">4392</td>
<td align="left">Taxa de juros - CDI</td>
<td align="left">acumulada no mês anualizada base 252 % a.a.</td>
<td align="left">M</td>
<td align="left">31/07/1986</td>
<td align="left">fev/2018</td>
<td align="left">BCB-Demab</td>
</tr>
</tbody>
</table>

USAGE
-----

See [DEMO](./DEMOS/) files.

TODO
====

-   Document Functions (R and html nb)
