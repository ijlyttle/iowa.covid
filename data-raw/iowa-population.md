
The purpose of this document is to download the spreadsheet for Iowa
population by county, then turn that into a package-dataset.

``` r
library("readxl")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("stringr")
library("forcats")
library("steward")
```

``` r
filename <- tempfile(fileext = ".xls")

download.file(
  url = "https://www.icip.iastate.edu/sites/default/files/uploads/tables/population/popest-annual.xls",
  destfile = filename
)
```

We read the 2019 estimates of population, then create some new
varaibles.

``` r
iowa_county_population <-
  read_xls(
    path = filename, 
    sheet = "Counties", 
    range = "A7:M107"
  ) %>%
  transmute(
    fips = Fips,
    county = str_replace(Area, " County, Iowa", ""),
    population = `2019`
  ) %>%
  filter(fips > 19) %>%
  arrange(population) %>%
  mutate(
    cumulative_population = cumsum(population),
    quantile_population = cumulative_population/max(cumulative_population),
    population_group = cut(
      quantile_population, 
      breaks = c(0, 0.25, 0.50, 0.78, 1),
      labels = c("small", "mid-small", "mid-large", "large")
    ),
    population_group = fct_rev(population_group)
  ) %>%
  arrange(desc(cumulative_population)) %>%
  print()
```

    ## # A tibble: 99 x 6
    ##     fips county   population cumulative_popul… quantile_popula… population_group
    ##    <dbl> <chr>         <dbl>             <dbl>            <dbl> <fct>           
    ##  1 19153 Polk         490161           3155070            1     large           
    ##  2 19113 Linn         226706           2664909            0.845 large           
    ##  3 19163 Scott        172943           2438203            0.773 mid-large       
    ##  4 19103 Johnson      151140           2265260            0.718 mid-large       
    ##  5 19013 Black H…     131228           2114120            0.670 mid-large       
    ##  6 19193 Woodbury     103107           1982892            0.628 mid-large       
    ##  7 19061 Dubuque       97311           1879785            0.596 mid-large       
    ##  8 19169 Story         97117           1782474            0.565 mid-large       
    ##  9 19049 Dallas        93453           1685357            0.534 mid-large       
    ## 10 19155 Pottawa…      93206           1591904            0.505 mid-large       
    ## # … with 89 more rows

Now, we can document the dataset.

``` r
iowa_county_population <-
  iowa_county_population %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "iowa_county_population",
    title = "Iowa population 2019",
    description = "Estimate of Iowa population by county, 2019.",
    sources = list(
      list(
        title = "Iowa Community Indicators Program",
        path = "https://www.icip.iastate.edu/tables/population/counties-estimates"
      )
    )
  ) %>%
  stw_mutate_dict(
    fips = "FIPS code",
    county = "county name",
    population = "population",
    cumulative_population = "population in this and smaller counties",
    quantile_population = "proportion of state population in this and smaller counties",
    population_group = "groups counties by populations such that each group has about a quarter of the state's population"
  ) %>%
  stw_validate()
```

``` r
stw_to_table(iowa_county_population)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jkoqcolpwz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#jkoqcolpwz .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#jkoqcolpwz .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#jkoqcolpwz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#jkoqcolpwz .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#jkoqcolpwz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#jkoqcolpwz .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#jkoqcolpwz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#jkoqcolpwz .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#jkoqcolpwz .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#jkoqcolpwz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#jkoqcolpwz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#jkoqcolpwz .gt_from_md > :first-child {
  margin-top: 0;
}

#jkoqcolpwz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jkoqcolpwz .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#jkoqcolpwz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#jkoqcolpwz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#jkoqcolpwz .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#jkoqcolpwz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#jkoqcolpwz .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#jkoqcolpwz .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#jkoqcolpwz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#jkoqcolpwz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#jkoqcolpwz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#jkoqcolpwz .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#jkoqcolpwz .gt_left {
  text-align: left;
}

#jkoqcolpwz .gt_center {
  text-align: center;
}

#jkoqcolpwz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jkoqcolpwz .gt_font_normal {
  font-weight: normal;
}

#jkoqcolpwz .gt_font_bold {
  font-weight: bold;
}

#jkoqcolpwz .gt_font_italic {
  font-style: italic;
}

#jkoqcolpwz .gt_super {
  font-size: 65%;
}

#jkoqcolpwz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="jkoqcolpwz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal gt_center" style>

IOWA\_COUNTY\_POPULATION

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style>

Iowa population
2019

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Name

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Type

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Description

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Levels

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_right" style="font-style: italic;">

fips

</td>

<td class="gt_row gt_left">

number

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

FIPS code

</p>

</div>

</td>

<td class="gt_row gt_left">

</td>

</tr>

<tr>

<td class="gt_row gt_right gt_striped" style="font-style: italic;">

county

</td>

<td class="gt_row gt_left gt_striped">

string

</td>

<td class="gt_row gt_left gt_striped">

<div class="gt_from_md">

<p>

county name

</p>

</div>

</td>

<td class="gt_row gt_left gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_right" style="font-style: italic;">

population

</td>

<td class="gt_row gt_left">

number

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

population

</p>

</div>

</td>

<td class="gt_row gt_left">

</td>

</tr>

<tr>

<td class="gt_row gt_right gt_striped" style="font-style: italic;">

cumulative\_population

</td>

<td class="gt_row gt_left gt_striped">

number

</td>

<td class="gt_row gt_left gt_striped">

<div class="gt_from_md">

<p>

population in this and smaller counties

</p>

</div>

</td>

<td class="gt_row gt_left gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_right" style="font-style: italic;">

quantile\_population

</td>

<td class="gt_row gt_left">

number

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

proportion of state population in this and smaller counties

</p>

</div>

</td>

<td class="gt_row gt_left">

</td>

</tr>

<tr>

<td class="gt_row gt_right gt_striped" style="font-style: italic;">

population\_group

</td>

<td class="gt_row gt_left gt_striped">

string

</td>

<td class="gt_row gt_left gt_striped">

<div class="gt_from_md">

<p>

groups counties by populations such that each group has about a quarter
of the state’s population

</p>

</div>

</td>

<td class="gt_row gt_left gt_striped">

large, mid-large, mid-small, small

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="4">

Sources:
<a href="https://www.icip.iastate.edu/tables/population/counties-estimates">Iowa
Community Indicators
    Program</a>

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
stw_use_data(iowa_county_population, overwrite = TRUE)
```

    ## ✓ Setting active project to '/Users/sesa19001/Documents/repos/public/graphics-group/iowa.covid'

    ## ✓ Saving 'iowa_county_population' to 'data/iowa_county_population.rda'

    ## ● Document your data (see 'https://r-pkgs.org/data.html')

    ## /Users/sesa19001/Documents/repos/public/graphics-group/iowa.covid/R/data-iowa_county_population.R

    ## ✓ Writing roxygen-documentation to 'R/data-iowa_county_population.R'
