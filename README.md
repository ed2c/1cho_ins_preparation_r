# 1Cijfer HO preparation for visualization

## Introduction

Welcome to this CEDA repository. The goal of this repository is to prepare the well-known 1CijferHO institution file into a data product that is suitable for visualisation. For an example of a dashboard build upon this data, see the [following CEDA repository](https://github.com/ed2c/1cho_ins_visualisation_tableau). Working with 1Cijfer has multiple advantages including:

-   Easy to compare with other institutions due to national uniformity
-   High quality, since it is validated by DUO and institutions themselves
-   Elaborate documentation
-   Over 100 variables, several from CBS and DUO so not covered by internal datasets

## Origins and context of data

The well-known 1 CijferHO institution enrollment file is the main data source. The input for this repository is the output of the [eencijfer package](https://libraries.io/pypi/eencijfer).

This file is register data for each public educational institution that provides higher education, i.e. (applied) universities. The data provider is DUO and the file is accompanied by a lot of supporting documents (see /metadata/data_dictionary_start) explaining the variables. In addition, [DUO has a page for it with contact details](https://duo.nl/zakelijk/hoger-onderwijs/studentenadministratie/bron-controleren/deelnames-en-resultaten-duo-registers.jsp). Finally, UNL and VH are in close contact with DUO and the ministry and have also expertise on the matter/

## Goal and activities

The goal of this repository is to prepare the data, specifically for visualization. This means that the column names and values should be easy to understand and that there should be categorical variables with a limited amount of categories (so they can be used as a colored grouping variable for line and bar charts). This leads to the following activities:

1.  The raw data is enriched with a lot of derived variables. For instance, many of the columns in the raw data are encodings, while supporting files have the character values belonging to the codes. These are added. In addition, derived variables regarding:

    -   Year of study (Student in programme)
    -   Drop-out first year before 1 February
    -   Number of enrollments at institution in given year (dubbele studie)
    -   High school tracks (so called "profielen")
    -   Gap years (Tussenjaren)
    -   Inflow (Aansluiting)

2.  Thereafter, it is enriched with programme data from CROHO. CROHO provides the number of EC of a programme and thus the nominal duration. This used to created derived statistics about students' progress.

3.  Consequently, the data is enriched with characteristics regarding 'nominal' success and drop-out.

These variables are made to provide easily insights into the (independent) characteristics of the students (aaansluiting, tussenjaar, profielen, dubbele studie). In turn, these variables help to explain the second group of variables that is made: the target variables around drop-out and study success.

## Secondary (technical) goals

This project is also a demonstration of the usefullness of packages from the [vusaverse](https://github.com/vusaverse/). The vusaverse is a series of packages that provide per package specific functionality. Functions from the vusaverse used in this repository are:

-   auditing the data (vvauditor)

-   preparation

    -   map values and basic adaptions (vvconverter)

    -   fill missing data (vvfiller)

-   dynamic save and load (vvmover)


## Usage

There is a special how_to.qmd file that explains how to use this repository. In addition, the user can run the build.R file

## TO DO

-   [ ] Finish CEDA checklist (see below), with the exception of content-dependent validations
-   [ ] Move calculated fields from combine to prepare
-   [ ] Fix TODO's in pipeline (Control-Shift-F TODO)
-   [ ] Add config for mapping tables for faculty and internal programme name
-   [ ] Check CROHO connection with real data
-   [ ] Improve folder structure (possibly with functions)
-   [ ] Expand how to
-   [ ] Move the loading of the decoding files to the python package
-   [ ] Adapt scripts for CROHO afterwards (still needed for nominal duration)
-   [ ] Create assets based on user needs and 'assets' from python package
-   [ ] Create visualisations based on the data (in Shiny?)

## CEDA general checklist

<details>

<summary>expand</summary>

### Status codes

❌ Out-of-scope

⏳On roadmap, but no concrete plans

🛠️ Currently under construction

✅Done!

| Item                                                                                                | Status |
|------------------------------------------------------|------------------|
| The code runs successfully                                                                          | ✅     |
| There is a config file for at least every institution-specific setting                              | ✅     |
| A build file                                                                                        | ✅     |
| An instruction file which explains the goal and context                                             | ✅     |
| clear structure in-line with best practices for data science                                        | ✅     |
| Data dictionaries at start and end                                                                  | ✅     |
| Well styled code ([guide](https://style.tidyverse.org/))                                            | 🛠️     |
| All files are machine-readable (.py, .R, .csv., .yaml, md, qmd).                                    | ✅     |
| All the data files at start and end of the repository are automatically checked by validation rules | ❌     |
| Every repository has synthetic or dummy ‘start’ data                                                | ✅     |
| The language in a repository is English. This goes for comments and documentation, not descriptions.| ✅     |
| A glossary with all the column names and explanation In English is provided                         | ⏳     |

</details>

## Contributing

Contributions to vvtableau are welcome! If you encounter any bugs, have feature requests, or would like to contribute code improvements, please open an issue or submit a pull request on the GitHub repository. In addition, if you disagree with the status on the checklist or would like be informed of any updates, let us know!
