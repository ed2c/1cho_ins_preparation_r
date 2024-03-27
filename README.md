# 1Cijfer HO preparation for visualization

## Introduction

Welcome to this CEDA repository. The goal of this repository is to prepare the well-known 1CijferHO institution file into a data product that is suitable for visualisation. For an example of a dashboard build upon this data, see the [following CEDA repository](https://github.com/ed2c/1cho_ins_visualisation_tableau).

## Origins and context of data

The well-known 1 CijferHO institution enrollment file is the main data source. Currently a slightly adapted version of this raw data is loaded, this will be changed to output of the [eencijfer package](https://libraries.io/pypi/eencijfer) in the coming months.

This file is register data for each public educational institution that provides higher education, i.e. (applied) universities. The data provider is DUO and the file is accompanied by a lot of supporting documents (see /metadata/data_dictionary_start) explaining the variables. In addition, [DUO has a page for it with contact details](https://duo.nl/zakelijk/hoger-onderwijs/studentenadministratie/bron-controleren/deelnames-en-resultaten-duo-registers.jsp). Finally, UNL and VH are in close contact with DUO and the ministry and have also expertise on the matter/

## Goal and activities

The goal of this repository is to prepare the data, specifically for visualization. This means that the column names and values should be easy to understand and that there should be categorical variables with a limited amount of categories (so they can be used as a colored grouping variable for line and bar charts). This leads to the following activities:

1.  The raw data is enriched with a lot of derived variables. For instance, many of the columns in the raw data are encodings, while supporting files have the character values belonging to the codes. These are added. In addition, derived variables regarding:

    -   Year of study (Student in programme)
    -   Drop-out first year before 1 February
    -   Number of enrollments at institution in given year (dubbele studie)
    -   High school tracks (VWO-profielen)
    -   Gap years (Tussenjaren)
    -   Inflow (Aansluiting)

2.  Raw data from the 1CijferHO 'EOI-Cohort' file, a file derived from the original 1CijferHO institution file, is also used. It contains definitiona about degrees and dropout. These are also transformed to categorical variables with easy to interpret values. In addition a few derived variables are added.

3.  Thirdly, it is enriched with programme data from CROHO. This is used to ensure correct integration of the enrollments with the cohort data. In addition, CROHO provides the number of EC of a programme and thus the nominal duration. This used to created derived statistics about students' progress.

## Secondary goals

This project is also a demonstration of the usefullness of packages from the [vusaverse](https://github.com/vusaverse/). The vusaverse is a series of packages that provide per package specific functionality. Functions from the vusaverse used in this repository are:

-   auditing the data (vvauditor)

-   preparation

    -   map values and basic adaptions (vvconverter)

    -   fill missing data (vvfiller)

-   transpose and aggregate (vvscultpor)

-   dynamic save and load (vvmover)

Currently, much is still in the overall vusa package, this will be more modular in the coming months.

## Usage

When opening the repository renv should start to install all the packages. This can take a few minutes.

A user should update the config.yml at the point of "default". The settings as seen by 'vu' and 'synthetic' can be updated with the file with the paths to data of the institution of choice, and the corresponding institution name and BRIN. Values that are set below default are inherited from synthetic and thereafter vu.

Thereafter the build.R file can be run. This will result in data files in /data subfolders: audited, prepared, combined and finally exported data. A data dictonary will be added to /metadata/data_dictionary_end.

As service to the user, we want to provide a results folder with only the final data and dictionary will be added as well.

## TO DO

-   [ ] Finish checklist (see below), with the exception of content-dependent validations
-   [ ] Use output from the [eencijfer package](https://libraries.io/pypi/eencijfer) as 'raw' data
-   [ ] Make results folder

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
| An instruction file which explains the goal and context                                             | 🛠️     |
| clear structure in-line with best practices for data science                                        | ✅     |
| Data dictionaries at start and end                                                                  | 🛠️     |
| Well styled code ([guide](https://style.tidyverse.org/))                                            | 🛠️     |
| All files are machine-readable (.py, .R, .csv., .yaml, md, qmd).                                    | ✅     |
| All the data files at start and end of the repository are automatically checked by validation rules | ❌     |
| Every repository has synthetic or dummy ‘start’ data                                                | ✅     |
| The main language in a repository is English. This goes for code, comments, and documentation.      | 🛠️     |
| A glossary with all the column names and explanation In English is proviced                         | ⏳     |

</details>

## Contributing

Contributions to vvtableau are welcome! If you encounter any bugs, have feature requests, or would like to contribute code improvements, please open an issue or submit a pull request on the GitHub repository. In addition, if you disagree with the status on the checklist or would like be informed of any updates, let us know!
