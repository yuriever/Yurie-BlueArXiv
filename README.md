# Yurie/BlueArXiv

A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs.

This paclet uses the following APIs:

* [arXiv API](https://info.arxiv.org/help/api/index.html)

* [INSPIRE REST API](https://github.com/inspirehep/rest-api-doc)

## Install

Install via the Wolfram Language Paclet Repository (may need `PacletSiteUpdate@PacletSites[]`):

``` wl
PacletInstall["Yurie/BlueArXiv"]
```

Install from this repository:

1. download the built paclet `build/*.paclet`;

2. install the paclet:

    ``` wl
    PacletInstall@File["the/path/of/paclet"]
    ```

Install manually:

1. download this repository, and move it to the paclet directory `$UserBasePacletsDirectory`;

2. rebuild the internal paclet data:

    ``` wl
    PacletDataRebuild[]
    ```

## Load

``` wl
Needs["Yurie`BlueArXiv`"]
Needs["Yurie`PaperTool`"]
```

## Upgrade

``` wl
PacletInstall["Yurie/BlueArXiv"]
```

## Uninstall

``` wl
PacletUninstall["Yurie/BlueArXiv"]
```

## Documentation

[Yurie/BlueArXiv](https://resources.wolframcloud.com/PacletRepository/resources/Yurie/BlueArXiv/)
