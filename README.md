# [Yurie/BlueArXiv](https://github.com/yuriever/Yurie-BlueArXiv)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Wolfram Language](https://img.shields.io/badge/Wolfram%20Language-14.3%2B-red.svg)](https://www.wolfram.com/language/)

A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs.

This paclet uses the following APIs:

* [arXiv API](https://info.arxiv.org/help/api/index.html)

* [INSPIRE REST API](https://github.com/inspirehep/rest-api-doc)


## [Documentation](https://resources.wolframcloud.com/PacletRepository/resources/Yurie/BlueArXiv/)


## Usage

Install from the Wolfram Language Paclet Repository:

``` wl
PacletSiteUpdate@PacletSites[];

PacletInstall["Yurie/BlueArXiv"]
```

Install from this repository:

1. Clone or download this repository

2. Move the entire folder to the user paclet directory:

   ```wl
   $UserBasePacletsDirectory
   ```

3. Rebuild the paclet data:

   ```wl
   PacletDataRebuild[]
   ```

4. Load the paclet

    ```wl
    Needs["Yurie`BlueArXiv`"]
    ```


### Uninstallation

```wl
PacletUninstall["Yurie/BlueArXiv"]
```


### Installation checking

```wl
PacletFind["Yurie/BlueArXiv"]
```