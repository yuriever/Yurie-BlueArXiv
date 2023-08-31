# Yurie/BlueArXiv

A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs.

This paclet uses the following APIs:

* [arXiv API](https://info.arxiv.org/help/api/index.html)

* [INSPIRE REST API](https://github.com/inspirehep/rest-api-doc)

## How to use

### Install

Install via the Wolfram Language Paclet Repository (may need `PacletSiteUpdate@PacletSites[]`):

```
PacletInstall["Yurie/BlueArXiv"];
```

Install from this repository:

1. download the built paclet `build/*.paclet`;

2. install the paclet:

    ``` 
    PacletInstall@File["the/path/of/paclet"];
    ```

Load the package(s):

```
Needs["Yurie`BlueArXiv`"];
Needs["Yurie`PaperTool`"];
```

### Upgrade

```
PacletInstall["Yurie/BlueArXiv"];
```

### Uninstall

```
PacletUninstall["Yurie/BlueArXiv"];
```

### Documentation

[Yurie/BlueArXiv](https://resources.wolframcloud.com/PacletRepository/resources/Yurie/BlueArXiv/)

