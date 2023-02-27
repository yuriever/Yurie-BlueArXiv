# Package ``lily`arxiv` ``
A Mathematica package for searching, downloading and renaming arXiv preprints by ID or title.

The documentation can also be found on [my website](https://yuriever.github.io/symbolic/package-lily-arxiv/).

This package uses the following APIs:

* [INSPIRE REST API](https://github.com/inspirehep/rest-api-doc)

* [arXiv API](https://info.arxiv.org/help/api/index.html)

## List of symbols

### Recognize arXiv ID

* `arXivIDQ[_]` - check whether a string is a valid arXiv ID. 
    The new ID template after 2007 is `xxxx.xxxx(x)`, and the old one is `class/xxxxxxx`.

### Set the format of file names

* `fileNameFormat[format_]` - set the format of file names. The default format is 

    ``` wl
    fileNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"];
    ```

    The accepted keywords in `format_` include the following. If the preprint is not published yet, `"journal"` will return an empty string.

    ``` wl
    "ID", "date", "title", "abs", "author", "firstAuthor", "firstThreeAuthor", "journal"
    ```

    This function will translate the input format into a lambda function stored in the internal symbol `fileNameFormatter` which is further called by others.

* `fileNameInPath[_String]` - return a list of PDF file names in the path.

* `fileNameRegulate[_String]` - regulate the file name with special characters like `"/"` and `"\n"`. The transformation rules are stored in ``fileNameRegulate`ruleList``.

### Extract, search, download and generate BibTeX by arXiv ID

* `extractID[_][_]` - extract all arXiv IDs from string, file name or path.
    
    * `extractID["string"][_String|_List]` - return found IDs from the input string or List of strings. The tag `"string"` can be omitted. The typical output is like

        ``` wl
        Out[]= {"xxxx.xxxx",...}
        ```

    * `extractID["file"|"path",opts_][_String|_List]` - return a dataset of associations with IDs, file names and where the IDs are found. The typical output is like
            
        ``` wl
        Out[]= {<|"ID"->"xxxx.xxxx","file"->{"file1",...},"IDLocation"->{"foundInFirstPage",...}|>}
        ```

    Default options:

    * `"tryFileName"->True` - IDs in the file name are extracted firstly, and if not found then the first page is searched. Otherwise only IDs in the first page are extracted.
    
    * `"hidePath"->True` - if the tag is `"path"`, the root path of file names will be hidden.

    * `"mergeDuplicateID"->True` - the result will be gathered and sorted by `"ID"`. Otherwise there can be duplicate IDs from different positions.

* `searchByID[_][_]` - search by IDs extracted from string, file or path, and return the found items on arXiv with formatted names by `fileNameFormatter`. 

    * `searchByID["string",opts_][_String|_List]` - the tag `"string"` can be omitted. The typical output is like

        ``` wl
        Out[]= {<|"ID"->{"xxxx.xxxx"},"item"->"formatted name","URL"->"URL.pdf"|>}
        ```

    * `searchByID["file"|"path",opts_][_String|_List]` - the typical output is like

        ``` wl
        Out[]= {<|"ID"->{"xxxx.xxxx"},"item"->"formatted name","URL"->"URL.pdf","file"->{"file1",...},"IDLocation"->{"foundInFirstPage",...}|>}
        ```

    Default options:

    * `"tryFileName"->True` - IDs in the file name are extracted firstly, and if not found then the first page is searched. Otherwise only IDs in the first page are extracted.

    * `"hidePath"->True` - if the tag is `"path"`, the root path of file names will be hidden.

    * `"mergeDuplicateID"->True` - the result will be gathered and sorted by `"ID"`. Otherwise there can be duplicate IDs from different positions.

    * `"fileNameRegulate"->True` - special characters will be replaced in the formatted name by `fileNameRegulate`.

* `downloadByID[__][_]` - download by IDs extracted from string, file or path to the target path, and return the file objects with formatted names by `fileNameFormatter`.

    * `downloadByID[targetPath_,"string",opts_][_String|_List]` - the tag `"string"` can be omitted. The typical output is like

        ``` wl
        Out[]= {<|"ID"->{"xxxx.xxxx"},"item"->"formatted name","URL"->"URL.pdf","fileObject"->"file.pdf"|>}
        ```

    * `downloadByID[targetPath_,"file"|"path",opts_][_String|_List]` - the typical output is like

        ``` wl
        Out[]= {<|"ID"->{"xxxx.xxxx"},"item"->"formatted name","URL"->"URL.pdf","file"->{"file1",...},"IDLocation"->{"foundInFirstPage",...},"fileObject"->"file.pdf"|>}
        ```

    Default options are the same as `searchByID`.

* `generateBibTeXByID[__][_]` - export the found BibTeX entries on inspirehep by IDs extracted from string, file or path, and return the BibTeX keys.

    * `generateBibTeXByID[targetPath_,bibName_,"string",opts_][_String|_List]` - the tag `"string"` can be omitted. The typical output is like

        ``` wl
        Out[]= {<|"key"->"author:YYYYxxx","ID"->{"xxxx.xxxx"},"BibTeX"->"found BibTeX"|>}
        ```

    * `generateBibTeXByID[targetPath_,bibName_,"file"|"path",opts_][_String|_List]` - the typical output is like

        ``` wl
        Out[]= {<|"key"->"author:YYYYxxx","ID"->{"xxxx.xxxx"},"BibTeX"->"found BibTeX","file"->{"file1",...},"IDLocation"->{"foundInFirstPage",...}|>}
        ```

    Default options are the same as `extractID`.

### Extract, search and download by title

* `extractTitle["file"|"path",opts_][_String|_List]` - extract title from the first page of PDF by searching grouped texts with larger Y coordinates and fontsizes. This is similar to the Python package [pdftitle](https://github.com/metebalci/pdftitle). The typical output is like 

    ``` wl
    Out[]= {<|"title"->"extracted title","file"->{"file1",...}|>}
    ```

    Default options:

    * `"hidePath"->True` - if the tag is `"path"`, the root path of file names will be hidden.
    
    * `"mergeDuplicateTitle"->True` - the result will be gathered and sorted by `"title"`. Otherwise there can be duplicate titles from different positions.

    * `"titleExtractMethod"->"plusYAndFontSize"` - select grouped texts by summing normalized Y coordinates and fontsizes. Another choice is `"sortYAndFontSize"` by sorting larger Y coordinates and fontsizes.

    * `"YResolution"->25` - the text elements extracted from the first page will be grouped according to the Y coordinate with resolution `"YResolution"`.

    __TODO: test and improve the accuracy (by SVM or some NN?)__

* `searchByTitle["file"|"path",opts_][_String|_List]` - search by titles extracted from file or path, and return the best-matched items on arXiv with formatted names by fileNameFormatter. The best match item is picked by minimizing `EditDistance`. The typical output is like 

    ``` wl
    Out[]= {<|"title"->"extracted title","item"->"formatted name","URL"->"URL.pdf","distance"->editDistance,"file"->{"file1",...},|>}
    ```

    Default options:

    * `"hidePath"->True` - if the tag is `"path"`, the root path of file names will be hidden.
    
    * `"mergeDuplicateTitle"->True` - the result will be gathered and sorted by `"title"`. Otherwise there can be duplicate titles from different positions.

    * `"titleExtractMethod"->"plusYAndFontSize"` - select grouped texts by summing normalized Y coordinates and fontsizes. Another choice is `"sortYAndFontSize"` by sorting larger Y coordinates and fontsizes.

    * `"YResolution"->25` - the text elements extracted from the first page will be grouped according to the Y coordinate with resolution `"YResolution"`.

    * `"fileNameRegulate"->True` - special characters will be replaced in the formatted name by `fileNameRegulate`.

    * `"maxItems"->10` - the number of items when searching a title.

* `downloadByTitle[targetPath_,"file"|"path",opts_][_String|_List]` - download by titles extracted from file or path to the target path, and return the file objects with formatted names by fileNameFormatter. The typical output is like 

    ``` wl
    Out[]= {<|"title"->"extracted title","item"->"formatted name","URL"->"URL.pdf","distance"->editDistance,"file"->{"file1",...},"fileObject"->"file.pdf"|>}
    ```

    Default options are the same as `searchByID`.


## To-do list and other issues

* The workflow can be found in [arxiv.pdf](https://github.com/yuriever/lily-arxiv/blob/main/arxiv/arxiv.pdf).

* Output: the returned value is either list of strings or dataset of associations storing relevant data, and downloading file is treated as side effect.

* TODO: rename files by ID or title.

