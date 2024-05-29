(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Yurie/BlueArXiv",
    "Description" -> "A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs",
    "Creator" -> "Yurie",
    "SourceControlURL" -> "https://github.com/yuriever/Yurie-BlueArXiv",
    "License" -> "MIT",
    "PublisherID" -> "Yurie",
    "Version" -> "2.1.4",
    "WolframVersion" -> "13+",
    "PrimaryContext" -> "Yurie`BlueArXiv`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"Yurie`BlueArXiv`", "Yurie`PaperTool`"},
        "Symbols" -> {
          "Yurie`BlueArXiv`arXivInterface",
          "Yurie`BlueArXiv`arXivPDFNameFormat",
          "Yurie`BlueArXiv`downloadByID",
          "Yurie`BlueArXiv`extractID",
          "Yurie`BlueArXiv`generateBibTeXByID",
          "Yurie`BlueArXiv`searchByID",
          "Yurie`PaperTool`extractCiteKey",
          "Yurie`PaperTool`extractTitleFromPDF"
        }
      },
      {
        "Kernel",
        "Root" -> "Utility",
        "Context" -> {
          "Yurie`BlueArXiv`Info`",
          "Yurie`BlueArXiv`Sample`"
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      },
      {"AutoCompletionData", "Root" -> "AutoCompletionData"},
      {
        "Asset",
        "Root" -> ".",
        "Assets" -> {
          {"License", "LICENSE"},
          {"ReadMe", "README.md"},
          {"Source", "Source"},
          {"Test", "Test"},
          {"TestSource", "TestSource"}
        }
      }
    }
  |>
]
