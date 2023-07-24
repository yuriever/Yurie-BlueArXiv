(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Yurie/BlueArXiv",
    "Description" -> "A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs",
    "Creator" -> "Yurie",
    "License" -> "MIT",
    "PublisherID" -> "Yurie",
    "Version" -> "1.0.1",
    "WolframVersion" -> "13+",
    "PrimaryContext" -> "Yurie`arxiv`",
    "SourceControlURL" -> "https://github.com/yuriever/Yurie-BlueArXiv",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"Yurie`arxiv`", "Yurie`paper`"},
        "Symbols" -> {
          "Yurie`arxiv`arXivIDQ",
          "Yurie`arxiv`arXivInterface",
          "Yurie`arxiv`arXivPDFNameFormat",
          "Yurie`arxiv`extractID",
          "Yurie`arxiv`searchByID",
          "Yurie`arxiv`downloadByID",
          "Yurie`arxiv`generateBibTeXByID",
          "Yurie`paper`extractCiteKey",
          "Yurie`paper`extractTitleFromPDF"
        }
      },
      {"Documentation", "Language" -> "English"},
      {
        "Asset",
        "Root" -> ".",
        "Assets" -> {
          {"License", "LICENSE"},
          {"ReadMe", "README.md"},
          {"SampleData", "SampleData"}
        }
      }
    }
  |>
]
