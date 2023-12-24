(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Yurie/BlueArXiv",
    "Description" -> "A Mathematica paclet for downloading preprints and generating BibTeX by arXiv IDs",
    "Creator" -> "Yurie",
    "SourceControlURL" -> "https://github.com/yuriever/Yurie-BlueArXiv",
    "License" -> "MIT",
    "PublisherID" -> "Yurie",
    "Version" -> "1.1.13",
    "WolframVersion" -> "13+",
    "PrimaryContext" -> "Yurie`BlueArXiv`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"Yurie`BlueArXiv`", "Yurie`PaperTool`"}
      },
      {
        "Kernel",
        "Root" -> "Utility",
        "Context" -> {"Yurie`BlueArXiv`"}
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
          {"Sample", "Sample"}
        }
      }
    }
  |>
]
