# Repository Guidelines

## Architecture

* `Kernel/` contains the main runtime package.
* `Kernel/BlueArXiv/` contains the public feature modules: ID extraction, arXiv search, PDF download, BibTeX generation, and PDF name formatting.
* `Utility/` contains secondary contexts such as sample data and paclet info helpers.
* `Source/` stores fixtures used by examples and tests: PDFs, images, and TeX files.
* `Documentation/English/` stores Wolfram documentation notebooks.
* `AutoCompletionData/` stores front-end completion metadata.
* `Sandbox/` stores AI-created temporary files, exploratory tests, and verification artifacts.
* `Workbench/` and `TestSource/` are developer workspace material and are ignored by git.
* `build/` stores generated paclet artifacts; avoid editing generated contents by hand.

## Context Model

* Load the public API with `` Needs["Yurie`BlueArXiv`"] ``.
* Load sample helpers only when needed with `` Get["Yurie`BlueArXiv`Sample`"] ``.
* The package talks to arXiv and INSPIRE APIs; keep network-dependent checks narrow and explicit.
* Prefer fresh Wolfram kernels for verification so loaded contexts and cached service state do not hide regressions.

## Test Policy

* `Test/` is human-maintained formal regression territory.
* Do not add, edit, or regenerate `Test/*.wlt` unless Owner explicitly requests formal regression updates.
* AI-created temporary files, exploratory tests, and verification scripts belong in `Sandbox/`.
* Existing tests use `VerificationTest` and `TestID` values tied to notebook names, for example `3-searchByID.nb`.

## Maintenance

* Preserve package load order in `Kernel/BlueArXiv.wl`.
* Keep one public feature module per file under `Kernel/BlueArXiv/`.
* Keep fixtures stable; update `Source/` only when examples or tests require new input data.
