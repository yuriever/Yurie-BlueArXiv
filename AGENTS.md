# Repository Guide

## Architecture

This paclet has three working zones: product code, human-maintained material, and AI workspace.

* Product code lives in `Kernel/`. Public feature modules are in `Kernel/BlueArXiv/`, one file per feature: ID extraction, arXiv search, PDF download, BibTeX generation, and PDF name formatting.
* `Utility/` contains auxiliary contexts for paclet path metadata and sample-data helpers.
* Paclet metadata and front-end resources live in `PacletInfo.wl`, `AutoCompletionData/`, `Documentation/English/`, `README.md`, and `ResourceDefinition.nb`.
* Stable fixtures for examples and tests live in `Source/`.
* Shared maintenance scripts belong in `Script/`.
* Generated paclet artifacts belong in `build/`; avoid editing generated files by hand.

## Ownership Boundary

* Human-only: `Workbench/`, `TestSource/`, and `Test/`.
* AI-only: `Sandbox/`.
* Shared: all project files outside the human-only and AI-only areas.
* Do not add, edit, or regenerate human-only files unless Owner explicitly requests it.
* Put AI scratch files, exploratory tests, temporary data, generated reports, and verification artifacts under `Sandbox/`.

## Runtime Context

* Load the public API with `` Needs["Yurie`BlueArXiv`"] ``.
* Load sample helpers only when needed with `` Get["Yurie`BlueArXiv`Sample`"] ``.
* Preserve the package load order in `Kernel/BlueArXiv.wl`.
* Keep public feature implementation inside the matching `Kernel/BlueArXiv/*.wl` module.

## Testing and Verification

* Formal regression tests in `Test/` are maintained by humans.
* AI tests belong in `Sandbox/Test/`, preferably using fresh Wolfram kernels.
* arXiv and INSPIRE checks are network-dependent; keep such verification narrow and explicit.

## Maintenance Style

* Match existing Wolfram formatting; public symbols use lower camel case.
* Keep fixtures stable; update `Source/` only when shared examples or human-maintained tests need new input data.

## Convention

* Naming
    * Prefer singular forms for section headings and directory names.

* Markdown
    * Use `*` for unordered list markers.
    * Use four spaces for nested indentation.
    * In prose, do not hard-wrap sentences to fit a fixed width.
    * For placeholders in commands, use angle/square brackets for required/optional arguments, such as `command <file> [--flag]`.

* Wolfram
    * Prefer file-based execution with `WolframKernel -script <file.wl>`.
