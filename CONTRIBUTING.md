##  Computing Contributing Guidelines: Environmental Drivers Across Ecological Scales


Our computing guidelines document is based on [this template](https://github.com/lter/scicomp/blob/main/CONTRIBUTING.md) and is meant to focus primarily on our internal processes for contributing. If you're not a member of this team, we hope you find this document valuable as-is but you should also feel free to make any modifications you feel are necessary if you choose to use our guidelines as a starting place.

## Version Control & GitHub

As much as possible, use the style of "[Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)" in your commit messages. See [here](https://njlyon0.github.io/tips/commits.html) for a nice summary of the highlights of that approach.

For **All Changes**, (1) communicate with the team to avoid merge conflicts and (2) make commits directly to main

## Style Guide

For folder/file names and in code, please follow the following style tips:

Use all lowercase
Related files/objects should have a shared file prefix (e.g., wrangle_...)
For files that have an inherent order, use zero-padded numbers as the prefix (e.g., 01_, 02_, etc.)
Use underscores (_) for separating major pieces of information and hyphens (-) in lieu of spaces (e.g., 01_find-area.R, harmonize_spp-rich-info.py, etc.)

## File Paths

When we write code we will use operating system (OS) agnostic file paths.

In R, this means using the file.path() function (from base R) to stitch together elements (e.g., file.path("data", "raw_2024.csv")). 

If you plan on re-using a file path, assign it to an object/variable (R) to avoid typos/errors when re-using the file path.
