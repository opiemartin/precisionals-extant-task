# Welcome to PRECISION ALS

This file is intended for use by project collaborators. If you're interested in general information about the project, please see <https://www.precisionals.ie>.

# Project structure

- **/data**: Input data files should be here. Not tracked.
- **/output**: Generated files are stored here. Each  Not tracked.
- **/doc**: Documentation related to the project.
- **/extra**: Files not covered elsewhere.
- **/src**: Source code files.
    - **./ext**: Common code for all subtasks containing parsed and cleaned data as well as utility functions.
    - **./qc**: Quality control scripts.
    - **./qX**: Subfolders specific for each extant subtask.

# Setting up your work environment

The easiest way to get started is to create a fork of the project through the GitHub website.
If you will be commiting your changes to another fork, you can skip this step.

In any case, once you have a fork ready, you should clone the repo to your local computer:

```sh
git clone https://github.com/<owner>/precisionals-extant-task
```

Now you should have a new folder called previsionals-extant-task in your current directory.
You should now create a new subfolder called /data. Source files will by default reference data in this folder, so you should save your own copy of the data in that folder respecting the original file names.

```sh
cd precisionals-extant-task
mkdir data
```

To allow for versioning of the dataset, or if you want to keep your data files elsewhere, you can do so by setting the PALS_EXTANT_DATADIR environment variable to the path containing your files. Please check the documentation for your current OS on how to set environment variables persistently if you want to do so.

```sh
export PALS_EXTANT_DATADIR="<path>"
```

The project makes heavy use of R packages from the so called tidyverse. Assuming you have your own copy of the R environment already installed, you can easily install all the dependencies with the following:

```sh
Rscript -e "install.packages(c('tidyverse', 'writexl'), repos = 'http://cran.us.r-project.org')"
```

You should also install the specific packages required by the

# Suggested workflow

Once you have finished setting up your environment, you can already start making changes.
It is recommended that you create one branch for every cohesive set of changes. You can do so with:

```sh
git checkout -b new-branch
```

Now you are ready to do all the changes. After you're done, you commit your changes to your local branch and push your changes to your remote repository (aka your fork). Once you've completed your task, you should create a pull request (PR). You can find instructions on how to do so [here](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request).

*Note*: Please remember to specify "Create a new branch" in the base branch dropdown, and use the following format \<qX-feature-xxx\> when creating your PR.

You're done!