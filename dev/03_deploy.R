# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Pre-deployment checklist ----
## 1. Update version number in DESCRIPTION
## 2. Update NEWS.md with changes
## 3. Run all checks below
## 4. Commit all changes

## Run checks ----
## Check the package before sending to prod
devtools::check()

## Check on multiple platforms (if submitting to CRAN)
# rhub::rhub_check()  # Newer rhub interface
# rhub::check_for_cran()  # Legacy interface

## Run tests with coverage
covr::package_coverage()

## Check spelling
spelling::spell_check_package()

## Test the app locally
golem::run_dev()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## Choose the appropriate Dockerfile for your deployment target

# Option 1: Generic Docker deployment with renv (RECOMMENDED)
# golem::add_dockerfile_with_renv()

# Option 2: Docker without renv (lighter but less reproducible)
# golem::add_dockerfile()

# Option 3: ShinyProxy deployment
# golem::add_dockerfile_with_renv_shinyproxy()

# Option 4: Heroku deployment
# golem::add_dockerfile_heroku()

## After creating Dockerfile, build and test:
# docker build -t nflendzoneapp .
# docker run -p 3838:3838 nflendzoneapp

## Posit Connect / ShinyApps.io / Shiny Server ----
## If you want to deploy on Posit related platforms

# Option 1: Posit Connect (formerly RStudio Connect)
# golem::add_positconnect_file()

# Option 2: ShinyApps.io (cloud hosting)
# golem::add_shinyappsio_file()

# Option 3: Shiny Server (open source, self-hosted)
# golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----

## Step 1: Create app.R in root (if not exists)
# golem::add_rstudioconnect_file()  # Creates app.R that loads your package

## Step 2: Add/update manifest file (optional; for Git-backed deployment)
# rsconnect::writeManifest()

## Step 3: Deploy via rsconnect
# First time: Set up your account
# rsconnect::setAccountInfo(name = "your-account", token = "TOKEN", secret = "SECRET")

## Deploy command
# rsconnect::deployApp(
#   appName = desc::desc_get_field("Package"),
#   appTitle = desc::desc_get_field("Title"),
#   appFiles = c(
#     # Core package files
#     "R/",
#     "inst/",
#     "data/",
#     "NAMESPACE",
#     "DESCRIPTION",
#     "app.R"
#     # Add any additional files unique to your app here
#     # "www/",
#     # "config.yml"
#   ),
#   appId = rsconnect::deployments(".")$appID,
#   lint = FALSE,
#   forceUpdate = TRUE
# )

## Alternative: Deploy using GitHub Actions
## See: usethis::use_github_action("shiny-deploy")

## AWS / Cloud Deployment ----
## For AWS, Azure, GCP deployment, see:
# https://engineering-shiny.org/deploy.html

## Performance monitoring ----
## Add {shinyloadtest} for load testing
# shinyloadtest::record_session("http://127.0.0.1:3838")
# shinyloadtest::run_loadtest()

## Add {profvis} for profiling
# profvis::profvis({ run_app() })
