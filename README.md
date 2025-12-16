# BST260 2025 final project for Team 4
*Title: Exploring Shiny as an interactive visualization tool for public health data*
Authors: Tanvi Gaitonde, Gabrielle Gonzalez, Allen Gu, Camila Guetter

# Access the published Shiny app
Our deployed Shiny app is available on <https://allenzpgu.shinyapps.io/shiny/>.

# Deploying a local instance of the Shiny app
You may deploy the Shiny app locally by following the following steps:

* Ensure shiny is installed: `install.packages("shiny")`
* In R console, load shiny with `library(shiny)`
* From the Project root directory, run in R console `runApp('code/shiny')`
* A local instance of our Shiny app should now be deployed

# Directory map
├── code/                           
│   ├── rough_work/           
│   │   └── ...            # rough work from each member of the team
│   └── shiny/
│       └── app.R          # the main Shiny app  
├── contributions/        
│   └── ...                # individual team contribution declarations
├── data/                  # irrelevant for our project, not used
├── docs/
│   ├── nature.csl         # Nature journal Citation Style Language file
│   ├── outline.txt        # our original project proposal outline
│   └── references.bib     # BibTex file containing our references
├── README.md
├── final-project.pdf      # Final Report compiled PDF
├── final-project.qmd      # Final Report rough Quarto file
└── supplementary_materials.pdf    # Supplementary figures file
