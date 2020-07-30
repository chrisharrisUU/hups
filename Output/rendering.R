### Render Rmarkdown files in this project
library(rmarkdown)

# # Counterbalancing old (deprecated)
# render(input = "Other/counterbalancing.Rmd",
#        output_dir = "Output/",
#        envir = new.env())
# 
# # Counterbalancing overview
# render(input = "Other/all_counterbalancing.Rmd",
#        output_dir = "Output/",
#        envir = new.env())

# Main summary
rmarkdown::render(input = "Analyses/HUPS1_summary.Rmd",
                  output_dir = "Output/",
                  envir = new.env())
rmarkdown::render(input = "Analyses/HUPS2_summary.Rmd",
                  output_dir = "Output/",
                  envir = new.env())
rmarkdown::render(input = "Analyses/HUPS3_summary.Rmd",
                  output_dir = "Output/",
                  envir = new.env())

# Run R code in Terminal to avoid slowing down IDE:
# https://jozefhajnala.gitlab.io/r/r905-rstudio-terminal/
# Shift + Alt + R to open Terminal
# R.exe to launch R
# Ctrl + 1 to go back to editor
# Ctrl + Alt + Enter to send commands to be executed directly to the terminal
# q() to quit