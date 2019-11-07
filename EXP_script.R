#CHANGING PARAMETERS ----
wd = "C:/Users/lya02/Dropbox/nestler=hurd/rat_drug_sa/raw_files/" #RAW FILE FOLDER. MUST ONLY CONTAIN YOUR RAW FILES. NEED "/" AT LOCATION END.
funcpath ="C:/Users/lya02/Dropbox/nestler=hurd/rat_drug_sa/" #"app.R" FOLDER. NEED "/" AT LOCATION END.
deploy ="C:/Users/lya02/Dropbox/nestler=hurd/rat_drug_sa" #"app.R" FOLDER. FOR SHINY DEPLOYMENT. NO "/" AT LOCATION END.
experiment = "fr" #CHOOSE ONE OF THE FOLLOWING: "fr", "pr", "of", "split_xl_to_csv", "clear_workspace"
#-------------------------------------------------------------------------------------
#SELECTIVE PARAMETERS ----
binl=600 #CHANGE FOR FR & PR TO BIN SESSION IN SECONDS.
bins=seq(0,21600,binl) #CHANGE FOR FR & PR FOR TOTAL SESSION LENTGH IN SECONDS.
savePDF="no" #FOR OPEN FIELD. WRITE "yes" OR "no". "yes" TAKES MUCH LONGER TO PROCESS.
saveHeatMap="no" #FOR OPEN FIELD. WRITE "yes" OR "no". "yes" TAKES MUCH LONGER TO PROCESS.
#----------------------------------------
#DON'T CHANGE THIS SECTION ----
source(paste(funcpath, "app.R", sep=""),local=TRUE) #sCRIPT sOURCE. IGNORE "ERROR" WHEN RUN.
experiment_selection(wd) #REFERS TO EXPERIMENT ABOVE AND SELECTS WHICH SCRIPT FUNCTION TO RUN.
#----------------------------------------
#DEPLOYMENT OF SHINY APPLICATION TO SHINYAPPS.IO WEBSITE (REMOVE "#" TO RUN) ----
options(rsconnect.check.certificate = FALSE)
rsconnect::deployApp(appDir = deploy, account = 'hurd-laboratory', appTitle = "proteomics_behavior")
#----------------------------------------
#UPDATE R (REMOVE "#" TO RUN) ----
#install.packages("installr")
#library(installr)
#updateR()
#R.version.string




#shinyAppDir(system.file(deploy, package="shiny"), x = "output.csv")