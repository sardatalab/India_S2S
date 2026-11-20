### Paths: one block per user, found by your Windows username (like the Stata masters).
### The repo folder (path) is set automatically in main.R: open India_S2S.Rproj before running.
###
### datapaths: the data folder of each exercise, i.e. the folder that holds
### Data/Stage 1 and Data/Stage 2. Names must match the files in config/.

user <- tolower(Sys.info()[["user"]])

#Jaime
if (user == "wb553773") {
  datapaths <- c(
    fy2022_backcast = "C:/Users/wb553773/OneDrive - WBG/Stats Team/IND S2S imputation/Reproducibility package"
  )
#Diego
} else if (user == "wb587758") {
  datapaths <- c(
    fy2022_backcast = "C:/Users/wb587758/OneDrive - WBG/IND/S2S/2022-23"
  )
#Anyone else: add a block like the ones above
} else {
  stop("No paths for user '", user, "': add your block in config/paths.R")
}
