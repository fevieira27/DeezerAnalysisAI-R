
# Cleans all variables
rm(list = ls())

# Limiting the number of rows from "print" commands in R console (doesn't affect exported files)
options(max.print=250)

# Setting default repo
local({r <- getOption("repos")
	 r["CRAN"] <- "https://cran.r-project.org"
	 options(repos=r)
})

# Install/Load the necessary libraries
if (!require("httr")) { install.packages("httr") }
if (!require("dplyr")) { install.packages("dplyr") }
if (!require("RecordLinkage")) { install.packages("RecordLinkage") }
if (!require("tibble")) { install.packages("tibble") }
# if (!require("writexl")) { install.packages("writexl") }
if (!require("openxlsx")) { install.packages("openxlsx") }

# Replace with your actual playlist ID (it needs to be public!)
playlist_id <- 'XXXXXX'
export_path <- 'C:/Users/XXXXXX/'

# Set your Deezer API credentials from Deezer Developers API (create text file with your own key and change path)
deezer_app_id <- "XXXXXX"
deezer_app_secret <- readLines("C:/Users/XXXXXX/Deezer_API_key.txt", warn=FALSE)

# Authenticate with Deezer
deezer_auth <- POST(
  url = "https://connect.deezer.com/oauth/access_token.php",
  body = list(
    grant_type = "client_credentials",
    client_id = deezer_app_id,
    client_secret = deezer_app_secret
  ),
  encode = "form",
  verbose()
)
access_token <- deezer_auth

# URL that gets track data from playlist
url <- paste0("https://api.deezer.com/playlist/", playlist_id, "/tracks")

# Function to recursively get all tracks from the playlist
get_songs <- function(url) {
  response <- GET(
  url,
  add_headers(Authorization = paste0("Bearer ", access_token)),
  verbose()
  )
   
  content <- content(response, as = "parsed")

  # Check if theres a next page and recursively get more songs
  if (!is.null(content$`next`)) {
    content$data <- c(content$data, get_songs(content$`next`))
  }

  return(content$data)
}

# Call function to get data from tracks of a playlist
songs <- get_songs(url)

# Convert to dataframe
df_songs <- bind_rows(lapply(songs, as.data.frame.list))

# Selecting only columns that matter
df_songs_filtered <- df_songs %>% select(id, title, title_short, isrc, duration, rank, type, artist.id, artist.name, artist.type, album.id, album.title, album.type)

# Review the results
# print(df_songs_filtered)

# Cleaning artist name to lowercase and changing header name
dupSongs_df <- data.frame(tolower(df_songs_filtered$artist.name), stringsAsFactors = FALSE)
colnames(dupSongs_df) <- "artist"

# Cleaning song title to lowercase
dupSongs_df$title <- tolower(df_songs_filtered$title)

# Adding duration to DF
dupSongs_df$duration <- df_songs_filtered$duration

# Review the results
# print(dupSongs_df)

############## Using Levenshtein Similarity

# Create a comparison object for deduplication
rpairsLeven <- compare.dedup(dupSongs_df, strcmp = TRUE, strcmpfun = levenshteinSim)

# Calculate weights for the comparison
rpairsLeven <- epiWeights(rpairsLeven)

# Get pairs with a high probability of being duplicates
duplicatesLeven <- getPairs(rpairsLeven, min.weight=0.79, max.weight=0.99)

# Review the results
# summary(epiClassify(rpairsLeven,0.6))
# print(duplicatesLeven)


############## Using Jaro & Winkler Algorithm, only for songs with the same duration

# Create a comparison object for deduplication
rpairsJaroW <- compare.dedup(dupSongs_df, strcmp = TRUE, strcmpfun = jarowinkler, blockfld=list("artist","duration"))

# Calculate weights for the comparison
rpairsJaroW <- epiWeights(rpairsJaroW)

# Get pairs with a high probability of being duplicates
duplicatesJaroW <- getPairs(rpairsJaroW, min.weight=0.7, max.weight=0.929)

# Review the results
# summary(epiClassify(rpairsJaroW,0.6))
# print(duplicatesJaroW)

# Creating a subframe with all the compared data for Jaro & Winkler Algorithm
subJW <- rpairsJaroW$pairs
subJW$AvgWeight <- epiWeights(rpairsJaroW)$Wdata
subJW$source <- "JaroWinkler"

# Creating a subframe with all the compared data for Levenshtein Similarity
subLV <- rpairsLeven$pairs
subLV$AvgWeight <- epiWeights(rpairsLeven)$Wdata
subLV$source <- "Levenshtein"

# Appending all those into a dataframe, filtering by similarity of artist, title and durantion
duplicatesTotal <- rbind(subJW, subLV)

# Creating ID column based on row number to original DF
dupSongs_df <- rownames_to_column(dupSongs_df, var = "ID")

# Join total duplicates found with the original dataset, to get track info and allow validation of duplicates
finalDupSongs <- merge(duplicatesTotal, dupSongs_df, by.x = "id1", by.y = "ID")
# colnames(finalDupSongs) <- c("id1","id2","artist_sim","title_sim","duration_sim","is_match","AvgWeight","source","artist_1","title_1","duration_1")
finalDupSongs <- merge(finalDupSongs, dupSongs_df, by.x = "id2", by.y = "ID")
colnames(finalDupSongs) <- c("id2","id1","artist_sim","title_sim","duration_sim","is_match","AvgWeight","source","artist_1","title_1","duration_1","artist_2","title_2","duration_2")

# Remove duplicates resulting from both comparison algorithms (when all 3 fields compared are equal - exact match)
finalDupSongs <- finalDupSongs[order(finalDupSongs$id1, finalDupSongs$id2, finalDupSongs$AvgWeight, decreasing = c(FALSE, FALSE, TRUE)),]
finalDupSongs <- finalDupSongs[duplicated(finalDupSongs[c("id1", "id2")], fromLast = TRUE),]

duplicatesFiltered <- subset(finalDupSongs, artist_sim >= 0.9 & title_sim >= 0.833 & duration_sim >= 0.6 & AvgWeight >= 0.82)
duplicatesFiltered <- duplicatesFiltered %>% select(artist_sim,title_sim,duration_sim,AvgWeight,source,id1,artist_1,title_1,duration_1,id2,artist_2,title_2,duration_2)

wb <- createWorkbook()
addWorksheet(wb, "Duplicates")
writeData(wb, "Duplicates", duplicatesFiltered)
setColWidths(wb, "Duplicates", cols = 1:5, widths = "auto")
setColWidths(wb, "Duplicates", cols = 6:13, widths = 5)
setColWidths(wb, "Duplicates", cols = c(7,8,11,12), widths = c(30, 40, 30, 40))

duration_style <- createStyle(fgFill="#EBF1DE")
artist_style <- createStyle(fgFill="#C5D9F1")
title_style <- createStyle(fgFill="#F2DCDB")

addStyle(wb, "Duplicates", duration_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 9)
addStyle(wb, "Duplicates", duration_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 13)
addStyle(wb, "Duplicates", artist_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 7)
addStyle(wb, "Duplicates", artist_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 11)
addStyle(wb, "Duplicates", title_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 8)
addStyle(wb, "Duplicates", title_style, rows = 2:(nrow(duplicatesFiltered)+1), cols = 12)

saveWorkbook(wb, paste(export_path,"deezer_playlist_analysis.xlsx"), overwrite = TRUE)


# Full extract, if needed
# test <- finalDupSongs[duplicated(finalDupSongs[c("id1", "id2")], fromLast = TRUE),]
# test <- test %>% select(artist_sim,title_sim,duration_sim,AvgWeight,source,id1,artist_1,title_1,duration_1,id2,artist_2,title_2,duration_2)
# if(file.exists(paste(export_path,"full_deezer_extract.xlsx"))){
#   file.remove(paste(export_path,"full_deezer_extract.xlsx"))
# } else {
#   write_xlsx(test,paste(export_path,"full_deezer_extract.xlsx"),use_zip64 = TRUE)
# }
