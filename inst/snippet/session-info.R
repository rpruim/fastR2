PP <- devtools::session_info()
PP$packages[, "source"] <- substr(PP$packages[, "source"], 1, 15)
PP

