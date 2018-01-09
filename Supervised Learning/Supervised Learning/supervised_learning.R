realtime_dat = read.csv("Dataset/Feature-User-Word-Translation-Realtime.csv",stringsAsFactors=FALSE);
end_dat = read.csv("Dataset/Feature-User-Word-Translation-At-the-end-ofproject.csv", stringsAsFactors = FALSE);
manual_check = read.csv("Dataset/Manual quality check TG2.csv", stringsAsFactors = FALSE);

manual_check = manual_check[, 1:19];

