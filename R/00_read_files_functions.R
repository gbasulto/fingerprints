
extract_info <- function(dir, subdir, filename){
    ## Extract gender and class and store them in single row dataframe
    ## with a single row.
    info <-
        paste0(dir, subdir, filename) %>%
        read.table(sep = ":",
                   stringsAsFactors = FALSE) %>%
        .$V2 %>%
        gsub(" ", "", .) %>%
        {data_frame(
             filename = gsub(".txt", "", filename),
             gender = .[1], class = .[2],
             impression_no = str_extract(.[3], "[fs]"),
             id = str_extract(.[3], "[0-9]{4}"),
             finger = str_extract(.[3], "_[0-9]{2}"))} %>%
        mutate(impression_no = 1 + (impression_no == "s"),
               finger = as.numeric(gsub("_", "", finger)))

    return(info)
}

extract_info_df <- function(db_dir){
    ## Use extract_info function above to extract the information for
    ## every individual and put it into a single dataframe.

    ## Subdirectories
    subdirs <- paste0("figs_", 0:7, "/")
    names(subdirs) <- subdirs

    ## List of filenames, one chararacter vector per subdirectory.
    filenames <-
        lapply(subdirs, function(subdir)
        list.files(paste0(db_dir, subdir), pattern = "*.txt"))

    ldply(subdirs, function(subdir)
        ldply(filenames[[subdir]], function(filename)
            extract_info(db_dir, subdir, filename))) %>%
        tbl_df() %>%
        rename(subdir = .id)
}

read_fingerprint_matrix <- function(dir, subdir, filename){
    srce <- paste0(dir, subdir, filename)
    img <- readPNG(source = srce, TRUE)
    return(as.matrix(img))
} 

sample_image <- function(dir, info_df, Gender = NULL,
                         Class = NULL, Impression = NULL,
                         ID = NULL, Finger = NULL){
    nr <- nrow(info)
    if(!is.null(Gender) & nr > 0) {
        info_df %<>% filter(gender == Gender)
        nr <- nrow(info)
    }
    if(!is.null(Class) & nr > 0) {
        info_df %<>% filter(class %in% Class)
        nr <- nrow(info)
    }        
    if(!is.null(Impression) & nr > 0) {
        info_df %<>% filter(impression == Impression)
        nr <- nrow(info)
    }        
    if(!is.null(ID) & nr > 0){
        info_df %<>% filter(id == ID)
        nr <- nrow(info)
    }
    if(!is.null(Finger) & nr > 0){
        info_df %<>% filter(finger == Finger)
        nr <- nrow(info)
    }

    if(nr  == 0) {
        msg <- "No fingerprint meets these criteria"
        warning(msg)
    }
 
    info_df %>%
        sample_n(1) %>%
        with(read_fingerprint_matrix(db_dirr, subdir,
                                    paste0(filename, ".png")))
}
