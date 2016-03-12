
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

sample_image <- function(dir, info_df,
                         Gender = unique(info_df$gender),
                         Class = unique(info_df$class),
                         Impression = unique(info_df$impression_no),
                         ID = unique(info_df$id),
                         Finger = unique(info_df$finger)){
    info_df <-
        info_df %>%
        filter(gender %in% Gender, class %in% Class,
               impression_no %in% Impression,
               id %in% ID, finger %in% Finger)

    nr <- nrow(info_df)

    if(nr  == 0) {
        msg <- "No fingerprint meets these criteria"
        warning(msg)
        return(outer(1:512, 1:512, function(x, y) dnorm(x)*dnorm(y)))
    }

    sampled <-
      info_df %>%
        sample_n(1)

    out <-
      sampled %>%
      with(read_fingerprint_matrix(db_dirr, subdir,
                                   paste0(filename, ".png"))) %>%
      as.matrix()

    attr(out, "id") <- sampled$id
    attr(out, "impression_no") <- sampled$impression_no
    return(out)
}

display_image <- function(image_matrix, ncolors = 256){

  title <- paste0("id: ", attr(image_matrix, "id"),
                 " , impression no.: ", attr(image_matrix, "impression_no"))


  out <- image(image_matrix[512:1, 512:1],
               col = gray(seq(0, 1, len = ncolors)),
               xlab = "", ylab = "",
               main = title)
  return(out)
}
