safe_crop <- purrr::possibly(magick::image_crop,otherwise = NULL)
#' Slice an image and reorder it
#'
#' @param img magick image object returned by image_read() or image_graph()
#' @param dpi number of slice
#'
#' @export
#' @import purrr
#' @import magick
#' @import glue
#' @examples
#' library(magick)
#' image_read("http://fr.web.img6.acsta.net/pictures/15/07/20/18/14/582462.jpg") %>%
#' slice_and_mix(dpi = 50)
slice_and_mix <- function(img,dpi=40){
  orig <- image_info(img)
  c(0,seq_len(dpi)) %>%
    map(
      ~img %>% safe_crop(geometry = glue::glue("{orig$width/dpi}x{orig$height}+{.x*orig$width/dpi}+0"))
    ) %>%
    compact()-> d1

  b1 <- d1[c(TRUE,FALSE)] %>%
    reduce(c) %>%
    image_append(stack = FALSE)
  b2 <- d1[c(FALSE,TRUE)] %>%
    reduce(c) %>%
    image_append(stack = FALSE)
  image_append(c(b1,b2),stack = FALSE)
}

#' Slice an image and reorder it twice
#'
#' @param img magick image object returned by image_read() or image_graph()
#' @param dpi number of slice
#'
#' @export
#'
#' @examples
#' library(magick)
#' image_read("http://fr.web.img6.acsta.net/pictures/15/07/20/18/14/582462.jpg") %>%
#' lens(dpi = 50)
lens <- function(img,dpi=30){
  img %>%
  slice_and_mix(dpi = dpi) %>%
  image_rotate(degrees = 90)%>%
  slice_and_mix(dpi = dpi) %>%
  image_rotate(degrees = -90)
}


