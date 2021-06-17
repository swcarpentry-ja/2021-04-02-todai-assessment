#' Count responses to multiple-answer questions
#'
#' @param data Survey data in wide format
#' @param ... Selection of columns, each of which is an answer to a 
#' multiple choice question. Must also include `num` (ID of survey participant)
#'
#' @return Tibble with two columns: `value` (answer, factor ordered by `n`) and 
#' `n` (number of times that answer was observed)
pivot_survey <- function(data, ...) {
	select(data, ...) %>%
		pivot_longer(-num) %>%
		filter(!is.na(value)) %>% 
		count(value) %>%
		mutate(value = str_wrap_ja(value, 10)) %>%
		mutate(value = fct_reorder(value, n))
}

str_wrap_ja_single <- function(text, width) {
	text_sep <- as.character(quanteda::tokens(text, what = "word"))
	groups <- ggplot2::cut_width(1:length(text_sep), width)
	split(text_sep, groups) %>% 
		purrr::map(~paste(., collapse = "")) %>% 
		unlist() %>% 
		paste(collapse = "\n")
}

str_wrap_ja <- function(text, width) {
	map_chr(text, ~str_wrap_ja_single(.x, width))
}
