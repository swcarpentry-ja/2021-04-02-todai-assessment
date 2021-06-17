# Visualize post-workshop answers

library(tidyverse)

path_to_csv_file_post <- "data/post-workshop-response-2021-04-22.csv"

partic_post <- read_csv(path_to_csv_file_post) %>%
	rename(num = `#`) %>%
	# Other questions have one column per answer, but the column names are very long
	# (they are the actual questions).
	# select/rename these so they are easier to read.
	select(
		num, 
		id = contains("固有の ID"),
		contains("環境で安心して学ぶことができた"),
		contains("学んだことをすぐに活かすことができる"),
		contains("自分の質問に対する講師の回答は明確だった"),
		contains("講師はワークショップに熱意を持っていた"),
		contains("気楽に講師とやり取りすることができた"),
		contains("講師は、教える内容について豊富な知識を持っていた"),
		contains("ヘルパーがあなたの学習経験にどのような影響を与えたか"),
		contains("友人や同僚に勧める可能性"),
		slug,
		program,
		start_date = `Start Date (UTC)`,
		end_date = `Submit Date (UTC)`,
		network_id =  `Network ID`
	) %>%
	# Remove two responses that were tests (filled in before the workshop ended)
	filter(end_date > "2021-04-15 01:18:24")

# `このワークショップを友人や同僚に勧める可能性はどのくらいでしょうか？`
partic_post %>%
	select(recommend = contains("友人や同僚に勧める可能性")) %>%
	ggplot(aes(x = recommend)) +
	geom_histogram()  +
	labs(
		title = "このワークショップを友人や同僚に勧める可能性はどのくらいでしょうか？(0-10)",
		subtitle = glue::glue("後調査 n = {nrow(partic_post)}人"),
		y = "回答の数"
	) +
	theme_gray (base_family = "HiraKakuPro-W3", base_size = 14) +
	theme(
		axis.title.x = element_blank()
	)

ggsave("results/post-res-recommend.png", width = 10, height = 8)

partic_post %>%
	select(
		`このワークショップで学んだことを\nすぐに活かすことができる。` = `このワークショップで学んだことをすぐに活かすことができる。`,
		`自分の質問に対する講師の\n回答は明確だった。` = `自分の質問に対する講師の回答は明確だった。`,
		`講師はワークショップに熱意を持っていた。` = `講師はワークショップに熱意を持っていた。`,
		`気楽に講師とやり取りすることができた。` = `気楽に講師とやり取りすることができた。`,
		`講師は、教える内容について豊富な知識\nを持っていた。` = `講師は、教える内容について豊富な知識を持っていた。`
	) %>% 
	pivot_longer(names_to = "question", values_to = "response", everything()) %>%
	filter(!is.na(response)) %>%
	mutate(
		response = factor(response, levels = c(1:5)) %>%
			fct_recode("全くそう思わない（1）" = "1", " どちらでもない（3）" = "3", "強くそう思う（5）" = "5", )) %>%
	ggplot(aes(x = response)) +
	geom_bar() +
	coord_flip() +
	labs(
		title = "ワークショップ後調査",
		subtitle = glue::glue("後調査 n = {nrow(partic_post)}人"),
		y = "回答の数"
		# fill = "時点"
	) +
	facet_wrap(~question, labeller = label_wrap_gen(width = 5)) +
	theme_gray (base_family = "HiraKakuPro-W3", base_size = 14) +
	theme(
		axis.title.y = element_blank(),
		plot.subtitle = element_markdown()
	)

ggsave("results/post-res.png", width = 12, height = 8)


