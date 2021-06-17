# Compare pre-post workshop survey answers

library(tidyverse)
library(ggtext)

# Load the participants responses for the pre-workshop survey
path_to_csv_file_pre <- "data/pre-workshop-response-2021-04-02.csv"

# Load the participants responses
# The `#` column has a unique ID for each answer, but this is not the same
# as the user-entered unique ID (those are unfortunately NA in some cases as the
# participant didn't answer that question)
partic_pre <- read_csv(path_to_csv_file_pre) %>%
	rename(num = `#`)

# Load the participants responses for the post-workshop survey
# The `#` column has a unique ID for each answer, but this is not the same
# as the user-entered unique ID (those are unfortunately NA in some cases as the
# participant didn't answer that question)
path_to_csv_file_post <- "data/post-workshop-response-2021-04-22.csv"

partic_post <- read_csv(path_to_csv_file_post) %>%
	rename(num = `#`,
		start_date = `Start Date (UTC)`,
		end_date = `Submit Date (UTC)`,
		network_id =  `Network ID`
	) %>%
	# Remove two responses that were tests (filled in before the workshop ended)
	filter(end_date > "2021-04-15 01:18:24")

common_cols <- intersect(colnames(partic_pre), colnames(partic_post))

partic_common <- 
	bind_rows(
		partic_pre %>% select(all_of(common_cols)) %>% mutate(when = "pre"),
		partic_post %>% select(all_of(common_cols)) %>% mutate(when = "post")
	) %>%
	select(
		-matches("num|このアンケートを受けることに同意しますか？|18歳以上の方ですか？|固有の ID を以下のように入力してください|slug|program")
	)

partic_common %>%
	rename(
		`元の生データにアクセスできることは、\n分析を再現するために重要である。`= `元の生データにアクセスできることは、分析を再現するために重要である。`,
		`仕事上、小さなプログラム、スクリプト\nもしくはマクロを問題解決のために\n作成することが出来る。` = `仕事上、小さなプログラム、スクリプトもしくはマクロを問題解決のために作成することが出来る。`,
		`技術的な質問の答えをオンラインで\n探す方法が分かる。` = `技術的な質問の答えをオンラインで探す方法が分かる。`,
		`自分のプログラミングソフトウェアによる\nデータ解析能力に自信がある。` = `自分のプログラミングソフトウェアによるデータ解析能力に自信がある。`,
		`プログラミング言語（RやPythonなど）\nを使うことで、自分の分析をより再現\nしやすくすることができる。` = `プログラミング言語（RやPythonなど）を使うことで、自分の分析をより再現しやすくすることができる。`,
		`プログラミング言語（RやPythonなど）\nを使用することで、データの処理をより\n効率的に行うことができる。` = `プログラミング言語（RやPythonなど）を使用することで、データの処理をより効率的に行うことができる。`
	) %>%
	pivot_longer(names_to = "question", values_to = "response", -when) %>%
	filter(!is.na(response)) %>%
	mutate(
		response = factor(response, levels = c(1:5)) %>%
			fct_recode("全くそう思わない（1）" = "1", " どちらでもない（3）" = "3", "強くそう思う（5）" = "5", )) %>%
	ggplot(aes(x = response, fill = when)) +
	geom_bar(position = position_dodge(preserve = "single")) +
	scale_fill_manual(
		labels = c("前", "後"), 
		breaks = c("pre", "post"),
		values = c(
			pre = "#ef8a62",  # red  
			post = "#67a9cf") # blue 
		) +
	coord_flip() +
	labs(
		title = "ワークショップ前後調査",
		subtitle = glue::glue("前調査 n = {nrow(partic_pre)}人、後調査 n = {nrow(partic_post)}人"),
		y = "回答の数",
		fill = "時点"
	) +
	facet_wrap(~question, labeller = label_wrap_gen(width = 5)) +
	theme_gray (base_family = "HiraKakuPro-W3", base_size = 14) +
	theme(
		axis.title.y = element_blank(),
		plot.subtitle = element_markdown()
	)

ggsave("results/pre-post-comp.png", width = 12, height = 8)
