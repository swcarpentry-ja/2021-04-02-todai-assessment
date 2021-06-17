# Visualize pre-workshop answers

library(tidyverse)

# Load the participants responses
# The `#` column has a unique ID for each answer, but this is not the same
# as the user-entered unique ID (those are unfortunately NA in some cases as the
# participant didn't answer that question)
partic <- read_csv("data/pre-workshop-response-2021-04-02.csv") %>%
  rename(num = `#`)

#' Define helper function to count responses to multiple-answer questions
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
    mutate(value = fct_reorder(value, n))
}

# Plot multiple-answer responses
# Area of research
pivot_survey(partic, num, `農業もしくは環境科学`:Other) %>%
  ggplot(aes(x = n, y = value)) +
  geom_col() +
  labs(title = "あなたの専門もしくは関連分野について教えてください") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.y = element_blank())

# Job
pivot_survey(partic, num, 事務職:Other_1) %>%
  ggplot(aes(x = n, y = value)) +
  geom_col() +
  labs(title = "現在の職業またはキャリアレベルを教えてください") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.y = element_blank())

# Reason for participating
pivot_survey(partic, num, `新しいスキルを習得するため。` :Other_2) %>%
  ggplot(aes(x = n, y = value)) +
  geom_col() +
  labs(title = "このワークショップに参加する理由を教えてください") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.y = element_blank())

# How they heard about the workshop
pivot_survey(partic, num, `ワークショップに関する E メールもしくはチラシを受け取った。`:Other_3) %>%
  ggplot(aes(x = n, y = value)) +
  geom_col() +
  labs(title = "このワークショップをどのようにして知りましたか？") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.y = element_blank())

# Other questions have one column per answer, but the column names are very long
# (they are the actual questions).
#
# The following questions are also included in the post-workshop survey, so 
# ignore those for now, and analyze them separately:
# agree_raw_data = `元の生データにアクセスできることは、分析を再現するために重要である。`,
# agree_write_script = `仕事上、小さなプログラム、スクリプトもしくはマクロを問題解決のために作成することが出来る。`,
# agree_find_answer_online = `技術的な質問の答えをオンラインで探す方法が分かる。`,
# agree_find_solution = `プログラミングをしていて行き詰ったときに、その問題を解決する手法を見つけることが出来る。`,
# agree_program_confidence = `自分のプログラミングソフトウェアによるデータ解析能力に自信がある。`,
# agree_program_reproduce = `プログラミング言語（RやPythonなど）を使うことで、自分の分析をより再現しやすくすることができる。`,
# agree_program_efficiency = `プログラミング言語（RやPythonなど）を使用することで、データの処理をより効率的に行うことができる。`,

simple_answers_lookup <- c(
  which_workshop = "どのワークショップに参加しますか？",
  os = "ワークショップで使用するコンピュータのオペレーティングシステムを教えてください。",
  use_gui = "ポイントアンドクリックグラフィックユーザーインターフェースに特化したソフトウェア (例: 統計解析: SPSS、SAS、など; 地理学的解析: ArcGIS、 QGIS、など; ゲノム解析: Geneious、など)",
  use_program = "プログラム言語 (R、パイソンなど)",
  use_database = "データベース (SQL、アクセスなど)",
  use_version_control = "バージョン管理ソフトウェア (Git、Subversion (SVN)、Mercurial、など)",
  use_shell = "コマンドシェル (通常マック OS やウィンドウズのパワーシェルを介してターミナルにアクセス)",
  workflow_satisfaction = "あなたの現在のデータマネジメントと解析に関するワークフロー(どのようにデータを収集、整理、保存そして解析するか)の満足度を教えてください。",
  want_to_learn = "このワークショップから学びたいことを教えてください。")

simple_answers <-
  select(partic, num, all_of(simple_answers_lookup)) %>%
  pivot_longer(-num, names_to = "q_short", values_to = "answer") %>%
  filter(!is.na(answer)) %>%
  count(q_short, answer) %>%
  left_join(
    tibble(q_short = names(simple_answers_lookup), q_long = simple_answers_lookup), 
    by = "q_short")

plot_simple_answer <- function(simple_answers, q_short_select) {
  simple_answers_sub <-
    simple_answers %>%
    filter(q_short == q_short_select) %>%
    mutate(answer = fct_reorder(answer, n))
  
  ggplot(simple_answers_sub, aes(x = n, y = answer)) +
    geom_col() +
    labs(title = unique(simple_answers_sub$q_long)) +
    theme_gray (base_family = "HiraKakuPro-W3") +
    theme(axis.title.y = element_blank())
}

plot_simple_answer(simple_answers, "os")
plot_simple_answer(simple_answers, "use_gui")
plot_simple_answer(simple_answers, "use_program")
plot_simple_answer(simple_answers, "use_database")
plot_simple_answer(simple_answers, "use_version_control")
plot_simple_answer(simple_answers, "use_shell")
plot_simple_answer(simple_answers, "workflow_satisfaction")
