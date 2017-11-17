words <- "
Karl Karl Karl Karl Karl
Cheri Cheri Cheri Cheri Cheri

Di Di Di
Heike Heike Heike
Hadley Hadley Hadley

Doug Doug Doug
Doerge Doerge Doerge
Ward Ward Ward

Tracy Tracy Tracy Tracy
Jiasen Jiasen Jiasen Jiasen
Gui Gui Gui Gui
Xiaosu Xiaosu Xiaosu Xiaosu
Brandon Brandon Brandon Brandon

Jeremy Jeremy
Aritra Aritra
Sarah Sarah
Yuying Yuying
Jeff Jeff
Philip Philip
Jeanine Jeanine

Ayu Ayu
Emery Emery
Zach Zach
Kelly-Ann Kelly-Ann

Crossfit
Poker

Patti
Holly
Roxie
Beth
Alicia
Jessie
Mary

Fulya

Megan
Hakeem
Rongrong
April
Courtney
David
Lori
Dominique
Murphy
Reese
Sophie
Yumin
Eric
Raquel
Kevin
Débora
Tim
Yixuan
Will
Kara
Ningning
Ting

"

library(magrittr)

pal <- RColorBrewer::brewer.pal(6, "Pastel1")
# pal <- RColorBrewer::brewer.pal(6, "Set1")
words <- stringr::str_replace_all(words, "\n", " ") %>%
  stringr::str_replace("^ ", "") %>%
  stringr::str_replace(" $", "")
while(stringr::str_detect(words, "  ")) {
  words <- stringr::str_replace_all(words, "  ", " ")
}

par(bg="grey20")
seed_num = 1234

print_with_seed <- function(seed_num) {
  print(seed_num)
  set.seed(seed_num)
  par(bg="grey20")
  words %>%
    strsplit(" ") %>%
    extract2(1) %>%
    table() %>%
    as.data.frame() %>%
    set_colnames(c("name", "freq")) ->
  word_freq
  wordcloud::wordcloud(
    words = word_freq$name,
    freq = word_freq$freq,
    scale = c(5,0.5),
    min.freq = 1,
    rot.per = 0,
    random.order = TRUE,
    colors = rev(pal[c(1,3,2,4,5)])
  )
  # text(0,0, seed_num, col = "white")
}

pdf("word_cloud.pdf")
for (seed_num in 1:1000) {
  print_with_seed(seed_num)
}
dev.off()

png("word_cloud.png")
print_with_seed(270)
dev.off()
