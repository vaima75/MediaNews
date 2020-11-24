library(rvest)
library(tidyr)

emoji_fullList = read_html("https://unicode.org/emoji/charts/full-emoji-list.html")
emoji_DataList = html_table(emoji_fullList)
emoji_Data = emoji_DataList[[1]]
emoji_Data = emoji_Data[-c(1)]

topic = c("Code","face-smiling",
          "face-affection",
          "face-tongue",
          "face-hand",
          "face-neutral-skeptical",
          "face-sleepy",
          "face-unwell",
          "face-hat",
          "face-glasses",
          "face-concerned",
          "face-negative",
          "face-costume",
          "cat-face",
          "monkey-face",
          "emotion",
          "People & Body",
          "hand-fingers-open",
          "hand-fingers-partial",
          "hand-single-finger",
          "hand-fingers-closed",
          "hands",
          "hand-prop",
          "body-parts",
          "person",
          "person-gesture",
          "person-role",
          "person-fantasy",
          "person-activity",
          "person-sport",
          "person-resting",
          "family",
          "person-symbol",
          "Component",
          "hair-style",
          "Animals & Nature",
          "animal-mammal",
          "animal-bird",
          "animal-amphibian",
          "animal-reptile",
          "animal-marine",
          "animal-bug",
          "plant-flower",
          "plant-other",
          "Food & Drink",
          "food-fruit",
          "food-vegetable",
          "food-prepared",
          "food-asian",
          "food-marine",
          "food-sweet",
          "drink",
          "dishware",
          "Travel & Places",
          "place-map",
          "place-geographic",
          "place-building",
          "place-religious",
          "place-other",
          "transport-ground",
          "transport-water",
          "transport-air",
          "hotel",
          "time",
          "sky & weather",
          "Activities",
          "event",
          "award-medal",
          "sport",
          "game",
          "arts & crafts",
          "Objects",
          "clothing",
          "sound",
          "music",
          "musical-instrument",
          "phone",
          "computer",
          "light & video",
          "book-paper",
          "money",
          "mail",
          "writing",
          "office",
          "lock",
          "tool",
          "science",
          "medical",
          "household",
          "other-object",
          "Symbols",
          "transport-sign",
          "warning",
          "arrow",
          "religion",
          "zodiac",
          "av-symbol",
          "gender",
          "math",
          "punctuation",
          "currency",
          "other-symbol",
          "keycap",
          "alphanum",
          "geometric",
          "Flags",
          "flag",
          "country-flag",
          "subdivision-flag")

palt = c("Code","Browser","Appl","Goog","FB","Wind","Twtr","Joy","Sams","Gmail","SB","DCM","KDDI","Description")

trowx = c()
for (t in topic) {
  rowx = which(t == emoji_Data[1])
  trowx = append(trowx, rowx)
}

emoji_Data = emoji_Data[-trowx,]
colnames(emoji_Data) = palt
rownames(emoji_Data) <- NULL

### Determine Maximum no of unicodes
# tcolx = c()
# for (i in 1:nrow(emoji_Data)) {
#   colx = str_count(emoji_Data[i,1],pattern = "\\s+")
#   tcolx = append(tcolx, colx)
# }
### Altenative
# sapply(emoji_Data$Code, function(x){
#   colx = str_count(x,pattern = "\\s+")
# })

emoji_Data = separate(emoji_Data, col = Code,
                       into = c("C1","C2","C3","C4","C5","C6","C7","C8"),
                       sep = "\\s+")

for (x in 1:8) emoji_Data[,x] = gsub(pattern = "(U\\+[0-9A-Fa-f]{4,8})", replacement = "<\\1>", x = emoji_Data[,x])

write.csv(emoji_Data, file = "data-raw/emoji_Data.csv", row.names = F)
save(emoji_Data, file = "data/emoji_Data.rda")
