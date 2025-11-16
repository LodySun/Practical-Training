set.seed(42)

# reference cards (slot -> feature)
slot_shape <- c("circle","triangle","cross","star")
slot_color <- c("red","green","blue","yellow")
slot_num   <- c("1","2","3","4")
slot_dir   <- c("up","right","down","left")

# mappings
shape2slot <- c(circle=1, triangle=2, cross=3, star=4)
color2slot <- c(red=1, green=2, blue=3, yellow=4)
num2slot   <- c("1"=1, "2"=2, "3"=3, "4"=4)
dir2slot   <- c(up=1, right=2, down=3, left=4)

# stimulus pool
shapes  <- c("triangle","cross","circle","star")
colors  <- c("red","green","yellow","blue")
numbers <- c("1","2","3","4")
dirs    <- c("up","right","down","left")

stim <- expand.grid(shape=shapes, number=numbers, color=colors, dir=dirs,
                    stringsAsFactors = FALSE)
stim$card <- paste0(stim$shape, stim$number, stim$color, "_", stim$dir)

# helpers
parse_card <- function(card) {
  m <- regexec("^([a-z]+)([1-4])([a-z]+)_([a-z]+)$", card)
  g <- regmatches(card, m)[[1]]
  if (length(g) != 5) return(NULL)
  list(shape=g[2], number=g[3], color=g[4], dir=g[5])
}

match_count_with_slot <- function(card_feats, j) {
  sum(c(
    card_feats$shape  == slot_shape[j],
    card_feats$color  == slot_color[j],
    card_feats$number == slot_num[j],
    card_feats$dir    == slot_dir[j]
  ))
}

correct_slot_for_rule <- function(card_feats, rule) {
  if (rule == "shape")     return(shape2slot[card_feats$shape])
  if (rule == "color")     return(color2slot[card_feats$color])
  if (rule == "number")    return(num2slot[card_feats$number])
  if (rule == "direction") return(dir2slot[card_feats$dir])
  stop("unknown rule")
}

is_unique_match <- function(card_feats, rule, strict_other_slots=TRUE) {
  k <- correct_slot_for_rule(card_feats, rule)
  if (match_count_with_slot(card_feats, k) != 1) return(FALSE)
  if (strict_other_slots) {
    for (j in setdiff(1:4, k)) {
      if (match_count_with_slot(card_feats, j) >= 2) return(FALSE)
    }
  }
  TRUE
}

# generate table
blocks <- 10
trials_per_rule <- 10
rules <- c("color","shape","number","direction")

rows <- list()
prev_rule <- NA

for (b in 1:blocks) {
  for (r in sample(rules)) {
    i_in_block <- 0
    while (i_in_block < trials_per_rule) {
      idx <- sample(nrow(stim), 1)
      cf  <- as.list(stim[idx, c("shape","number","color","dir")])
      if (!is_unique_match(cf, r, TRUE)) next
      i_in_block <- i_in_block + 1
      card  <- stim$card[idx]
      k     <- correct_slot_for_rule(cf, r)
      prev  <- if (is.na(prev_rule)) 0 else correct_slot_for_rule(cf, prev_rule)
      seqn  <- i_in_block
      ruleq <- paste0('"', r, '"')
      descq <- paste0('"', card, '"')
      rows[[length(rows)+1]] <- c(card, k, prev, seqn, ruleq, descq)
    }
    prev_rule <- r
  }
}

wcst <- do.call(rbind, rows)

write.table(wcst, file="wcst4d.txt",
            sep=" ", quote=FALSE, row.names=FALSE, col.names=FALSE)

cat(sprintf("Done! wcst4d.txt created (%d rows)\n", nrow(wcst)))

