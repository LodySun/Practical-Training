set.seed(42)

# ---- reference cards: slot -> feature ----
slot_shape <- c("circle","triangle","cross","star")
slot_color <- c("red","green","blue","yellow")
slot_num   <- c("1","2","3","4")
slot_dir   <- c("up","right","down","left")

# ---- feature -> slot (consistent with reference cards) ----
shape2slot <- c(circle = 1, triangle = 2, cross = 3, star = 4)
color2slot <- c(red = 1, green = 2, blue = 3, yellow = 4)
num2slot   <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4)
dir2slot   <- c(up = 1, right = 2, down = 3, left = 4)

# ---- stimulus pool ----
stim <- expand.grid(
  shape = c("triangle","cross","circle","star"),
  number = c("1","2","3","4"),
  color = c("red","green","yellow","blue"),
  dir = c("up","right","down","left"),
  stringsAsFactors=FALSE
)
stim$card <- paste0(stim$shape, stim$number, stim$color, "_", stim$dir)

# ---- helpers (minimal set) ----
parse_card <- function(card) {
  g <- regmatches(card, regexec("^([a-z]+)([1-4])([a-z]+)_([a-z]+)$", card))[[1]]
  if (length(g) != 5) stop("Bad card format: ", card)
  list(shape=g[2], number=g[3], color=g[4], dir=g[5])
}

match_count <- function(cf, j) {
  sum(cf$shape == slot_shape[j], cf$color == slot_color[j],
      cf$number == slot_num[j], cf$dir == slot_dir[j])
}

slot_for_rule <- function(cf, rule) {
  switch(rule,
         shape = unname(shape2slot[cf$shape]),
         color = unname(color2slot[cf$color]),
         number = unname(num2slot[cf$number]),
         direction = unname(dir2slot[cf$dir]),
         stop("Unknown rule: ", rule))
}

unique_ok <- function(cf, rule) {
  k <- slot_for_rule(cf, rule)
  if (match_count(cf, k) != 1) return(FALSE)
  for (j in setdiff(1:4, k)) if (match_count(cf, j) >= 2) return(FALSE)
  TRUE
}

q <- function(x) ifelse(grepl('^".*"$', x), x, paste0('"', x, '"'))

# ---- generate then "fix" columns inline ----
blocks <- 10
trials_per_rule <- 10
rules <- c("color","shape","number","direction")

out_rows <- vector("list", blocks * length(rules) * trials_per_rule)
rowi <- 0
prev_slot2 <- 0
prev_block_last_rule <- NA_character_

for (b in 1:blocks) {
  rule_order <- sample(rules)
  # Enforce a rule switch at block boundary:
  # first rule of current block must differ from last rule of previous block.
  if (!is.na(prev_block_last_rule) && rule_order[1] == prev_block_last_rule) {
    swap_idx <- which(rule_order != prev_block_last_rule)[1]
    tmp <- rule_order[1]
    rule_order[1] <- rule_order[swap_idx]
    rule_order[swap_idx] <- tmp
  }

  for (r in rule_order) {
    for (seqn in 1:trials_per_rule) {
      repeat {
        idx <- sample(nrow(stim), 1)
        cf  <- as.list(stim[idx, c("shape","number","color","dir")])
        if (unique_ok(cf, r)) break
      }
      card  <- stim$card[idx]
      slot2 <- slot_for_rule(cf, r)
      slot3 <- if (seqn == 1) 0 else prev_slot2
      prev_slot2 <- slot2
      
      rowi <- rowi + 1
      out_rows[[rowi]] <- c(card, slot2, slot3, seqn, q(r), q(card))
    }
  }
  prev_block_last_rule <- rule_order[length(rule_order)]
}

out <- do.call(rbind, out_rows[1:rowi])

outfile <- "wcst4d.txt"
write.table(out, file = outfile, sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(sprintf("Done! %s created (%d rows)\n", outfile, nrow(out)))

