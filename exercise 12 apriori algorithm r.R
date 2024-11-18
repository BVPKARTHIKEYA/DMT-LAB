data <- list(
  c("Milk", "Bread"),
  c("Milk", "Bread", "Butter"),
  c("Bread", "Butter"),
  c("Milk", "Cheese"),
  c("Milk", "Bread", "Butter", "Cheese")
)
min_support <- 0.5
min_confidence <- 0.6
total_transactions <- length(data)
item_counts <- table(unlist(data))
frequent_items <- item_counts[item_counts / total_transactions >= min_support]
frequent_items <- names(frequent_items)

cat("Association Rules:\n")

for (item1 in frequent_items) {
  for (item2 in frequent_items) {
    if (item1 != item2) {
      count_item1 <- sum(sapply(data, function(tran) item1 %in% tran))
      count_both <- sum(sapply(data, function(tran) item1 %in% tran && item2 %in% tran))
      confidence <- count_both / count_item1
      if (confidence >= min_confidence) {
        cat(sprintf("Rule: %s -> %s | Confidence: %.2f\n", item1, item2, confidence))
      }
    }
  }
}

