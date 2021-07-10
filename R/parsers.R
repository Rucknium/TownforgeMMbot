#' Produces a data.frame containing instances of investors sending the bot gold
#'
#' Description
#'
#' @param url.townforged TODO
#' @param	bot.account.id TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
get_gold_investors <- function(
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  bot.account.id,
  ...) {

  gold.receipt.ls <- TownforgeR::tf_rpc_curl(url = url.townforged,
    method ="cc_get_game_events",
    params = list(cmd = 2), num.as.string = TRUE)$result$events
  # TODO: Can narrow down the block height with min_height argument, which may be faster

  gold.receipt.ls <- unlist(gold.receipt.ls)
  stopifnot(length(gold.receipt.ls) %% 9 == 0 )  # "%%" is the modulo operation

  gold.receipt.df <- as.data.frame(matrix(gold.receipt.ls, ncol = 9, byrow = TRUE))
  colnames(gold.receipt.df) <- c("account", "balance", "cmd", "event", "height",
    "items.amount", "items.type", "nonce", "tx_fee")
  # NOTE: This relies upon the return value having exactly 9 elements, which is safe for now, but might always be the case.
  gold.receipt.df$balance <- as.numeric(gold.receipt.df$balance)
  receipt.nonces <- gold.receipt.df$nonce[gold.receipt.df$balance > 0 & gold.receipt.df$account == bot.account.id]
  # When someone sends gold too someone else, two record show up, one
  # on the receiving end and the other on the sending side, sharing the same nonce

  gold.receipt.df <- gold.receipt.df[gold.receipt.df$balance < 0 & gold.receipt.df$nonce %in% receipt.nonces, , drop = FALSE]
  colnames(gold.receipt.df)[colnames(gold.receipt.df) == "account"] <- "investor.id"
  gold.receipt.df$item.id <- "gold"
  gold.receipt.df$item.quantity <- gold.receipt.df$balance

  gold.receipt.df[, c("investor.id", "item.id", "item.quantity", "event", "height", "nonce", "tx_fee")]

}






#' Produces a data.frame containing instances of investors sending the bot the commodity under its management
#'
#' Description
#'
#' @param url.townforged TODO
#' @param commodity.id TODO
#' @param	bot.account.id TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
get_commodity_investors  <- function(
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  commodity.id,
  bot.account.id,
  ...) {

  commodity.receipt.ls <- TownforgeR::tf_rpc_curl(url = url.townforged,
    method ="cc_get_game_events",
    params = list(cmd = 16, item = commodity.id, account = bot.account.id), num.as.string = TRUE)$result$events

  commodity.receipt.ls <- lapply(commodity.receipt.ls, FUN = function(x) {
    if (substr(x$event, 1, 8) != "Received") { return(NULL) }
    items.df <- as.data.frame(matrix(as.numeric(unlist(x$items)), ncol = 2, byrow = TRUE))
    colnames(items.df) <- c("items.amount", "items.type")
    item.quantity <- sum(items.df$items.amount[items.df$items.type == commodity.id])
    # This guards against the possibility that more than one type of item is sent
    cbind(as.data.frame(x[names(x) != "items"]),
      data.frame(item.quantity = item.quantity, item.id = commodity.id, stringsAsFactors = FALSE) )
  })
  commodity.receipt.df <- do.call(rbind, commodity.receipt.ls)
  colnames(commodity.receipt.df)[colnames(commodity.receipt.df) == "counterparties"] <- "investor.id"
  col.rearrange <- c("investor.id", "item.id", "item.quantity", "event", "height", "nonce", "tx_fee")
  commodity.receipt.df[, col.rearrange, drop = FALSE]
}


#' Produces a data.frame containing player-made items
#'
#' Description
#'
#' @param url.townforged TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
get_custom_items <- function(
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  ...) {

  custom.items.df <- unlist(TownforgeR::tf_rpc_curl(
    url = url.townforged, method ="cc_get_custom_items",
    params = list(), num.as.string = TRUE)$result$items)
  # TODO: figure out how to deal with issue of inability to pass a vector data type bto
  # the ids argument when I actually just want one of them, which is required by the method

  stopifnot(length(custom.items.df) %% 21 == 0 )  # "%%" is the modulo operation
  # Relying on the "flattened" return being of length 21 always

  custom.items.df <- as.data.frame(matrix(custom.items.df, ncol = 21, byrow = TRUE))
  colnames(custom.items.df) <- c("amount", "coin_design", "creation_height", "creator", "gold",
    "group", "hash", "id", "ignore", "ipfs_data", "ipfs_error", "ipfs_multihash", "is_group",
    "is_public", "name", "pdesc", "sdesc", "user_data.1", "user_data.2", "user_data.3", "user_data.4")

  custom.items.df$is_group <- as.logical(custom.items.df$is_group)
  custom.items.df
}
