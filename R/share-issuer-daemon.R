#' Bot for creating limit orders in response to market movements
#'
#' Description
#'
#' @param url.townforged TODO
#' @param url.wallet TODO
#' @param commodity.id TODO
#' @param	bot.account.id TODO
#' @param	loop.sleep.time TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
share_issuer_daemon <- function(
  # URL for TF RPC connection
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  url.wallet = "http://127.0.0.1:63079/json_rpc",
  commodity.id,
  bot.account.id,
  loop.sleep.time = 10, # Time in seconds
  ...) {

  while (TRUE) {

    custom.items.df <- get_custom_items(url.townforged = url.townforged)
    # NOTE: Do not turn on the bot until it has gotten at least one contrib each in gold and commodity

    nonces.issued.gold.shares <- gsub("(^.*[{])([0-9]+)([}].*$)", "\\2",
      custom.items.df$pdesc[custom.items.df$gold.contrib])

    nonces.issued.commodity.shares <- gsub("(^.*[{])([0-9]+)([}].*$)", "\\2",
      custom.items.df$pdesc[custom.items.df$commodity.contrib])

    gold.contribs.df <- get_gold_contribs(url.townforged = url.townforged,
      bot.account.id = bot.account.id)

    commodity.contribs.df <- get_commodity_contribs(url.townforged = url.townforged,
      commodity.id = commodity.id, bot.account.id = bot.account.id)

    gold.shares.to.issue <- setdiff(gold.contribs.df$nonce, nonces.issued.gold.shares)
    commodity.shares.to.issue <- setdiff(commodity.contribs.df$nonce, nonces.issued.commodity.shares)
    # These are in the form of nonces

    for (gold.nonce in gold.shares.to.issue) {
      # This loop will skip if gold.shares.to.issue is of length zero
      gold.contribs.df.single <- gold.contribs.df[
        gold.contribs.df$nonce == gold.nonce, , drop = FALSE]

      create_share(
        url.wallet = url.wallet,
        commodity.id = commodity.id, # commodity.id is actually irrelevant when contrib.type = "gold"
        contrib.type = "gold",
        contrib.quantity = gold.contribs.df.single$item.quantity,
        # TODO: how much to divide by?
        contrib.transaction.height = gold.contribs.df.single$height,
        contrib.nonce = gold.nonce,
        contrib.investor =  gold.contribs.df.single$investor.id)
    }

    for (commodity.nonce in commodity.shares.to.issue) {

      commodity.contribs.df.single <- commodity.contribs.df[
        commodity.contribs.df$nonce == commodity.nonce, , drop = FALSE]

      create_share(
        url.wallet = url.wallet,
        commodity.id = commodity.id,
        contrib.type = "commodity",
        contrib.quantity = commodity.contribs.df.single$item.quantity,
        # TODO: how much to divide by?
        contrib.transaction.height = commodity.contribs.df.single$height,
        contrib.nonce = commodity.nonce,
        contrib.investor =  commodity.contribs.df.single$investor.id)
    }

    # Ok this second part below checks for (recently created) items in the bot's
    # possession that should be sent to investors and then sends them
    # if they exist

    bot.possessed.items.df <- TownforgeR::tf_rpc_curl(url = url.townforged,
      method ="cc_get_account",
      params = list(id = bot.account.id), num.as.string = TRUE)$result$item_balances

    bot.possessed.items.df <- as.data.frame(as.matrix(unlist(bot.possessed.items.df), ncol = 2, byrow = TRUE))
    colnames(bot.possessed.items.df) <- c("quantity", "item.id")

    extant.shares.df <- custom.items.df$id[custom.items.df$creator == bot.account.id &
        (custom.items.df$commodity.contrib | custom.items.df$gold.contrib), drop = FALSE]

    unsent.shares.id <- intersect(bot.possessed.items.df$item.id, extant.shares.df$id)

    for ( shares.to.be.sent in unsent.shares.id) {
      # Will not loop if unsent.shares.id is empty

      recipient.id <- gsub("(^.*[[])([0-9]+)([]].*$)", "\\2",
        extant.shares.df$pdesc[extant.shares.df$id == shares.to.be.sent])

      recipient.pub.key <- TownforgeR::tf_rpc_curl(url = url.townforged,
        method ="cc_get_account",
        params = list(id = recipient.id), num.as.string = TRUE)$result$public_key
      # BTW, the COMMAND_RPC_CC_LOOKUP_ACCOUNT takes public key as input and returns player ID

      TownforgeR::tf_rpc_curl(url = url.wallet,
        method ="cc_give",
        params = list(type = unsent.shares.id, amount = 1, public_key = recipient.pub.key))
    }

    Sys.sleep(loop.sleep.time)


  }


}
