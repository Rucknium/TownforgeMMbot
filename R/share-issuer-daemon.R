#' Bot for creating limit orders in response to market movements
#'
#' Description
#'
#' @param url.townforged TODO
#' @param url.wallet TODO
#' @param commodity.id TODO
#' @param	bot.account.id TODO
#' @param	loop.sleep.time TODO
#' @param	verbose TODO
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
  verbose = FALSE,
  ...) {

  # NOTE: Do not turn on the bot until it has gotten at least one contrib each in gold and commodity

  shares.to.be.sent.pending.in.mempool <- c()
  gold.shares.to.create.pending.in.mempool <- c()
  commodity.shares.to.create.pending.in.mempool <- c()

  while (TRUE) {
#browser()
    custom.items.df <- get_custom_items(url.townforged = url.townforged)
    # NOTE: Destroyed shares will appear here. This fact prevents shares
    # from being re-issued after being destroyed. The "amount" column
    # will be zero for destroyed items

    nonces.issued.gold.shares <- gsub("(^.*[{])([0-9]+)([}].*$)", "\\2",
      custom.items.df$pdesc[custom.items.df$gold.contrib & (! custom.items.df$is_group) ])

    nonces.issued.commodity.shares <- gsub("(^.*[{])([0-9]+)([}].*$)", "\\2",
      custom.items.df$pdesc[custom.items.df$commodity.contrib & (! custom.items.df$is_group) ])

    gold.contribs.df <- get_gold_contribs(url.townforged = url.townforged,
      bot.account.id = bot.account.id)

    commodity.contribs.df <- get_commodity_contribs(url.townforged = url.townforged,
      commodity.id = commodity.id, bot.account.id = bot.account.id)

    gold.shares.to.create <- setdiff(gold.contribs.df$nonce, nonces.issued.gold.shares)
    commodity.shares.to.create <- setdiff(commodity.contribs.df$nonce, nonces.issued.commodity.shares)
    gold.shares.to.create <- setdiff(gold.shares.to.create, gold.shares.to.create.pending.in.mempool)
    commodity.shares.to.create <- setdiff(commodity.shares.to.create, commodity.shares.to.create.pending.in.mempool)
    # These are in the form of nonces

    for (gold.nonce in gold.shares.to.create) {
      # This loop will skip if gold.shares.to.create is of length zero
      gold.contribs.df.single <- gold.contribs.df[
        gold.contribs.df$nonce == gold.nonce, , drop = FALSE]

      create_share(
        url.townforged = url.townforged,
        url.wallet = url.wallet,
        commodity.id = commodity.id, # commodity.id is actually irrelevant when contrib.type = "gold"
        bot.account.id = bot.account.id,
        contrib.type = "gold",
        contrib.quantity = gold.contribs.df.single$item.quantity,
        contrib.transaction.height = gold.contribs.df.single$height,
        contrib.nonce = gold.nonce,
        contrib.investor =  gold.contribs.df.single$investor.id,
        verbose = verbose)

      gold.shares.to.create.pending.in.mempool <- c(gold.shares.to.create.pending.in.mempool, gold.nonce)
    }

    for (commodity.nonce in commodity.shares.to.create) {

      commodity.contribs.df.single <- commodity.contribs.df[
        commodity.contribs.df$nonce == commodity.nonce, , drop = FALSE]

      create_share(
        url.townforged = url.townforged,
        url.wallet = url.wallet,
        commodity.id = commodity.id,
        bot.account.id = bot.account.id,
        contrib.type = "commodity",
        contrib.quantity = commodity.contribs.df.single$item.quantity,
        # TODO: how much to divide by?
        contrib.transaction.height = commodity.contribs.df.single$height,
        contrib.nonce = commodity.nonce,
        contrib.investor =  commodity.contribs.df.single$investor.id,
        verbose = verbose)

      commodity.shares.to.create.pending.in.mempool <- c(commodity.shares.to.create.pending.in.mempool, commodity.nonce)
    }
# browser()
    # Ok this second part below checks for (recently created) items in the bot's
    # possession that should be sent to investors and then sends them
    # if they exist

    bot.possessed.items.df <- TownforgeR::tf_rpc_curl(url = url.townforged,
      method ="cc_get_account",
      params = list(id = as.numeric(bot.account.id)), num.as.string = TRUE)$result$item_balances

    bot.possessed.items.df <- as.data.frame(matrix(unlist(bot.possessed.items.df), ncol = 2, byrow = TRUE))
    colnames(bot.possessed.items.df) <- c("quantity", "item.id")

    extant.shares.df <- custom.items.df[custom.items.df$creator == bot.account.id &
        (custom.items.df$commodity.contrib | custom.items.df$gold.contrib), , drop = FALSE]

    unsent.shares.id <- intersect(bot.possessed.items.df$item.id, extant.shares.df$id)
    unsent.shares.id <- setdiff(unsent.shares.id, shares.to.be.sent.pending.in.mempool)
    #browser()

    for ( share.to.be.sent in unsent.shares.id) {
      # Will not loop if unsent.shares.id is empty

      recipient.id <- gsub("(^.*[[])([0-9]+)([]].*$)", "\\2",
        extant.shares.df$pdesc[extant.shares.df$id == share.to.be.sent])

      recipient.pub.key <- TownforgeR::tf_rpc_curl(url = url.townforged,
        method ="cc_get_account",
        params = list(id = as.numeric(recipient.id)), num.as.string = TRUE)$result$public_key
      # BTW, the COMMAND_RPC_CC_LOOKUP_ACCOUNT takes public key as input and returns player ID
# browser()
      TownforgeR::tf_rpc_curl(url = url.wallet,
        method ="cc_give",
        params = list(
          public_key = recipient.pub.key,
          items =  "<VERBATIMREPLACE1>"), # c(type = as.numeric(share.to.be.sent), amount = 1)
        verbatim.replace =
          paste0("[{\"type\": ", share.to.be.sent, ", \"amount\": ", 1, "}] ")
          )

      if (verbose) {
        cat(base::date(), "\n\tItem ", share.to.be.sent, " sent to player ", recipient.id, "\n\n", sep = "")
      }

      shares.to.be.sent.pending.in.mempool <- c(shares.to.be.sent.pending.in.mempool, share.to.be.sent)

    }

    #browser()
    Sys.sleep(loop.sleep.time)

  }

}
