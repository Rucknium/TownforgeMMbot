#' Creates two item groups. One for shares issued to investors who contribute commodity; one for gold contributors.
#'
#' Description
#'
#' @param url.wallet TODO
#' @param	commodity.id TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
create_share_item_groups <- function(
  # URL for TF RPC connection
  url.wallet = "http://127.0.0.1:63079/json_rpc",
  commodity.id,
  ...) {

  commodity.id.key <- as.data.frame(matrix(c(
    1, "Sandstone",
    2, "Granite",
    3, "Marble",
    4, "Pine",
    5, "Oak",
    6, "Teak",
    8, "Runestone",
    256, "Labour",
    257, "Firewood",
    1024, "Vegetables",
    1025, "Grain",
    1026, "Meat",
    1027, "Salted meat"
  ), ncol = 2, byrow = TRUE))
  colnames(commodity.id.key) <- c("id", "name")
  # TODO: make this a data object in the package

  commodity.name <- commodity.id.key$name[commodity.id.key$id == commodity.id]

  TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_new_item",
    params = list(
      name = paste0(commodity.name, " MM bot share - commodity contribution"),
      is_group = TRUE,
      is_public = FALSE,
      primary_description = paste0(commodity.name, " placeholder."),
      secondary_description = ""),
    #do_not_relay = TRUE, get_tx_metadata = TRUE, get_tx_hex = TRUE),
    nonce.as.string = TRUE)
  # I don't really get any additional info from the return value,,, so can just discard.
  # Need to use cc_get_custom_items to get info

  # Primary max number of characters is e between 4000 and 4500 characters
  # Same for secondary

  TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_new_item",
    params = list(
      name = paste0(commodity.name, " MM bot share - gold contribution"),
      is_group = TRUE,
      is_public = FALSE,
      primary_description = paste0(commodity.name, " placeholder."),
      secondary_description = ""),
    nonce.as.string = TRUE)

  return(invisible(NULL))
}


#' Creates shares. Option for gold contribution or commodity contribution.
#'
#' Description
#'
#' @param url.wallet TODO
#' @param	commodity.id TODO
#' @param	contrib.type TODO
#' @param	contrib.quantity TODO
#' @param	contrib.transaction.height TODO
#' @param	contrib.nonce TODO
#' @param	contrib.investor TODO
#' @param	verbose TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
create_share <- function(
  # URL for TF RPC connection
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  url.wallet = "http://127.0.0.1:63079/json_rpc",
  commodity.id,
  bot.account.id,
  contrib.type,
  contrib.quantity,
  contrib.transaction.height,
  contrib.nonce,
  contrib.investor,
  verbose = FALSE,
  ...) {
#browser()
  stopifnot(length(contrib.type) == 1 && contrib.type %in% c("gold", "commodity"))

  commodity.id.key <- as.data.frame(matrix(c(
    1, "Sandstone",
    2, "Granite",
    3, "Marble",
    4, "Pine",
    5, "Oak",
    6, "Teak",
    8, "Runestone",
    256, "Labour",
    257, "Firewood",
    1024, "Vegetables",
    1025, "Grain",
    1026, "Meat",
    1027, "Salted meat"
  ), ncol = 2, byrow = TRUE))
  colnames(commodity.id.key) <- c("id", "name")
  # TODO: make this a data object in the package

  commodity.name <- commodity.id.key$name[commodity.id.key$id == commodity.id]

  custom.items.df <- get_custom_items(url.townforged = url.townforged)

  gold.contrib.id <- custom.items.df$id[custom.items.df$creator == bot.account.id &
      custom.items.df$is_group & custom.items.df$gold.contrib]
  # For item groups, "id" is the group id and "group" is 0. For items within a group,
  # "id" is the item id and "group" is the id of the group that it belongs to.

  commodity.contrib.id <- custom.items.df$id[custom.items.df$creator == bot.account.id &
      custom.items.df$is_group & custom.items.df$commodity.contrib]

  if (contrib.type == "gold") {
    contrib.quantity <- contrib.quantity/10e+7
  }
  contrib.quantity.str <- formatC(contrib.quantity, big.mark = ",",
    format = "f", drop0trailing = TRUE, digits = 3)

  primary.desciption <- paste0("\t", commodity.name, " MM bot share: ", contrib.quantity.str,
    " ", contrib.type, "\n\tContribution at block height: <", contrib.transaction.height,
    ">\n\tNonce of contribution transaction: {", contrib.nonce,
    "}\n\tInitially issued to player ID: [", contrib.investor,
    "]\n\tMM bot share version: |0.1|")

  if (verbose) {
    cat(base::date(), "\n", primary.desciption, "\n\n", sep = "")
  }

  TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_new_item",
    params = list(
      name = paste0(commodity.name, " MM bot share, ", contrib.quantity.str,
        " ", contrib.type, " contribution"),
      amount = 1,
      is_group = FALSE,
      is_public = FALSE,
      group = as.numeric(ifelse(contrib.type == "gold", gold.contrib.id, commodity.contrib.id)),
      primary_description = primary.desciption,
      secondary_description = ""))
  # NOTE: Have to pass as numeric the arguments that the RPC expects as numeric. Cannot be strings
  # TODO: not sure if it's best to put the ID of the initial recipient
  # in the primary (immutable) or secondary (mutable) field

  return(invisible(NULL))
}
