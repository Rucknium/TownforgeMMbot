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
      name = paste0(commodity.name, " MM bot share - commodity investment"),
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
      name = paste0(commodity.name, " MM bot share - gold investment"),
      is_group = TRUE,
      is_public = FALSE,
      primary_description = paste0(commodity.name, " placeholder."),
      secondary_description = ""),
    nonce.as.string = TRUE)

  return(invisible(NULL))

}
