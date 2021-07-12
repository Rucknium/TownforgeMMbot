#' Bot for creating limit orders in response to market movements
#'
#' Description
#'
#' @param url.townforged TODO
#' @param url.wallet TODO
#' @param commodity.id TODO
#' @param	bot.account.id TODO
#' @param	order.quantity TODO
#' @param	price.increment TODO
#' @param	price.steps TODO
#' @param	init.midpoint.price TODO
#' @param	loop.sleep.time TODO
#' @param ... TODO
#'
#' @details TODO
#'
#' @export
mm_daemon <- function(
  # URL for TF RPC connection
  url.townforged = "http://127.0.0.1:18881/json_rpc", # "http://127.0.0.1:28881/json_rpc"
  url.wallet = "http://127.0.0.1:63079/json_rpc",
  commodity.id,
  bot.account.id,
  order.quantity,
  price.increment,
  price.steps,
  init.midpoint.price,
  cancel.all.orders.and.exit = FALSE,
  loop.sleep.time = 10,
  verbose = FALSE,
  ...) {


  if (cancel.all.orders.and.exit) {
    order.bk.df <- TownforgeR::tf_parse_markets(url = url.townforged)
    order.bk.df <- order.bk.df[order.bk.df$account == bot.account.id & order.bk.df$id == commodity.id, , drop = FALSE]

    for (i in order.bk.df$nonce) {
      TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_cancel_nonce",
        params = list(nonce = i ))
    }
    return(invisible(NULL))
  }




  current.height <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="get_block_count",
    params = list(), num.as.string = FALSE)$result$count

  lookback.height <- min(2 * 60 * 24, current.height)
  # Look at most recent 24 hours of blocks just to be safe.

  # TODO: every X% of times, look back all the way, with runif()

  TownforgeR::tf_rpc_curl(url = url.townforged, method ="cc_get_account", params = list(id = 7))


  # initiate liquidity


  bid.price <- init.midpoint.price - price.increment * seq_len(price.steps)
  ask.price <- init.midpoint.price + price.increment * seq_len(price.steps)

  for (i in bid.price) {
    # i <- bid.price[1]
    print(TownforgeR::tf_rpc_curl(url = url.wallet, method ="cc_trade_item",
      params = list(
        bid = TRUE,
        id = commodity.id,
        amount = order.quantity,
        price = formatC(i * 1e+08, format = "fg")),
      num.as.string = TRUE))
  }

  for (i in ask.price) {
    # i <- bid.price[1]
    print(TownforgeR::tf_rpc_curl(url = url.wallet, method ="cc_trade_item",
      params = list(
        bid = FALSE,
        id = commodity.id,
        amount = order.quantity,
        price = formatC(i * 1e+08, format = "fg")),
      num.as.string = TRUE))
  }

  # data.table::%chin%

  existing.nonces <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="cc_get_game_events",
    params = list(cmd = 7, item = commodity.id), num.as.string = TRUE)$result$events

  existing.nonces <- sapply(existing.nonces, FUN = function(x) {
    if (substr(x[["event"]], 1, 4) == "Sold") {
      x[["nonce"]]
    } else {
      return(NULL)
    }
  })

  existing.nonces <- unlist(existing.nonces)



  while (TRUE) {

    Sys.sleep(loop.sleep.time)

    #data.table::`%chin%`("a" , "a" )
    #data.table::fsetdiff("a" , "a" )

    current.height <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="get_block_count",
      params = list(), num.as.string = FALSE)$result$count

    recent.trades <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="cc_get_game_events",
      params = list(cmd = 7, min_height = current.height - lookback.height,
        item = commodity.id), num.as.string = TRUE)$result$events

    new.nonces <- sapply(recent.trades, FUN = function(x) {
      if (substr(x[["event"]], 1, 4) == "Sold") {
        x[["nonce"]]
      } else {
        return(NULL)
      }
    })

    new.nonces <- unlist(new.nonces)

    if (length(setdiff(new.nonces, existing.nonces)) == 0 ) { next }

    existing.nonces <- unique(c(new.nonces, existing.nonces))

    recent.trades <- sapply(recent.trades, FUN = function(x) {
      if (substr(x[["event"]], 1, 4) == "Sold") {
        stopifnot(length(x$items) == 1) # Not sure what to do if there are multiple items here
        return(c(cost = x$balance, quantity = x$items[[1]][["amount"]], height = x$height))
      } else {
        return(NULL)
      }
    })

    # do.call(rbind.data.frame, c(recent.trades, list(deparse.level = 1)))

    recent.trades <- do.call(rbind, recent.trades)

    mode(recent.trades) <- "numeric"

    recent.trades <- recent.trades[recent.trades[, "height"] == max(recent.trades[, "height"]), , drop = FALSE]
    # only trades included in most recent block where trades occurred

    midpoint.price <- round(sum(recent.trades[, "cost"]) / sum(recent.trades[, "quantity"]))
    # get the average price, weighted by the quantity sold at each price
    # would be the same as
    # weighted.mean((recent.trades[, "cost"] / recent.trades[, "quantity"]), recent.trades[, "quantity"])

    midpoint.price <- midpoint.price / 1e+08


    bid.price <- sort(midpoint.price - price.increment * seq_len(price.steps))
    ask.price <-      midpoint.price + price.increment * seq_len(price.steps)


    order.bk.df <- TownforgeR::tf_parse_markets(url = url.townforged)
    order.bk.df <- order.bk.df[order.bk.df$account == bot.account.id & order.bk.df$id == commodity.id, , drop = FALSE]

    bid.bk.df <- order.bk.df[   order.bk.df$bid, , drop = FALSE]
    ask.bk.df <- order.bk.df[ ! order.bk.df$bid, , drop = FALSE]

    order.iter <- 1

    while ( nrow(ask.bk.df) > 0 | nrow(bid.bk.df) > 0  |
        order.iter <= length(bid.price) | order.iter <= length(ask.price)
    ) {
      # TODO: clean up this while condition
      # while there are still orders in the original order book
      # while current price step is less than...price.steps


      # TODO: could also have a "safer" mode in which it confirms that a nonce is canceled
      # before initiating a new limit order

      if (nrow(bid.bk.df) > 0) {
        TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_cancel_nonce",
          params = list(nonce = bid.bk.df$nonce[ which.min(bid.bk.df$price) ]))

        bid.bk.df <- bid.bk.df[ (-1) * which.min(bid.bk.df$price), , drop = FALSE]
        print(bid.bk.df)
      }

      if (order.iter <= length(bid.price)) {
        TownforgeR::tf_rpc_curl(url = url.wallet, method ="cc_trade_item",
          params = list(
            bid = TRUE,
            id = commodity.id,
            amount = order.quantity,
            price = formatC(bid.price[order.iter] * 1e+08, format = "fg")))
      }


      if (nrow(ask.bk.df) > 0) {
        TownforgeR::tf_rpc_curl(url = url.wallet, method = "cc_cancel_nonce",
          params = list(nonce = ask.bk.df$nonce[ which.max(ask.bk.df$price) ]))

        ask.bk.df <- ask.bk.df[ (-1) * which.max(ask.bk.df$price), , drop = FALSE]
      }

      if (order.iter <= length(ask.price)) {
        TownforgeR::tf_rpc_curl(url = url.wallet, method ="cc_trade_item",
          params = list(
            bid = FALSE,
            id = commodity.id,
            amount = order.quantity,
            price = formatC(ask.price[order.iter] * 1e+08, format = "fg")))
      }

      # TODO: There are a lot of assumptions about these broadcasted transactions actually being mined

      # TODO: This while loop checks if a new block has been found, but we will
      # encounter problems if network is unstable. maybe best to check if previous TX want through


      initial.height <- new.height <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="get_block_count",
        params = list(), num.as.string = FALSE)$result$count

      while (initial.height == new.height) {

        new.height <- TownforgeR::tf_rpc_curl(url = url.townforged, method ="get_block_count",
          params = list(), num.as.string = FALSE)$result$count

        Sys.sleep(ceiling(loop.sleep.time / 2))
        # TODO: determine the best sleep time. maybe this command is why Im getting
        # "Exception at while refreshing, what=no connection to daemon"

      } # keep checking for a new block. can probably have a small sys.sleep here

      order.iter <- order.iter + 1

    }


  }

}
