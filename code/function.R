##########################################################
###                     function.R                     ###
##########################################################


### --------------  test  --------------- ###
chisq.table <- function(df, var, obs, drop.var = c(), drop.obs = c(), ratio = TRUE) {
    tab <- table(unlist(df[var]), unlist(df[obs]))
    tab <- tab[!(row.names(tab) %in% drop.var),!(colnames(tab) %in% drop.obs)]
    df <- add.total(tab, ratio)
    # calculate p value
    df$p.value <- NA
    gs <- row.names(tab)
    for (g in gs) {
        df[g, "p.value"] <- chisq.sub(tab, g1 = gs[1], g2 = g)
    }
    # reformat
    df$p.value[1:length(gs)] <- formatC(df$p.value[1:length(gs)], format = "e", digits = 3)
    return(df)
}

chisq.table.level <- function(df, var, obs, level, drop.var = c(), drop.obs = c()) {
    # all
    tab <- chisq.table(df, var = var, obs = obs, drop.var = drop.var, drop.obs = drop.obs)
    tab$p.adj <- tab$or.adj <- NA
    tab[row(tab)[1:(nrow(tab)-1)], c("or.adj", "p.adj")] <- test.mantelhaen(df, var, level, obs, drop.var, drop.obs)
    res <- list()
    res[["All"]] <- tab
    # separate levels
    for (l in levels(df[,level])) {
        res[[l]] <- chisq.table(df[df[,level] == l,], var = var, obs = obs, drop.var = drop.var, drop.obs = drop.obs)
    }
    return(res)
}

test.mantelhaen <- function(df, var, level, obs, drop.var = c(), drop.obs = c()) {
    tab <- table(unlist(df[var]), unlist(df[obs]), unlist(df[level]))
    tab <- tab[!(dimnames(tab)[[1]] %in% drop.var),,
               !(dimnames(tab)[[2]] %in% drop.obs)]
    gs <- dimnames(tab)[[1]]
    or <- p <- c()
    for (g in gs) {
        tab.tmp <- tab[c(gs[1], g),,]
        fit <- mantelhaen.test(tab.tmp)
        or <- c(or, fit$estimate)
        p <- c(p, fit$p.value)
    }
    names(or) <- names(p) <- gs
    or <- round(or, 3)
    p <- formatC(p, format = "e", digits = 3)
    res <- data.frame(or.adj = or, p.adj = p, stringsAsFactors = FALSE)
    return(res)
}


add.total <- function(tab, ratio) {
    df <- as.data.frame.matrix(tab)
    df$TOTAL <- rowSums(tab)
    df <- rbind(df, TOTAL = colSums(df))
    if (ratio) {
        df$Rate <- df[,2] / df[,3]
        df$Odds.Ratio <- df$Rate / df$Rate[1]
        df$Rate <- round(df$Rate, 3)
        df$Odds.Ratio <- round(df$Odds.Ratio, 3)
        df["TOTAL", "Odds.Ratio"] <- NA
    }
    return(df)
}

chisq.sub <- function(tab, g1, g2) {
    tab <- tab[c(g1, g2),]
    fit <- chisq.test(tab)
    return(fit$p.value)
}

merge.table <- function(tabs) {
    for (n in names(tabs)) {
        if (n != "All") {tabs[[n]] <- cbind(tabs[[n]], " " = NA, " " = NA)}
    }
    tabs <- lapply(names(tabs), FUN = function(x) insert.row.col.names(tabs[[x]], name = x))
    tab <- do.call(rbind, tabs)
    return(tab)
}

insert.row.col.names <- function(tab, name) {
    tab <- cbind(row.names(tab), tab, stringsAsFactors = FALSE)
    colnames(tab)[1] <- name
    tab <- rbind(colnames(tab), tab, stringsAsFactors = FALSE)
    colnames(tab) <- NA
    row.names(tab) <- 1:nrow(tab)
    return(tab)
}

### --------------   writing   --------------- ###
write.excel <- function(tabs, sheet, path) {
    tab <- merge.table(tabs)
    write.xlsx(tab, file = path, sheetName = sheet, showNA = FALSE, append = TRUE, col.names = FALSE, row.names = FALSE)
}

write.reg <- function(fit, path) {
    s <- summary(fit)$coefficients
    s[,1:3] <- apply(s[,1:3], 2, FUN = function(x) round(x, 3))
    write.csv(s, file = path)
}

### --------------  plotting  --------------- ###
plot.bar <- function(df, var, obs, drop.var = c(), drop.obs = c(), norm = "none", main = NULL) {
    tab <- table(unlist(df[var]), unlist(df[obs]))
    tab <- tab[!(row.names(tab) %in% drop.var),!(colnames(tab) %in% drop.obs)]
    if (is.character(norm)) {
        if (norm == "total") {
            for (i in 1:ncol(tab)) {tab[,i] <- tab[,i] / sum(tab[,i])}
        } else if (norm != "none") {stop("only none or total are accepted.")}
    } else if (length(norm) == ncol(tab)) {
        for (i in 1:ncol(tab)) {tab[,i] <- tab[,i] / norm[i]}
    }
    barplot(as.matrix(tab), beside = T, main = main, col = c("red", "yellow", "green", "blue"))
}

plot.bar.ratio <- function(df1, df2, var, obs, main = NULL, drop.var = c(), drop.obs = c()) {
    denom <- table(unlist(df1[var]), unlist(df1[obs]))
    numer <- table(unlist(df2[var]), unlist(df2[obs]))
    denom <- denom[!(row.names(denom) %in% drop.var),!(colnames(denom) %in% drop.obs)]
    numer <- numer[!(row.names(numer) %in% drop.var),!(colnames(numer) %in% drop.obs)]
    tab <- numer / denom
    barplot(as.matrix(tab), beside = T, main = main, col = c("red", "yellow", "green", "blue"))
}

plot.pie <- function(df, var, obs, drop.var = c(), drop.obs = c()) {
    tab <- table(unlist(df[var]), unlist(df[obs]))
    tab <- tab[!(row.names(tab) %in% drop.var),!(colnames(tab) %in% drop.obs)]
    print(tab)
    n <- nrow(tab)
    par.old <- par()
    par(mfrow = c(1, n))
    for (i in 1:n) {
        pie(tab[i,], labels = colnames(tab), main = row.names(tab)[i])
    }
    par(par.old)
}

plot.density <- function(df, var, obs, main = NULL, cut = NULL) {
    data <- df[!is.na(df[,obs]),]
    if (!is.null(cut)) {
        data <- data[(data[,obs] > cut[1]) & (data[,obs] < cut[2]),]
    }
    gs <- levels(df[,var])
    ds <- list()
    for (g in gs) {
        ds[[g]] <- density(data[data[,var] == g,obs], adjust = 1.6)
    }
    d.x <- do.call(c, lapply(ds, FUN = function(x) x$x))    
    d.y <- do.call(c, lapply(ds, FUN = function(x) x$y))
    xlim <- c(min(d.x), max(d.x))
    ylim <- c(min(d.y), max(d.y))
    if (length(gs) == 4) {
        col <- c("red", "yellow", "green", "blue")
    } else {
        stop("Number of color not match.")
    }
    # plot
    plot(ds[[1]]$x, ds[[1]]$y, type = "l", col = col[1], xlim = xlim, ylim = ylim, 
         lwd = 2, main = main, xlab = "", ylab = "")
    for (i in 2:length(ds)) {
        lines(ds[[i]]$x, ds[[i]]$y, type = "l", col = col[i], lwd = 2, xlim = xlim)
    }
}

