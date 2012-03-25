geolm <-
function (formula, data = parent.env(), type = "xz", version = 1, 
    plot = TRUE, ...) 
{
    model <- lm(formula, data = data, ...)
    ef <- -1 * effects(model)
    w.int <- which(names(ef) == "(Intercept)")
    w.resid <- which(names(ef) == "")
    w.effect <- (1:length(ef))[-1 * c(w.int, w.resid)]
    s.int <- sum(ef[w.int]^2)
    s.effect <- sum(ef[w.effect]^2)
    s.resid <- sum(ef[w.resid]^2)
    l.int <- sign(ef[w.int]) * sqrt(s.int)
    l.effect <- sqrt(s.effect)
    l.resid <- sqrt(s.resid)
    if (version == 1) {
        transform <- function(x) {
            to2d(x, type = type)
        }
    }
    else {
        transform <- function(x) {
            x
        }
    }
    origin <- transform(c(0, 0, 0))
    y_bar_pt <- transform(c(l.int, 0, 0))
    effect_vec <- transform(c(0, l.effect, 0))
    y_hat_pt <- y_bar_pt + effect_vec
    resid_vec <- transform(c(0, 0, l.resid))
    y_pt <- y_hat_pt + resid_vec
    pts <- rbind(origin, y_bar_pt, origin, y_hat_pt, origin, 
        y_pt, y_bar_pt, y_hat_pt, y_bar_pt, y_pt, y_hat_pt, y_pt)
    if (version == 1) {
        pts <- cbind(pts, rep(0, nrow(pts)))
    }
    d <- data.frame(x = pts[, 1], y = pts[, 2], z = pts[, 3], 
        gp = rep(letters[1:6], each = 2))
    aspect <- c(diff(range(d$y))/diff(range(d$x)), diff(range(d$z))/diff(range(d$x)))
    if (plot) {
        if (version == 1) {
            print(xyplot(y ~ x, data = d, groups = gp, type = "l", 
                lwd = 4, lty = 1, aspect = "iso", col = c("orange", 
                  "blue", "black", "forestgreen", "purple", "red"), 
                scales = list(draw = F), xlab = "", ylab = ""))
        }
        else {
            print(cloud(z ~ x * y, data = d, groups = gp, aspect = aspect, 
                col = c("orange", "blue", "black", "forestgreen", 
                  "purple", "red"), xlab = "mean", ylab = "effect", 
                zlab = "resid", scales = list(draw = F), type = "l"))
        }
    }
    return(model)
}
