xyplot( waiting ~ eruptions, data = faithful )
xyplot( head(waiting, -1) ~ tail(eruptions, -1), data = faithful )

