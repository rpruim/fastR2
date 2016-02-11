xyplot(births~dayofyear, groups = dayofyear %% 7, data = Births78,
			auto.key = list(columns = 3))
bwplot(births~factor(dayofyear %% 7), groups = dayofyear %% 7, Births78)


