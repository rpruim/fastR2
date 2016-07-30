xyplot(pallets ~ day, data = Pallets,
			groups = employee,
			type='b', auto.key = list(columns = 2, lines = TRUE))

