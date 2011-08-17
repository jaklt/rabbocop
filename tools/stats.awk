/info: bestscore [^ ]+ [0-9]+/ {
	mcts_count += 1
	mcts_val += $5
	if (num[2] < 100000) {
	}
}

/info: bestscore [^ ]+ depth:([0-9]+)/ {
	split($5, num, ":")
	if (num[2] < 20) {
		ab_count += 1
		ab_depth += num[2]
	}
}

END {
	if (mcts_count != 0)
		print "mcts:" (mcts_val / mcts_count)
	if (ab_count != 0)
		print "ab:" (ab_depth / ab_count)
}
