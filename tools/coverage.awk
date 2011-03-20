
# hpc show Main > jaj
# awk -f coverage.awk jaj $module

BEGIN {
	hpc    = 2;
	module = "MCTS"; # module name
	l      = 1;      # number of line after module definition
}

index($0, module) {
	if (hpc == 1) {
		split($4, num, ":");      # from line
		split(num[2], num2, "-"); # to line

		for (i = num[1]; i<=num2[2]; i++)
			count[i]++;
	}
}

# Start/end of file
FNR==1 || EOF { hpc--; }

{
	# On second file match hpc information to lines
	if (hpc == 0) {
		print "\t" l "\t" count[l] "\t" $0
		l++;
	}
}
