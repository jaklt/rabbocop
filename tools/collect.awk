# Parse multiple aei outputs and sum results

BEGIN {
    rounds = -1
    parse  = -1
    roundCount = 0
}

/^Number of rounds: / {
    rounds      = $4
    roundCount += $4
}

/^After round / {
    if (rounds == $3) {
        parse = 0
    }
}

parse != -1 && /^[a-zA-Z0-9]+ has [0-9]+ wins and [0-9]+ timeouts/ {
    if (parse == 0) parse = 1
    else if (parse == 1) parse = 2

    name[parse,"wins"]     += $3
    name[parse,"timeouts"] += $6
    name[parse,"name"]      = $1
}

parse != -1 && /^ *[0-9]+ by [a-z]/ {
    name[parse,"by",$3] += $1
}

FNR==1 || EOF { parse = -1; rounds = -1 }

END {
    split("abcdefghijklmnopqrstuvwxyz", A, "")

    for (pl=1; pl<=2; pl++) {
        print name[pl,"name"] " has " name[pl,"wins"] " wins and " name[pl,"timeouts"] " timeouts"

        for (i=1; i<=26; i++) {
            if (name[pl,"by",A[i]] != 0)
                print "   " name[pl,"by",A[i]] " by " A[i]
        }
    }
}
