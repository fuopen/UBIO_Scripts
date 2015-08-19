BEGIN {
    
    ns = tolower(start)
    ne = tolower(end)
    split(tolower(versus), uv, ",")
    for (i in uv) nv[uv[i]] = i
}

NR == 1 {

    for (f = 1; f <= NF; f++) {
	if (tolower($f) == ns) s = f
	if (tolower($f) == ne) e = f
	if (tolower($f) in nv)
	    v[f] = tolower($f)
    }

    print $0""FS"StartVisitPosition"FS"EndVisitPosition"FS"VisitOrder"
}

NR != 1 {
    
    wl[ns] = parse($s)
    wl[ne] = parse($e)
    for (l in v)
	wl[v[l]] = parse($l)

    p = 0
    seq = ""
    delete c
    PROCINFO["sorted_in"] = "sort"
    for (z in wl) {

	if (wl[z] != 0) {
	    
	    if (p != wl[z] && length(seq) > 0)
		seq = seq " | "
	    if (p == wl[z]) 
		seq = seq "&"
	    
	    p = wl[z]
	    seq = seq camel(z)

	    if (wl[ns] != 0) {
		if (wl[z] > wl[ns] && (!c["bs"] || wl[c["bs"]] > wl[z]))
		    c["bs"] = z
		if (wl[z] <= wl[ns] && z != ns && (!c["as"] || wl[c["as"]] <= wl[z]))
		    c["as"] = z
	    }
	    if (wl[ne] != 0) {
		if (wl[z] > wl[ne] && (!c["be"] || wl[c["be"]] > wl[z]))
		    c["be"] = z
		if (wl[z] <= wl[ne] && z != ne && (!c["ae"] || wl[c["ae"]] <= wl[z]))
		    c["ae"] = z
	    }

	}
    }

    vs = ve = ""
    if (c["as"])
	vs = "Start_at_or_after_"camel(c["as"])
    if (!vs && c["bs"])
	vs = "Start_before_"camel(c["bs"])
    if (c["ae"])
	ve = "End_at_or_after_"camel(c["ae"])
    if (!ve && c["be"])
	ve = "End_before_"camel(c["be"])
	
    print $0""FS""vs""FS""ve""FS""seq

}

function parse(p) {

    match(tolower(p), /([0-9na]+)[-/]*([0-9na]+)[-/]*([0-9na]+)[-/]*/, ap)

    for (r in ap) {
	if (ap[r] == "na")
	    ap[r] = "01"
    }

    dp = 0
    if (length(ap[2]) != 2)
	dp
    else if (length(ap[1]) == 2 && length(ap[3]) == 4)
	dp = mktime(ap[3]" "ap[2]" "ap[1]" 01 00 00")
    else if (length(ap[1]) == 4 && length(ap[3]) == 2)
	dp = mktime(ap[1]" "ap[2]" "ap[3]" 01 00 00")

    return dp
}

function sort(i1, v1, i2, v2) {
    
    v1 = v1 + 0
    v2 = v2 + 0
    if (v1 < v2)
	return -1
    
    return (v1 != v2)	
}

function camel(s) {

    t = toupper(substr(s,1,1)) tolower(substr(s,2))
    gsub("date", "", t)
    return t
}
