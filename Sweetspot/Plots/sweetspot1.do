*** Sweetspot Graphs
*** Based on Monte Carlo Simulations
*** Reported in JCR Mediation Tutorial 2017-2018

*** Data are in sweetspot1.dta

* First (to be sure) drop graphs from Stata so that new ones can be made
* !! Skip if no graphs available yet.

graph drop lenient
graph drop strict
graph drop combined

********************************************************************************
** Lenient Discriminant Validity
********************************************************************************

* title is in title of graph in text + LOG scale
   line MinimumCorrelation samplesize, lwidth(medium) ///
|| line base50 samplesize, lstyle(p2) lpattern(shortdash) ///
|| line base60 samplesize, lstyle(p3) lpattern(dash) ///
|| line base70 samplesize, lstyle(p4) lpattern(longdash_shortdash) ///
|| line base80 samplesize, lstyle(p5) lpattern(longdash) ///
|| line base90 samplesize, lstyle(p6) ///
||, title("Lenient criterion", size (medium)) name(lenient) /// graph title and name
xsize (5.5) /// x-size of graph in inches
xscale(log range(50 1000) titlegap(2)) /// log scale
xline(68, lcolor(gs10)) /// no earlier line, because no cutpoint for .90
xlabel(50 100 150 200 250 300 400 500 1000, nogrid labsize(small)) legend(colf) /// 
ytitle("Size of observed correlation") ///
ysize (5.5) /// y-size of graph in inches
yscale(range(0 0.85) titlegap(2)) /// 
ylabel(.10(.10).80, nogrid labsize(small)) legend(colf) ///
legend(label(1 "lower limit:") ///
       label(2 "reliability .50") ///
       label(3 ".60") ///
	   label(4 ".70") ///
	   label(5 ".80") ///
	   label(6 ".90") ///   
       order(- "upper limit at:" 2 3 4 5 6 - " " 1 - - - - -) textfirst cols(2) rowgap(0)) /// 
legend(bmargin("0 0 0 0") size(small))	 ///
graphregion(color(white)) bgcolor(white) /// drop the Stata lightblue
text(.38 84 "{&bullet}", place(c))  /// bullet centered
text(.44 84 "a", place(c))  /// label; "a" above bullet point
text(.50 200 "{&bullet}", place(c))  /// bullet centered
text(.56 200 "b", place(c))  // "a" above bullet point

********************************************************************************
** Strict Discriminant Validity
********************************************************************************

* title is in title of graph in text + LOG scale
   line MinimumCorrelation samplesize, lwidth(medium) ///
|| line reliability50 samplesize, lstyle(p2) lpattern(shortdash) ///
|| line reliability60 samplesize, lstyle(p3) lpattern(dash) ///
|| line reliability70 samplesize, lstyle(p4) lpattern(longdash_shortdash) ///
|| line reliability80 samplesize, lstyle(p5) lpattern(longdash) ///
|| line reliability90 samplesize, lstyle(p6) ///
||, title("Strict criterion", size(medium)) name(strict) ///
xsize (5.5) /// x-size of graph in inches
xscale(log range(50 1000) titlegap(2)) ///  log scale
xline(134, lcolor(gs10)) ///
xlabel(50 100 150 200 250 300 400 500 1000, nogrid labsize(small)) legend(colf) ///
ysize (5.5) /// y-size of graph in inches: 5.5
ytitle("Size of observed correlation") ///
yscale(range(0 0.85) titlegap(2)) /// 
ylabel(.10(.10).80, nogrid labsize(small)) legend(colf) ///
legend(label(1 "lower limit:") ///
       label(2 "reliability .50") ///
       label(3 ".60") ///
	   label(4 ".70") ///
	   label(5 ".80") ///
	   label(6 ".90") ///   
       order(- "upper limit at:" 2 3 4 5 6 - " " 1 - - - - -) textfirst cols(2) rowgap(0)) /// 
legend(bmargin("0 0 0 0") size(small))	 ///
graphregion(color(white)) bgcolor(white) /// drop the Stata lightblue
text(.38 84 "{&bullet}", place(c))  /// bullet centered
text(.44 84 "a", place(c))  /// label; "a" above bullet point
text(.50 200 "{&bullet}", place(c))  /// bullet centered
text(.56 200 "b", place(c))  // "a" above bullet point

********************************************************************************
* Display graphs
********************************************************************************
graph display strict 
graph display lenient

* combine the two graphs into a single one
graph combine lenient strict, graphregion(color(white)) ///
      ycommon name(combined, replace) iscale(1)
graph display combined, ysize (5.5) xsize(12) // indicate size of x-axis in inches2

********************************************************************************
* save graphs in different formts
graph export G:\combined1.pdf, replace  // make sure drive/path exists
graph export G:\combined1.wmf, replace
graph export G:\combined1.eps, replace
graph export G:\combined1.tif, width(4000) height(2000) replace  // high quality
********************************************************************************


