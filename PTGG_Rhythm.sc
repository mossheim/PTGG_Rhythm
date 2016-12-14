PTGG_Rhythm : PTGG {
	var <>shortLimit;
	var <>bUseDotted;

	*new {
		arg ruleDict, pExpand, shortLimit, bUseDotted;
		^super.new([\beat, \short, \dotted, \extraDotted], ruleDict, pExpand).init(shortLimit, bUseDotted);
	}

	init {
		arg shortLimit, bUseDotted;
		this.shortLimit = shortLimit;
		this.bUseDotted = bUseDotted;
	}

	// helper methods
	*ratioToFloat {
		|ratio|
		^ratio[0].product/ratio[1].product;
	}

	*isShort {
		|param|
		^PTGG_Rhythm.ratioToFloat(param.ratio) < this.shortLimit
	}

	*isDotted {
		|param|
		^param.ratio[0].last%3 == 0;
	}

	*isExtraDotted {
		|param|
		^param.ratio[0].last.factors.maxItem?1 > 3;
	}

	*isReadyToSplit {
		|param|
		^param.t_wait == 0
	}

	isBelowShortLimit {
		|sentence|
		var ratios = sentence.reject({|term| term.type==PTGG.term_VAR}).collect {
			|term|
			PTGG_Rhythm.ratioToFloat(term.param.ratio)
		};
		^ratios.any(_<this.shortLimit);
	}

	symbolClassificationFunc {
		|param|
		^case
		{~isShort.(param)} {\short}
		{~isDotted.(param) && bUseDotted} {\dotted}
		{~isExtraDotted.(param) && bUseDotted} {\extraDotted}
		{\beat}
	}

	updateParam {
		|param, mode, dur, sum|
		^switch(mode)
		{\split} {
			(\ratio:[param.ratio[0]++[dur], param.ratio[1]++[sum]], \t_wait:param.t_wait)
		}
		{\wait} {
			(\ratio:param.ratio, \t_wait:param.t_wait-1)
		}
		{("PTGG_Rhythm.updateParam: invalid mode:"+mode).error}
	}

	subdivide {
		|term, list|

		var param = term.param, sum = list.sum, sentence;

		^if(PTGG_Rhythm.isReadyToSplit(param)) {
			sentence = list.collect {
				|dur, i|
				var newParam = this.updateParam(param, \split, dur, sum);
				var newSymbol = this.symbolClassificationFunc(newParam);
				PTGG.nt(newSymbol, newParam)
			};
			this.isBelowShortLimit(sentence).if {[term]} {sentence}
		} {
			term.param = this.updateParam(param, \wait);
		}
	}

	subdivideLet {
		|term, list, letList|

		var param = term.param, sum = list.sum, subst, expr;
		var subst_dur, subst_param, subst_symbol;

		// error checking
		letList.do {
			|maybe, i|
			maybe.if {
				subst_dur.isNil.if {
					subst_dur = list[i];
				} {
					if(list[i] != subst_dur) {
						("PTGG_Rhythm.subdividelet: letList implies that the variable is assigned to multiple rhythmic durations:"+list+","+letList).error;
					}
				}
			}
		};

		^if(PTGG_Rhythm.isReadyToSplit(param)) {
			// make substitution term
			subst_param = this.updateParam(param, \split, subst_dur, sum);
			subst_symbol = this.symbolClassificationFunc(subst_param);
			subst = PTGG.nt(subst_symbol, subst_param);
			// make expression
			expr = list.collect {
				|dur, i|
				if(letList[i]) {
					PTGG.vari();
				} {
					var newParam = this.updateParam(param, \split, dur, sum);
					var newSymbol = this.symbolClassificationFunc(newParam);
					PTGG.nt(newSymbol, newParam);
				}
			};

			(this.isBelowShortLimit([subst]++expr)).if {[term]} {[PTGG.let([subst], expr)]};
		} {
			term.param = this.updateParam(param, \wait);
			[term]
		}
	}

	mkSubdivide {
		arg list;
		^{arg term; this.subdivide(term, list)}
	}

	mkSubdivideLet {
		arg list, letList;
		if(letList.isKindOf(Boolean)) {letList = list};
	if(letList[0].isKindOf(Integer)) {letList = letList>0};
		^{arg term; this.subdivideLet(term, list, letList)}
	}

	// more high-level manipulations
	*sentenceToTimes {
	arg sentence, bIntegrate = false;

	var ratioSequence = sentence.collect({
		|term|
		var ratio;
		if(term.type != \nt) {
			"PTGG_Rhythm.sentenceToTimepoints: non-NT found in sentence".error;
		};
		ratio = term.param.ratio;
		[ratio[0].product, ratio[1].product];
	});

	var lcm = ratioSequence.flop[1].reduce(_.lcm(_));

	var normalizedRatioSequence = ratioSequence.collect({
		|ratio|
		ratio*lcm.div(ratio[1]);
	});

	var result = bIntegrate.if {
		normalizedRatioSequence.flop[0].insert(0,0).integrate/lcm;
	} {
		normalizedRatioSequence.flop[0]/lcm;
	};

	^result;
	}

	paramfunc {
		|param, bVerbose = false|
		var ratio = param.ratio;
		var nums = ratio[0], denoms = ratio[1];
		var string = "(";

		bVerbose.if {
			string = string++nums++"/"++denoms++")";
			string.removeEvery(" ");
		} {
			var num = nums.product, denom = denoms.product;
			var gcd = gcd(num, denom);
			string = string++num.div(gcd)++"/"++denom.div(gcd)++")";
		};
		if(param.t_wait > 0) {
			string = string++"["++param.t_wait++"]";
		};
		^string
	}














}
