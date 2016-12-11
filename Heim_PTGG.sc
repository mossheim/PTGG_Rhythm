// port of Donya Quick's PTGG (probabilistic temporal graph grammar) from Haskell

PTGG {
	classvar <term_NT = \nt, <term_LET = \let, <term_VAR = \var;

	var <>ruleDict; // Dictionary[Symbol -> (Prob -> Func)]
	var <>symbolSet; // [Symbol]
	var <>pExpand;

	*new {
		arg symbolSet, ruleDict, pExpand;
		^super.new.init(symbolSet, ruleDict, pExpand);
	}

	init {
		arg symbolSet, ruleDict, pExpand;
		this.symbolSet = symbolSet;
		this.ruleDict = this.normalizeRules(ruleDict);
		this.pExpand = pExpand;
	}

	normalizeRules {
		arg ruleDict;
		var result = Dictionary[];
		ruleDict.pairsDo {
			|symbol, probFuncAssocs|
			var sum = probFuncAssocs.collect(_.key).sum;
			probFuncAssocs.do({
				|assoc|
				assoc.key = assoc.key / sum;
			});
			result = result.add(symbol -> probFuncAssocs);
		};
		^result;
	}

	update {
		arg sentence;

		var result = [];

		if(sentence.isKindOf(Dictionary)) {
			if(sentence.collect(_.isKindOf(Event)).reduce('&&').not) {
				"PTGG.update: non-Event found in sentence input".error;
			} {
				"PTGG.update: non-Dictionary input".error
			}
		};

		sentence.do {
			|term, i|

			var subsentence = term.type.switch
			{term_NT} {this.applyRules(term)}
			{term_LET} {pExpand.coin.if {
				this.expand(term);
			} {
				[PTGG.let(this.update(term.val), this.update(term.expr))]
			}
			}
			{term_VAR} {[term]}
			{"PTGG.update: term found with impossible type".error};
			if(subsentence.isKindOf(Collection).not) {
				("PTGG.update: non-collection subsentence returned from term" + term.asString + "|" + subsentence.asString).error;
			};
			result = result ++ subsentence;
		};

		^result;
	}

	applyRules {
		arg term;

		var symbol = term.symbol;
		var rules, ruleFunc;

		if(symbolSet.includes(symbol).not) {
			("PTGG.applyRules: not in symbol set ("++symbol++"). term:"+term.asString).error;
		};

		rules = ruleDict[symbol];
		if(rules.isNil) {
			("PTGG.applyRules: symbol not in ruleDict:"+symbol.asString).error;
		};
		ruleFunc = rules.collect(_.value).wchoose(rules.collect(_.key));

		^ruleFunc.value(term);
	}

	expand {
		arg term;
		// term.postln;
		^if(term.type == term_LET) {
			var val = term.val;
			var expr = term.expr;
			var result = [];

			if(val.isNil || expr.isNil) {
				("PTGG.expand: term" + term.asString + "has a nil parameter").error;
			};
			expr.do({
				|term|
				result = result ++(term.type==term_VAR).if {
					val
				} {
					[term]
				}
			});

			result;
		} {
			[term];
		};
	}

	expandAll {
		arg sentence;
		while {sentence.any({|term| term.type==term_LET})} {
			// "loop".postln;
			sentence = sentence.collect(this.expand(_)).reduce('++');
		};
		^sentence;
	}

	// convenience
	*nt {
		arg symbol, param = ();
		^(type:\nt, symbol:symbol, param:param);
	}

	*vari {
		^(type:\var);
	}

	*let {
		arg val, expr;
		^(type:\let, val:val, expr:expr);
	}

	*sentenceToString {
		arg sentence, paramfunc = _.asString;
		^sentence.collect(PTGG.termToString(_,paramfunc));
	}

	*termToString {
		arg term,paramfunc = _.asString;
		^switch(term.type)
		{term_NT} {term.symbol.asString++paramfunc.value(term.param)}
		{term_LET} {"LET "++PTGG.sentenceToString(term.val,paramfunc)++" IN "++PTGG.sentenceToString(term.expr,paramfunc)}
		{term_VAR} {"var"};
	}
}