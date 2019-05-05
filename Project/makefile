%.lex.sml :  %.lex  
			mllex $<

%.grm.sml : %.grm
			mlyacc $<

all       : source
clean     : 
			rm -rf *.lex.sml *.grm.sml *.grm.sig *.grm.desc source

source    : types.sml symbol.sml printColor.sml typechecking.sml semantic.sml code_gen.sml c.grm c.lex c.grm.sml c.lex.sml parsetest2.sml absyn.sml run.sml stack.sml source.mlb
		mlton source.mlb

test : all 
	 ${CURDIR}/source
