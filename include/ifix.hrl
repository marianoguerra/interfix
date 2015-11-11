-define(CB(Clauses), {cb, _, Clauses}).
-define(CB(Line, Clauses), {cb, Line, Clauses}).
-define(CL(Exp), {clause, _, {Exp, _}}).
-define(CL(Exp, Body), {clause, _, {Exp, Body}}).
-define(CL(Line, Exp, Body), {clause, Line, {Exp, Body}}).
-define(C(Names), {call, _, {Names, _}}).
-define(C(Names, Args), {call, _, {Names, Args}}).
-define(C(Line, Names, Args), {call, Line, {Names, Args}}).
-define(Op(Line, Op, Left, Right), {call, Line, {['_', Op, '_'], [Left, Right]}}).
-define(Op(Line, Op, Left), {call, Line, {[Op, '_'], [Left]}}).

% clause call shape
-define(CCS(Shape), {clause, _, {{call, _, {Shape, _}}, _}}).
