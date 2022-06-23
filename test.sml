use "task.sml"
;

fun test(function_name : string, true_result, fact_result) =
    if true_result = fact_result
    then (function_name, "Ok")
    else (function_name, "Failed");
;

test("only_capitals", ["Bx"], only_capitals(["abc", "aXa", "Bx"]));
test("only_capitals", [], only_capitals(["abc", "aXa", "kx"]));

test("longest_string1", "", longest_string1([]));
test("longest_string1", "abcde", longest_string1(["abcde", "1234", "kx"]));
test("longest_string1", "abcde", longest_string1(["abcde", "12345", "kx"]));

test("longest_string2", "", longest_string2([]));
test("longest_string2", "abcde", longest_string2(["abcde", "1234", "kx"]));
test("longest_string2", "12345", longest_string2(["abcde", "12345", "kx"]));

test("longest_string3", "", longest_string3([]));
test("longest_string3", "abcde", longest_string3(["abcde", "1234", "kx"]));
test("longest_string3", "abcde", longest_string3(["abcde", "12345", "kx"]));


test("longest_string4", "", longest_string4([]));
test("longest_string4", "abcde", longest_string4(["abcde", "1234", "kx"]));
test("longest_string4", "12345", longest_string4(["abcde", "12345", "kx"]));

test("longest_capitalized", "", longest_capitalized([]));
test("longest_capitalized", "Abcde", longest_capitalized(["Abcde", "1234", "kx"]));
test("longest_capitalized", "", longest_capitalized(["abcde", "12345", "kx"]));

test("rev_string", "", rev_string(""));
test("rev_string", "edcbA", rev_string("Abcde"));

test("first_answer", 5, first_answer (fn(x) => if x = 5 then SOME 5 else NONE)  [1,2,3,4,5]);
test("first_answer", 0, first_answer (fn(x) => if x = 6 then SOME 6 else NONE)  [1,2,3,4,5]);

test("all_answers", SOME [], all_answers (fn(x) => if x = 5 then SOME [5] else NONE)  []);
test("all_answers", NONE, all_answers (fn(x) => if x = 5 then SOME [5] else NONE)  [1,2,3,4,5]);
test("all_answers", SOME [5], all_answers (fn(x) => if x = 5 then SOME [5] else NONE)  [5]);
test("all_answers", SOME [5,5], all_answers (fn(x) => if x = 5 then SOME [5] else NONE)  [5,5]);

test("count_wildcards", 1 ,count_wildcards Wildcard);
test("count_wildcards", 1,count_wildcards (TupleP ([Wildcard])));
test("count_wildcards", 2,count_wildcards (TupleP ([Wildcard,Wildcard])));
test("count_wildcards", 1,count_wildcards (TupleP ([Wildcard,ConstP(1)])));
test("count_wildcards", 3,count_wildcards (TupleP ([Wildcard,Wildcard,Wildcard,ConstP(1)])));
test("count_wildcards", 0,count_wildcards (TupleP ([ConstP(1),ConstP(1)])));

test("count_wild_and_variable_lengths", 3,count_wild_and_variable_lengths (TupleP ([Wildcard,Wildcard,Wildcard])));
test("count_wild_and_variable_lengths", 4,count_wild_and_variable_lengths (TupleP ([Wildcard,Wildcard,Variable("ab")])));
test("count_wild_and_variable_lengths", 2,count_wild_and_variable_lengths (TupleP ([Variable("ab")])));
test("count_wild_and_variable_lengths", 9,count_wild_and_variable_lengths (TupleP ([Variable("ab"),Variable"abcde",Wildcard,Wildcard])));

test("count_some_var", 1,count_some_var("ab", (TupleP ([Variable("ab")]))));
test("count_some_var", 2,count_some_var("ab", (TupleP ([Variable("ab"),Variable("bc"),Variable("ab")]))));
test("count_some_var", 0,count_some_var("ab2", (TupleP ([Variable("ab"),Variable("bc"),Variable("ab")]))));
test("count_some_var", 0,count_some_var("wild",ConstructorP ("wild",(Wildcard))));
test("count_some_var", 2,count_some_var ("x",TupleP[TupleP[TupleP[Variable "x",ConstructorP("wild",Wildcard)],Wildcard],Variable "x"]));

test("check_pat", true, check_pat((TupleP ([Variable("")]))));
test("check_pat", true, check_pat((TupleP ([Variable("ab")]))));
test("check_pat", false, check_pat((TupleP ([Variable("ab"),Variable("ab"),Variable("bc")]))));
test("check_pat", true, check_pat( (TupleP ([Variable("ab1"),Variable("ab2"),Variable("ab3")]))));

test("match", NONE, match( Const 1,TupleP ([Variable("")])));
test("match", SOME [], match( Const 1,ConstP 1));
test("match", SOME[], match( Tuple[ (Const 1), Unit,Constructor("asd", Const 1), Constructor("dsa",Const 2)], 
							TupleP ([ConstP 1,UnitP,ConstructorP ("asd", ConstP 1),ConstructorP("dsa",ConstP 2) ])));
test("match", SOME [("dsa3",Constructor("dsa",Const 2)),("dsa2",Constructor("asd", Const 1)),
								           ("dsa1",Const 1)], match( Tuple[ (Const 1), Unit,Constructor("asd", Const 1), Constructor("dsa",Const 2)], 
							TupleP ([Variable("dsa1"), UnitP , Variable("dsa2") ,Variable("dsa3")])));

test("first_match", NONE, first_match (Constructor("dsa",Const 1)) ([UnitP,ConstP 1]));
test("first_match", SOME [("abc",Const 1)], first_match (Const 1) ([UnitP,Variable("abc")]));
test("first_match", SOME [("abc",Const 1)], first_match (Const 1) ([Variable("abc"),Variable("abc1")]));