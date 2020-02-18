 /* required knowledge base on the battles */
history('Battle of Winwaed',[penda, king_of_mercia, was, slain, killed, oswui, king_of_bernicians, took_place, '15 Novemember 655']).
history('Battle of Stamford Bridge', [tostig_godwinson, herald_hardrada, was, slain, took_place, '25 September 1066']).
history('Battle of Boroughbridge', [edwardII, defeated, earl_of_lancaster, execution, took_place, '16 March 1322']).
history('Battle of Towtown', [edwardIV, defeated, henryVI, took_place, '30 December 1490']).
history('Battle of Wakefield', [richard_of_york, took_place, '30 Decemember 1490', was, slain, war_of_the_roses]).
history('Battle of Adwalton Moor', [earl_of_newcastle, defeats, fairfax, took_place, '30 June 1643', battle, bradford, bloody]).
history('Battle of Marston Moor', [prince_rupert, mariquis_of_newcastle, defeats, fairfax, oliver_cromwell, ironsides, took_place, '2 June 1644', bloody]).

battles(['Battle of Winwaed', 'Battle of Stamford Bridge', 'Battle of Boroughbridge', 'Battle of Towtown', 'Battle of Wakefield', 'Battle of Adwalton Moor', 'Battle of Marston Moor']).

/*Dictionary*/
noun('15 Novemember 655').
noun('25 September 1066').
noun('16 March 1322').
noun('30 Decemember 1490').
noun('30 June 1643').
noun('2 June 1644').
noun(penda).
noun(oswui).
noun(tostig_godwinson).
noun(herald_hardrada).
noun(edwardII).
noun(earl_of_lancaster).
noun(edwardIV).
noun(henryVI).
noun(richard_of_york).
noun(fairfax).
noun(bradford).
noun(prince_rupert).
noun(mariquis_of_newcastle).
noun(oliver_cromwell).
noun(ironsides).
noun(battle).
noun(slain).
noun(earl_of_newcastle).

adj(king_of_bernicians).
adj(king_of_mercia).
adj(war_of_the_roses).
adj(bloody).
adj(traitorous).

synonym(king_of_bernicians, oswui).
synonym(king_of_mercia, penda).
synonym(happened, took_place).
synonym(occurred, took_place).

verb(was).
verb(defeats).
verb(execution).
verb(defeated).
verb(took_place).
verb(killed).

det(the).
det(a).

prep(on).
prep(by).

exit_message([quit]).
exit_message([bye]).
exit_message([exit]).
exit_message([halt]).
exit_message([stop]).
exit_message([e]).
exit_message([q]).

micro_watson :-
	not(clean_target_list),
	reset_answer,
	write('Please aske me a question'),
	nl,
	write('micro_watson:'),
	read(Input),
	assert(targets([])),
	process(Input).

process(Quit) :-
	exit_message(Quit),
	write('Goodbye').

process(Sentence) :-
	filter(Sentence, FilteredSentence),
	sentence(FilteredSentence, Output),
	write(Output), nl,
	battles(Battles),
	find_matches(Answers, Battles),
	find_most_likely_answer(Answers),
	answer(Battle, _),
	write('What is '), write(Battle), nl,
	micro_watson.

/*filter out words that aren't in our vocab*/
filter([], []).
filter([Head|Tail], [Head|Sentence]) :-
	(   det(Head) ; prep(Head) ; verb(Head) ; noun(Head) ; adj(Head) ; synonym(Head, _) ; synonym(_, Head) ),
	filter(Tail, Sentence).
filter([_|Tail], Sentence) :-
	filter(Tail, Sentence).

/*Natural Language Processing Code*/
sentence([], sentence(unknown)).

sentence(Sentence, sentence(noun_phrase(NounPhrase), verb_phrase(VerbPhrase))) :-
	nounphrase(Sentence, NounPhrase, Remainder),
	verbphrase(Remainder, VerbPhrase).

sentence(Sentence, sentence(verb_phrase(VerbPhrase))) :-
	verbphrase(Sentence, VerbPhrase).

nounphrase([Head|Sentence], noun_phrase(det(Head), Output), Remainder) :-
	det(Head),
	nounphrase2(Sentence, Output, Remainder).
nounphrase(Sentence, Output, Remainder) :-
	nounphrase2(Sentence, Output, Remainder).
nounphrase(Sentence, Output, Remainder) :-
	nounphrase(Sentence, Output, Remainder),
	prepositionalphrase(Sentence, Output, Remainder).
nounphrase([], noun(unknown), _).

nounphrase2([Head|Sentence], noun(Head), Sentence) :-
	noun(Head),
	add_to_list(Head).
nounphrase2([Head, NextWord|Sentence], noun_phrase2(adj(Head), Output), Remainder) :-
	adj(Head),
	(   adj(NextWord) ; noun(NextWord) ),
	nounphrase2([NextWord|Sentence], Output, Remainder).
nounphrase2([Head|Sentence], noun(Noun), Sentence) :-
	(   synonym(Head, Noun) ; synonym(Noun, Head) ),
	add_to_list(Noun).

nounphrase2([], noun(unknown), _).

prepositionalphrase([Head|Sentence], prepphrase(prep(Head), Output), Remainder) :-
	prep(Head),
	nounphrase(Sentence, Output, Remainder).
prepositionalphrase([], prepphrase(unknown), _).

verbphrase([Head|Sentence], verb_phrase(verb(Head), Output)) :-
	verb(Head),
	prepositionalphrase(Sentence, Output, _),
	add_to_list(Head).
verbphrase([Head|Sentence], verb_phrase(verb(Head), Output)) :-
	(   synonym(Head, Verb) ; synonym(Verb, Head) ),
	prepositionalphrase(Sentence, Output, _),
	add_to_list(Verb).
verbphrase([Head|Sentence], verb_phrase(verb(Head), Output)) :-
	nounphrase(Sentence, Output, _),
	verb(Head),
	add_to_list(Head).
verbphrase([Head|_], verb(Head)) :-
	verb(Head),
	add_to_list(Head).
verbphrase([Head|Sentence], verb_phrase(verb(Head), Output)) :-
	nounphrase(Sentence, Output, _),
	(   synonym(Head, Verb) ; synonym(Verb, Head) ),
	add_to_list(Verb).
verbphrase([Head|_], verb(Head)) :-
	(   synonym(Head, Verb) ; synonym(Verb, Head) ),
	add_to_list(Verb).

verbphrase([], verb(unknown)).

/*Generate Target List*/
:- dynamic targets/1.

clean_target_list :-
	retract(targets(_)),
	clean_target_list.

add_to_list(Element) :-
	targets(X),
	NewList = [Element|X],
	retract(targets(X)),
	assert(targets(NewList)).
/*cycle through all the History(Name, Blablabla)'s building up a list of answers*/
/*then check for the best, if all equal guess, if none above 50% (<0.5 write(no answer) else write the answer*/

find_matches(_, []).

find_matches([[Battle, Confidence]|ListOfAnswers], [Battle|ListOfBattles]) :-
	find_probable_answers(Battle, Confidence),
	find_matches(ListOfAnswers, ListOfBattles).

find_probable_answers(NameOfBattle, Confidence) :-
	targets(List),
	length(List, Length),
	history(NameOfBattle, HistoryList),
	find_history_matches(List, HistoryList, Matches),
	length(Matches, NumMatches),
	(   Length > 0,
	Confidence is NumMatches/Length ; Confidence is 0).

find_history_matches([Head|Tail], HistoryList, [Head|Matches]) :-
	member(Head, HistoryList),
	find_history_matches(Tail, HistoryList, Matches).

find_history_matches([_|TargetList], HistoryList, Matches) :-
	find_history_matches(TargetList, HistoryList, Matches).

find_history_matches(_, _, []).

member(Item, [Item|_]).
member(Item, [_|List]) :-
	member(Item, List).

:-dynamic answer/2.

answer('None', 0).

find_most_likely_answer([]).

find_most_likely_answer([[Battle, Confidence]|Rest]) :-
	Confidence > 0.5,
	answer(OldBattle, OldConfidence),
	Confidence > OldConfidence,
	retract(answer(OldBattle, OldConfidence)), /*get rid of old answer*/
	assert(answer(Battle, Confidence)),
	find_most_likely_answer(Rest).

find_most_likely_answer([[_, _]|Rest]) :-
	find_most_likely_answer(Rest).

reset_answer :-
	retract(answer(_, _)),
	assert(answer('None', 0)).

/*add weights to history list keywords to get more accurate answers, eg. dates->0.5, battle->0.1, so if a date is found it adds more to the probablity of that being the correct answer. rarer words make the answer more likely whereas common words like battle, took_place, add only a little.*/
