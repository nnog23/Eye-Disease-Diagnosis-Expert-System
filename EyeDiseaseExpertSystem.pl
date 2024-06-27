% Expert system shell based on Luger
% To run, solve(diagnose(X), CF)
% solve(+,?)
:- dynamic known/2.
solve(Goal,CF) :-
print_instructions,
retractall(known(_,_)), 
(solve(Goal,CF,[],20)  -> true ; write('Unable to provide a diagnosis with the given information. Please consult a medical professional at the nearest hospital.'), nl, fail).

print_instructions :-
nl, write('You will be asked a series of queries.'), nl,
write('Your response must be either:'),
nl, write('a. A number between -100 and 100 representing'), nl,
write(' your confidence in the truth of the query'), nl,
write('b. why'),
nl, write('c. how(X), where X is a goal'),nl.

% solve(+,?,+,+)
solve(Goal,CF,_,Threshold):-
known(Goal,CF),!,
above_threshold(CF,Threshold).

solve(not(Goal),CF,Rules,Threshold) :- !,
invert_threshold(Threshold,New_threshold),
solve(Goal,CF_goal,Rules,New_threshold),
negate_cf(CF_goal,CF).

solve((Goal1,Goal2),CF,Rules,Threshold) :- !,
solve(Goal1,CF1,Rules,Threshold),
above_threshold(CF1,Threshold),
solve(Goal2,CF2,Rules,Threshold),
above_threshold(CF2,Threshold),
and_cf(CF1,CF2,CF).

solve(Goal,CF,Rules,Threshold) :-
rule((Goal:-(Premise)),CF_rule),
solve(Premise,CF_premise,[rule((Goal:-Premise),CF_rule)|Rules],Threshold),
rule_cf(CF_rule,CF_premise,CF),
above_threshold(CF,Threshold).

solve(Goal,CF,_,Threshold) :-
rule(Goal,CF),
above_threshold(CF,Threshold).

solve(Goal,CF,Rules,Threshold):-
askable(Goal),
askuser(Goal,CF,Rules),!,
assert(known(Goal,CF)),
above_threshold(CF,Threshold).

above_threshold(CF,T) :- T>=0, CF>=T.
above_threshold(CF,T) :- T<0, CF=<T.
invert_threshold(Threshold,New_threshold):-
    New_threshold is -1 * Threshold.

negate_cf(CF,Negated_CF):-
	Negated_CF is -1 * CF.

and_cf(A,B,A) :- A =< B.
and_cf(A,B,B) :- B < A.

rule_cf(CF_rule,CF_premise,CF):-
	CF is (CF_rule * CF_premise / 100).

% askuser(+,?,+)
askuser(Goal,CF,Rules):-
    nl,write('Query : '),
    write(Goal), write(' ? '),
    read(Ans),
    respond(Ans,Goal,CF,Rules).

% respond(+,+,?,+)
respond(CF,_,CF,_) :-
	number(CF), CF=<100, CF>= -100. % no response issued because user enters a valid CF

respond(why,Goal,CF,[Rule|Rules]) :-
    nl, write_rule(Rule),
    askuser(Goal,CF,Rules).

respond(why,Goal,CF,[]) :-
    nl, write('Back to top of rule stack.'),
    nl, askuser(Goal,CF,[]).

respond(how(X),Goal,CF,Rules):-
    build_proof(X,CF_X,Proof), !,
    nl, write('The goal '), write(X),
    write(' was concluded with certainty '),
    write(CF_X), write('.'), nl,
    nl, write('The proof of this is:'), nl,
    write_proof(Proof,0),nl,
    askuser(Goal,CF,Rules).

respond(how(X),Goal,CF,Rules) :-
    write('The truth of '), write(X), nl,
    write('is not yet known.'), nl,
    askuser(Goal,CF,Rules).

respond(_,Goal,CF,Rules):-
    write_rule('Unrecognized response.'), nl,
    askuser(Goal,CF,Rules).

% build_proof(+,?,?)
build_proof(Goal,CF,(Goal,CF:-given)):-
	known(Goal,CF),!.

build_proof(\+ Goal,CF,  Proof) :- !,
	build_proof(Goal,CF_goal,Proof),
    negate_cf(CF_goal,CF).

build_proof((Goal1,Goal2),CF,(Proof1,Proof2)):-
    build_proof(Goal1,CF1,Proof1),
    build_proof(Goal2,CF2,Proof2),
    and_cf(CF1,CF2,CF).

build_proof(Goal,CF,(Goal,CF:-Proof)):-
    rule((Goal:-Premise),CF_rule),
    build_proof(Premise,CF_premise,Proof),
    rule_cf(CF_rule,CF_premise,CF).

build_proof(Goal,CF,(Goal,CF:-fact)):-
    rule(Goal,CF).

write_rule(rule((Goal:-(Premise)),CF)) :-
	write('I am trying to prove the following rule:'), nl, write(Goal),
    write(':-'), nl,
    write_premise(Premise),
    write('CF = '), write(CF), nl.

write_rule(rule(Goal,CF)) :-
    write('I am trying to prove the following goal:'), nl, write(Goal),
    write('CF = '), write(CF), nl.

write_premise((Premise1,Premise2)) :- !,
	write_premise(Premise1),
    write_premise(Premise2).

write_premise(\+ Premise) :- !,
    write(' '), write(not), write(' '), write(Premise), nl.

write_premise(Premise) :- !,
    write(' '), write(Premise), nl.

% write_proof(+,+)
write_proof((Goal,CF:-given),Level):-
    indent(Level), write(Goal),
    write(' CF='), write(CF),
    write(' was given by the user'), nl, !.

write_proof((Goal,CF:-fact),Level) :-
    indent(Level), write(Goal),
    write(' CF='), write(CF),
    write(' was a fact in the KB'), nl, !.

write_proof((Goal,CF:-Proof),Level) :-
    indent(Level), write(Goal),
    write(' CF='), write(CF), write(':-'), nl,
    New_level is Level + 1,
    write_proof(Proof,New_level),!.

write_proof(\+ Proof,Level) :-
    indent(Level), write((not)), nl,
    New_level is Level + 1,
    write_proof(Proof,New_level),!.

write_proof((Proof1,Proof2),Level):-
	write_proof(Proof1,Level),
    write_proof(Proof2,Level), !.

indent(0).
indent(X):-
write(' '), X_new is X - 1, indent(X_new).
rule((diagnose(Advice):-
    (disease(X),diagnose(X,Advice))),100).


% diagnosis
rule((disease(acute_angle_closure_glaucoma):-
	(symptom(has_eye_pain),symptom(has_eye_redness),
         symptom(has_blurry_vision), symptom(has_headache),
         symptom(is_vomiting), symptom(sees_halos_around_lights))),50).

rule((disease(corneal_ulcer):-
	(symptom(has_eye_pain),symptom(has_eye_redness),
         symptom(has_blurry_vision),symptom(has_difficulty_opening_the_eyes),
         symptom(has_eye_discharge), history(eye_trauma), history(contact_lens_use))), 80).

rule((disease(uveitis):-
	(symptom(has_eye_redness),symptom(has_blurry_vision),
        symptom(has_difficulty_opening_the_eyes),symptom(has_photophobia))),80).

rule((disease(corneal_abrasion):-
	(symptom(has_eye_pain),symptom(has_eye_redness),
         symptom(has_blurry_vision),symptom(has_difficulty_opening_the_eyes), history(eye_trauma))),80).

rule((disease(scleritis):-
	(symptom(has_eye_pain),symptom(has_eye_redness),
         symptom(has_eye_irritation))),75).

rule((disease(dry_eye_syndrome):-
	(symptom(has_eye_redness),symptom(has_blurring_of_vision),
         symptom(has_foreign_body_sensation))),80).

rule((disease(conjunctivitis):-
	(symptom(has_eye_redness),symptom(has_eye_discharge),
         symptom(has_eye_itchiness),symptom(is_tearing),
         symptom(has_eye_swelling))),80).

rule((disease(blepharitis):-
	(symptom(has_eye_redness),symptom(has_eye_discharge),
         symptom(has_eye_itchiness),symptom(has_eye_irritation))),80).

rule((disease(entropion):-
	(symptom(has_eye_redness), symptom(is_tearing),
         symptom(has_foreign_body_sensation))),80).

rule((disease(diabetic_retinopathy):-
	(symptom(has_blurry_vision),
        disease(floaters))),80).

rule((disease(cataract):-
	(symptom(has_gradual_blurring_of_vision),symptom(has_glare))),80).

rule((disease(armd):-
	(disease(floater),symptom(has_eye_redness),
         symptom(has_eye_irritation))),75).

rule((disease(strabismus):-
	(symptom(has_double_vision))),90).

rule((disease(floaters):-
	(symptom(seeing_dark_spots_or_squiggly_lines))),75).

% test
rule((symptom(has_eye_pain):-(ask(has_eye_pain))), 50).
rule((symptom(has_eye_redness):-(check(is_eye_color_red))), 60).
rule((symptom(has_blurry_vision):-(result(eye_vision_test_blurry))), 80).
rule((symptom(sees_halos_around_lights):-(result(halos_around_lights_exam))), 80).
rule((symptom(sees_halos_around_lights):-(ask(has_halos_around_lights))), 50).
rule((symptom(has_headache):-(ask(has_headache))), 50).
rule((symptom(is_vomiting):-(ask(has_vomited_recently))), 50).
rule((symptom(has_gradual_blurring_of_vision):-(symptom(has_blurry_vision), check(has_gradual_blurring_of_vision))), 60).
rule((symptom(has_glare):-(ask(has_glare))), 50).
rule((symptom(has_tearing):-(result(tearing_test))), 80).
rule((symptom(has_eye_discharge):-(result(eye_discharge_test))), 80).
rule((symptom(has_eye_itchiness):-(ask(has_eye_itchiness))), 50).
rule((symptom(has_eye_swelling):-(check(has_eye_swelling))), 60).
rule((symptom(has_foreign_body_sensation):-(ask(has_feels_like_something_in_eye))), 50).
rule((symptom(has_difficulty_opening_the_eyes):-(result(difficulty_opening_eyes_test))), 80).
rule((symptom(seeing_dark_spots_or_squiggly_lines):-(result(floaters_test))), 80).
rule((symptom(has_double_vision):-(result(double_vision_test))), 80).
rule((symptom(has_photophobia):-(ask(has_eye_pain_when_exposed_to_light))), 50).


% result
rule(diagnose(acute_angle_closure_glaucoma,'You might be experiencing Acute Angle-Closure Glaucoma, please go immediately to the emergency room.'),100).
rule(diagnose(corneal_ulcer, 'You might be experiencing Cornea Ulcer, please go immediately to the emergency room.'), 100).
rule(diagnose(uveitis, 'You might be experiencing Uveitis, please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(corneal_abrasion, 'You might be experiencing Corneal Abrasion, please go immediately to the emergency room.'), 100).
rule(diagnose(scleritis, 'You might be experiencing Scleritis, please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(dry_eye_syndrome, 'You might be experiencing Dry Eye Syndrome. You may prescribed with lid hygiene and eye drops, eye gel or ointments. Please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(conjunctivitis, 'You might be experiencing Conjunctivitis. You may apply cold compresses to alleviate symptoms and eye drops may be prescribed. Please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(blepharitis, 'You might be experiencing Blepharitis. You can consider eyelid hygiene practices, such as applying warm moist compresses, doing lid massages, and lid scrubs. Please consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(entropion, 'You might be experiencing Entropion. Eye drops and eye ointments may be prescribed, please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(diabetic_retinopathy, 'You might be experiencing Diabetic Retinopathy. Please have a regular check-up, especially if diagnosed with diabetes.'), 100).
rule(diagnose(cataract,'You might have cataract, consult an eye doctor for definite diagnosis and management. Cataract surgery is recommended when the cataract causes difficulty in the patient’s daily activities.'),100).
rule(diagnose(amblyopia, 'You might be experiencing Amblyopia, please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(armd, 'You might be experiencing Age-related macular degeneration, please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).
rule(diagnose(strabismus, 'You might be experiencing Strabismus, please consult an eye doctor for examination and management.'), 100).
rule(diagnose(floaters, 'You might be experiencing Floaters. No treatment may be needed but it may be a sign of a more serious condition. Please go consult with an eye doctor for further evaluation and appropriate medical guidance.'), 100).

askable(result(_)).
askable(check(_)).
askable(ask(_)).
askable(history(_)).