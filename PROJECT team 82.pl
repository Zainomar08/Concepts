
assign_quiz(_,[],[]).

assign_quiz(quiz(Name,Day,Slot,NumTAs),[day(Dayname,H)|TR],AssignedTAs):-
    Day \= Dayname,
	assign_quiz(quiz(Name,Day,Slot,NumTAs),TR,AssignedTAs).
	
assign_quiz(quiz(Name,Day,Slot,NumTAs),[day(Dayname,H)|TR],AssignedTAs):-
	Day == Dayname,
	nth1(Slot,H,Slot1),
	length(Slot1,Ln),
	Ln>=NumTAs,
	permutation(Slot1, TAS),
	assign_quiz2(TAS,NumTAs,AssignedTAs).
	

assign_quiz2(_,0,[]).                      
assign_quiz2([TAS|T],NumTAs,[TAS|AssignedTAs]):-                            
    NumTAs2 is NumTAs - 1,                 
    assign_quiz2(T,NumTAs2,AssignedTAs). 
		
assign_quizzes([],_,[]).
assign_quizzes([],FreeSchedule,FreeSchedule).		
assign_quizzes([quiz(Course,Day,Slot,Count)|RestQuizzes], FreeSchedule, [proctors(quiz(Course,Day,Slot,Count),Ata)|RestProctoring]):-
    assign_quiz(quiz(Course,Day,Slot,Count),FreeSchedule,Ata),	
    update_free_schedule(FreeSchedule,Ata, Day, Slot, Count, UpdatedFreeSchedule),
	assign_quizzes(RestQuizzes,UpdatedFreeSchedule,RestProctoring).

update_free_schedule([],_ ,_, _, _, []).
update_free_schedule([day(DayName,Times)|Rest],Ata,Day,Slot,Count,[day(DayName,UpdatedTimes)|RestUpdated]):-
    Day =DayName,
    update_time_slot(Times,Ata,Slot,Count,UpdatedTimes),
    update_free_schedule(Rest,Ata,Day,Slot,Count,RestUpdated).
update_free_schedule([day(DayName,Times)|Rest],Ata,Day,Slot,Count,[day(DayName, Times)|RestUpdated]) :-
    Day \=DayName,
    update_free_schedule(Rest,Ata, Day, Slot, Count, RestUpdated).	


update_time_slot([],_, _, _, []).
update_time_slot([Time|Rest],Ata,Slot,Count,[Res|RestUpdated]):-
    Slot=Time,
    subtract(Time,Ata,Res), 
    Count1 is Count+1,	
    update_time_slot(Rest,Ata,Slot,Count1,RestUpdated).
update_time_slot([Time|Rest],Ata,Slot,Count,[Time|RestUpdated]):-
    Slot\=Time,
	Count1 is Count+1,
    update_time_slot(Rest,Ata,Slot,Count1,RestUpdated).	

	
available(_, [], []).
available(Dayname,[ta(Ta,Dayoff)|Ttas],T):-
    Dayname==Dayoff,
    available(Dayname,Ttas,T).
available(Dayname,[ta(Ta,Dayoff)|Ttas],[Ta|T]):-
    Dayname\=Dayoff,
    available(Dayname,Ttas,T).

check(_,[],[]).

check(Ta,[H|Ttas],[H1|Tfreeschd]):-
     check2(Ta,H,H1),
	 check(Ta,Ttas,Tfreeschd).
check2([],_,[]).
check2([Ta|T],Ttas,Tfreeschd):-
	 member(Ta,Ttas),
	 check2(T,Ttas,Tfreeschd).
check2([Ta|T],Ttas,[Ta|Tfreeschd]):-
	 \+member(Ta,Ttas),
	 check2(T,Ttas,Tfreeschd).	 
 
free_schedule(_,[],[]).
free_schedule(AllTAs,[day(Dayname,Slot)|DT], [day(Dayname,FSlot)|FT]):-
    available(Dayname, AllTAs, Available),
    check(Available, Slot, FSlot),	
	free_schedule(AllTAs,DT,FT).
	

assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
	free_schedule(AllTAs,TeachingSchedule,FreeSchedule),
	assign_quizzes(Quizzes,FreeSchedule,ProctoringSchedule).
	



	
















