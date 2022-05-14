% <======================================================================================>
% <======================================================================================>
% --------------------------------Tree In Prolog-----------------------------------------
% <======================================================================================>
% <======================================================================================>



ibt(empty).
ibt(node(N,L,R)):-integer(N),ibt(L),ibt(R).


%==================>    some  useful functions ===================>
left_tree(ibt(node(N,L,R)),ibt(L)).
right_tree(ibt(node(N,L,R)),ibt(R)).
value(ibt(node(N,L,R)),N).
isempty(ibt(empty)).


max_num(X,Y,Y):-Y>=X.
max_num(X,Y,X):-X>Y.

diff_num(X,Y,Z):-X>Y->Z is X-Y;Z is Y-X.

%----------------------------list functions-------------------------

list_len([],0).
list_len([X|L],N):-N1 is N-1,list_len(L,N1).

list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).

list_addf(X,L1,[X|L1]).

list_addb(X,[],[X]).
list_addb(X,[Y|L1],[Y|L2]):-list_addb(X,L1,L2).

list_create(L,L).

list_empty([]).

list_ispresent(X,[X|L]).
list_ispresent(X,[Y|L]):-list_ispresent(X,L).

list_dlt(X,[X|L],L).
list_dlt(X,[Y|L1],[Y|L2]):-list_dlt(X,L1,L2).


list_divide(0,L,[],L).
list_divide(N,[X|L],[X|L1],L2):-N1 is N-1,
                                list_divide(N1,L,L1,L2).

list_similar(L1,L):-list_len(L1,N),list_len(L,N),is_present(X,L),is_present(X,L1).


%================================================================
%                      SOLUTIONS
%================================================================

%--------------------- HEIGHT----------------------

height(ibt(empty),0).
height(BT,N):-left_tree(BT,L),
              right_tree(BT,R),
              height(L,N1),
              height(R,N2),
              max_num(N1,N2,Z),
              N is Z+1.

%---------------------SIZE--------------------------

size(ibt(empty),0).
size(BT,N):-left_tree(BT,L),
            right_tree(BT,R),
            size(L,N1),
            size(R,N2),
            N is 1+N1+N2.

%-----------------------PREORDER---------------------
preorder(ibt(empty),[]).
preorder(BT,L):-left_tree(BT,LT),
                right_tree(BT,RT),
                value(BT,N),
                preorder(LT,L0),
                preorder(RT,L2),
                list_addf(N,L0,L1),
                list_concat(L1,L2,L).

%-----------------------INORDER------------------------

inorder(ibt(empty),[]).
inorder(BT,L):- left_tree(BT,LT),
                right_tree(BT,RT),
                value(BT,N),
                inorder(LT,L1),
                inorder(RT,L0),
                list_addf(N,L0,L2),
                list_concat(L1,L2,L).

%------------------------POSTORDER----------------------

postorder(ibt(empty),[]).
postorder(BT,L):-   left_tree(BT,LT),
                    right_tree(BT,RT),
                    value(BT,N),
                    postorder(LT,L1),
                    postorder(RT,L0),
                    list_addb(N,L0,L2),
                    list_concat(L1,L2,L).

%
%-------------------------------------------------
% TAIL RECURSION
%-------------------------------------------------
trPreorder(ibt(empty),[]).
trPreorder(BT,[X|L]):-  left_tree(BT,LT),
                        right_tree(BT,RT),
                        value(BT,X),
                        size(LT,N),
                        list_divide(N,L,L1,L2),
                        trPreorder(LT,L1),
                        trPreorder(RT,L2),!.

trInorder(ibt(empty),[]).
trInorder(BT,L):-   left_tree(BT,LT),
                    right_tree(BT,RT),
                    value(BT,X),
                    size(LT,N),
                    list_divide(N,L,L1,[X|L2]),
                    trInorder(LT,L1),
                    trInorder(RT,L2),!.
                

trPostorder(ibt(empty),[]).
trPostorder(BT,L):- left_tree(BT,LT),
                    right_tree(BT,RT),
                    value(BT,X),
                    size(LT,N),
                    list_divide(N,L,L1,L0),
                    list_concat(L2,[X],L0),
                    trPostorder(LT,L1),
                    trPostorder(RT,L2),!.
                            


%
%----------------------------------------------
%  EULERTOUR
%----------------------------------------------

eulerTour(ibt(empty),[]).
eulerTour(BT,L):-   left_tree(BT,LT),
                    right_tree(BT,RT),
                    value(BT,N),
                    eulerTour(LT,L01),
                    eulerTour(RT,L02),
                    list_addeuler(L01,N,L3),
                    list_addeuler(L02,N,L2),
                    list_addf(N,L3,L1),
                    list_concat(L1,L2,L).
                  

list_addeuler([],N,[]).
list_addeuler(L,N,L1):- \+ list_empty(L),
                        list_addb(N,L,L1).

%--------------------------------------------
preET(ibt(empty),[]).
preET(BT,L):-eulerTour(BT,L0),
             seprate_pre(L0,L,[]).

seprate_pre([],[],V).
seprate_pre([X|L1],[X|L],V):- \+ list_ispresent(X,V),
                              list_addb(X,V,V1),
                              seprate_pre(L1,L,V1).

seprate_pre([X|L1],L,V):- list_ispresent(X,V),
                           seprate_pre(L1,L,V).
                            

postET(ibt(empty),[]).
postET(BT,L):-eulerTour(BT,L0),
            seprate_post(L0,L,[]),!.

seprate_post([],[],V).
seprate_post(L1,L,V):- list_addb(X,L01,L1),
                       \+ list_ispresent(X,V),
                       list_addb(X,V,V1),
                       list_addb(X,L0,L),
                       seprate_post(L01,L0,V1).

seprate_post(L1,L,V):- list_addb(X,L01,L1),
                       list_ispresent(X,V),
                       seprate_post(L01,L,V).


inET(ibt(empty),[]).
inET(BT,L):- left_tree(BT,LT),
            right_tree(BT,RT),
            value(BT,N),
            inorder(LT,L1),
            inorder(RT,L0),
            list_addf(N,L0,L2),
            list_concat(L1,L2,L).
                


%------------------------------------------------
% TOSTRING
%-----------------------------------------------

toString(ibt(empty),"()").
toString(BT,S):-left_tree(BT,LT),
                right_tree(BT,RT),
                value(BT,N),
                toString(LT,S1),
                toString(RT,S2),
                atomics_to_string(["(",N,", ",S1,", ",S2,")"], S).
                



%--------------------------------------------------
% ISBALANCED
%--------------------------------------------------

isBalanced(ibt(empty)).
isBalanced(BT):-left_tree(BT,LT),
                right_tree(BT,RT),
                height(LT,H1),
                height(RT,H2),
                diff_num(H1,H2,H),
                H<2,
                isBalanced(LT),
                isBalanced(RT).

%---------------------------------------------------
% isBST(BT)
%---------------------------------------------------

isBST(ibt(empty)).
isBST(BT):-left_tree(BT,LT),
          right_tree(BT,RT),
          value(BT,N),
          check_leftnode(N,LT),
          check_rightnode(N,RT),
          isBST(LT),
          isBST(RT).

check_leftnode(N,ibt(empty)).
check_leftnode(N,LT):- \+ isempty(LT),
                       value(LT,N0),
                       N0<N.

check_rightnode(N,ibt(empty)).
check_rightnode(N,RT):- \+ isempty(RT),
                      value(RT,N0),
                      N0>N.


%------------------------------------------
% makeBST(L,BST)
%------------------------------------------





makeBST(L,BST) :- sort(L,X), bst_maker(X,BST),!.
divide(L, A, B, C) :- %It divides a list in 2 halves and 1 element

                    append(A, [C|B], L),
                    length(A, O),
                    length(B, N),
                    ((O-1)=:=N;O=:=N),!.
            
bst_maker([],ibt(empty)).
bst_maker([N],BST):- BST = ibt(node(N,empty,empty)).
bst_maker(L,BST):- divide(L, A, B, C) ,
                   value(BST,C),
                   left_tree(BST,LT),
                   right_tree(BST,RT),
                   bst_maker(A,LT),
                   bst_maker(B,RT).


%
%---------------------------------------------
% LOOKUP
%---------------------------------------------

lookup(N,ibt(node(N,empty,empty))).
lookup(N,BST):-left_tree(BST,LT),
               right_tree(BST,RT),
               value(BST,N0),
               take_bst(RT,LT,N0,N,BT),
               lookup(N,BT),!.

take_bst(RT,LT,N,N,ibt(node(N,empty,empty))).
take_bst(RT,LT,N0,N,RT):- N>N0.
take_bst(RT,LT,N0,N,LT):-N<N0.

%
%-----------------------------------------------
% INSERT
%-----------------------------------------------
%


insert(N,ibt(empty),ibt(node(N,empty,empty))).
insert(N,BST1,BST2):-value(BST1,N0),value(BST2,N0),
                     left_tree(BST1,LT1),
                     right_tree(BST1,RT1),
                     N>N0,
                     left_tree(BST2,LT1),
                     right_tree(BST2,RT2),
                     insert(N,RT1,RT2).

insert(N,BST1,BST2):-value(BST1,N0),
                     value(BST2,N0),
                     left_tree(BST1,LT1),
                     right_tree(BST1,RT1),
                     N0>N,
                     left_tree(BST2,LT2),
                     right_tree(BST2,RT1),
                     insert(N,LT1,LT2).



%
%----------------------------------------------------
% DELETE
%----------------------------------------------------
%
minval(ibt(X,empty,empty),X).
minval(BT,X):-
              left_tree(BT,LT),
              \+ isempty(LT),
              minval(LT,X).
minval(BT,X):-
                left_tree(BT,LT),
                isempty(LT),
                value(BT,X).

delete(N,ibt(empty),ibt(empty)).
delete(N,ibt(node(N,empty,empty)),ibt(empty)).
delete(N,BST1,BST2):-left_tree(BST1,LT1),
                     right_tree(BST1,RT1),
                     value(BST1,X),
                     N>X,
                     value(BST2,X),
                     left_tree(BST2,LT1),
                     right_tree(BST2,RT2),
                     delete(N,RT1,RT2).
delete(N,BST1,BST2):-left_tree(BST1,LT1),
                    right_tree(BST1,RT1),
                    value(BST1,X),
                    N<X,
                    value(BST2,X),
                    left_tree(BST2,LT2),
                    right_tree(BST2,RT1),
                    delete(N,LT1,LT2).

delete(N,BST1,LT1):- value(BST1,X),
                left_tree(BST1,LT1),
                right_tree(BST1,RT1),
                N=:=X,
                isempty(RT1),
                \+ isempty(LT1).

delete(N,BST1,RT1):- value(BST1,X),
            left_tree(BST1,LT1),
            right_tree(BST1,RT1),
            N=:=X,
            isempty(LT1),
            \+ isempty(RT1).




delete(N,BST1,BST2):- value(BST1,X),
                      left_tree(BST1,LT1),
                      right_tree(BST1,RT1),
                      N=:=X,
                      value(BST2,X1),
                      left_tree(BST2,LT1),
                      right_tree(BST2,RT2),
                      \+ isempty(RT1),
                      \+ isempty(LT1),
                      minval(RT1,X1),
                      delete(X1,RT1,RT2).



%---------------------------------------------------------
