//a slight adaptation of our posfix calculator to allow commands to be "human"
//e.g. 1 plus 2 times 4 divided by 60

syms:(`$("(";")";"minus";"plus";"times";"divided";"power"))!`lp`rp`minus`plus`times`div`exp;
helperwords:`to`the`of`by;
prec:`minus`plus`times`div`exp!1 1 2 2 3;
argn:`minus`plus`times`div`exp!5#'2;
ops:`minus`plus`times`div`exp!(-;+;*;%;xexp);
isop:{type[`a]=type x};
todigit:{(("IF") "." in x)$x};
tokenize:{((syms `$;todigit) any each x in\:.Q.n)@'x}; //space separated to tokens
stot:{raze tokenize string except[ ;helperwords] `$t:" "vs ssr[ ;"  ";" "]/[trim x]}; //strings to tokens, deals with extra spacing and negatives
//standard infix->postfix translation algorithm, http://csis.pace.edu/~wolf/CS122/infix-postfix.htm
conv:{  r:x[0]; os:x[1];
       $[not isop y;                      r:r,y; //a number, goes straight to our result list
        y=`lp;                            os:y,os; ///left parens just get put on oepration stack
        y=`rp;                            [r:r,(w:os?`lp) sublist os; os:(w+1) _ os]; //right parens, find matching left, pop all elems upto lp, add to queue, remove parens on both
        first prec[y]<=prec o:first os;   [r:r,o; os:y,1_os]; //if top of stack operator has higher precedence, pop and add to queu, push new
                                          os:y,os]; //default, add to operator to stack
       (r;os)}; //return postfix translation thus far
postfix:{raze conv/[2#();x]}; //translator from infix to postfix
evaluate:{last {$[not isop y;x,y; (a _x),(ops y) . (a:neg argn y) sublist x]}/[();x]}; //evaluator
calc:{evaluate postfix stot (),x}; //calculator

//some quick examples
(-3+(1%2)+4*5)~calc "1 divided by 2 minus 3 plus 4 times 5"
1000 ~ calc"1000"
(3+1000*-2)~calc "1000 times -2 plus 3"
(-3+4-7)~calc"4 minus 3 minus 7"
(1+2 xexp 2)~calc"2 to the power of 2 plus 1"
