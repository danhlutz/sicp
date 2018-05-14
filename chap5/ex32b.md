# optimizing interpretation

I think Alyssa is wrong. 

Each optimization added to the interpreter also adds a cost to other
computation

For example, with this optimization, now every application
involving an undefined compound operator will now have to perform 
two extra instructions. Time is saved when the operator is
defined or a primitive. But time is lost in other cases. 

I believe every interpreter optimization of this sort probably
comes with similiar trade-offs.
