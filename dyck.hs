data EmptyStack = EmptyStack
newtype Stack x = Stack x

left :: x -> (Stack x -> t) -> t
left x f' = insertOnStack
    where 
        insertOnStack = f' (Stack x)

right :: Stack x1 -> (x1 -> x2) -> x2
right (Stack x) f' = popFromStack
    where
        popFromStack = f' x

start :: (EmptyStack -> t) -> t
start f' = f' EmptyStack

end :: EmptyStack -> ()
end EmptyStack = ()

a = start left left right right end
a2 = start end

w1 = start left right end -- compiles, '()' are balanced
w2 = start left left right left right right end -- compiles. '(()())' are balanced
w3 = start left left right left right left end -- doesn't compile, '(()()(' are not balanced
