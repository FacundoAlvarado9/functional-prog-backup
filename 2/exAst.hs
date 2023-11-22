data ElemML = IntElem Int | StringElem String | BoolElem Bool
data ML = EmptyML | ML ElemML ML

lengthML :: ML -> Int
lengthML EmptyML = 0
lengthML (ML elem ml) = 1 + lengthML ml

prepend :: ML -> ML -> ML
prepend (ML a mla) b
    | (lengthML (ML a mla)) == 1 = (ML a b)

--- let a = (ML (StringElem "Hola") EmptyML)
--- let b = (ML (StringElem "Chau") (ML (BoolElem True) EmptyML)) // 2 elem
--- lengthML (prepend a b)

mconcat :: ML -> ML -> ML
mconcat (ML a mla) b
    | 