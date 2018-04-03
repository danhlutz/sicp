(load "logic.scm")

(define genesis
  (list '(son Adam Cain)
        '(son Cain Enoch)
        '(son Enoch Irad)
        '(son Irad Mehujael)
        '(son Mehujael Methushael)
        '(son Methushael Lamech)
        '(wife Lamech Ada)
        '(son Ada Jabal)
        '(son Ada Jubal)
        '(rule (son ?father ?son)
               (and (wife ?father ?wife)
                    (son ?wife ?son)))
        '(rule (grandson ?gf ?gs)
               (and (son ?gf ?father)
                    (son ?father ?gs)))
        '(rule (append-to-form () ?y ?y))
        '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z))
        '(rule (ends-in ?items ?x)
               (append-to-form ?z ?x ?items))
        '(rule ((great . ?rel) ?gxf ?gxs)
               (and (ends-in ?rel (grandson))
                    (append-to-form (great) ?rel2 ?rel)
                    (son ?gxf ?gxf-son)
                    ((great . ?rel2) ?gxf-son ?gxs)))
        '(rule ((great grandson) ?ggf ?ggs)
               (and (son ?ggf ?gf)
                    (grandson ?gf ?ggs)))))



(populate-db genesis)