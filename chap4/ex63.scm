;; a Biblical database

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

(rule (grandson ?grandfather ?gs)
      (son ?grandfather ?father)
      (son ?father ?gs))

;; find the grandson of Cain
(grandson Cain ?gs)

(rule (son ?father ?son)
      (and (wife ?w ?father)
           (son ?w ?son)))

;; find the sons of Lamech
(son Lamech ?s)

;; find the grandsons of Methushael
(grandson Methushael ?s)
