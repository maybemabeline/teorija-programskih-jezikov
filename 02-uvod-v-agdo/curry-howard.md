# Curry-Howardov izomorfizem

Delajmo v konstruktivnem svetu, torej brez:
    izključene tretje možnosti : A \/ ~A
    oz.
    dokaza s protislovjem : ~~A => A

match riemannova_hipoteza_velja_ali_ne with
| Left _ -> 
| Right _ 

izjava ~ tip
dokaz izjave ~ program ustreznega tipa

Ideja:

dokaz (A /\ B) = (dokaz A) x (dokaz B)
dokaz (A \/ B) = (dokaz A) + (dokaz B)
dokaz (A => B) = (dokaz A) -> (dokaz B)
dokaz T = unit
dokaz F = empty

A /\ B => B /\ A

(dokaz A) x (dokaz B) -> (dokaz B) x (dokaz A)

{1, 2, 3} U {3, 4} = {1, 2, 3, 4}
{1, 2, 3} + {3, 4} = {(1, 0), (2, 0), (3, 0), (3, 1), (4, 1)}
                   = {ι₁(1), ι₁(2), ι₁(3), ι₂(3), ι₂(4)}
