{-

type nat =
    | Zero
    | Succ of nat

-}

data Bool : Set where
    true : Bool
    false : Bool

data Nat : Set where
    zero : Nat
    succ : Nat -> Nat

-- data ℕ : Set where
--     zero : ℕ
--     succ : ℕ → ℕ

plus : Nat → Nat → Nat
plus zero n = n
plus (succ m) n = succ (plus m n)

-- _+_ : Nat → Nat → Nat
-- zero + n = n
-- succ m + n = succ (m + n)

times : Nat → Nat → Nat
times zero n = zero
times (succ m) n = plus (times m n) n

{-# BUILTIN NATURAL Nat #-}

odgovor : Nat
odgovor = 42

data List : Set → Set where
    [] : {A : Set} → List A
    _::_ : {A : Set} → A → List A → List A

prazenSeznamNaravnihStevil : List Nat
prazenSeznamNaravnihStevil = []

_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys 
(x :: xs) ++ ys = x :: (xs ++ ys) 

data Vec : Set → Nat → Set where
    [] : {A : Set} → Vec A zero
    _::_ : {A : Set} {n : Nat} → A → Vec A n → Vec A (succ n)

_++ᵥ_ : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (plus m n)
[] ++ᵥ ys = ys
(x :: xs) ++ᵥ ys = x :: (xs ++ᵥ ys)

hd : {A : Set} {n : Nat} → Vec A (succ n) → A
hd (x :: xs) = x

data _<_ : Nat → Nat → Set where
    z<s : {n : Nat} → zero < succ n
    s<s : {m n : Nat} → m < n → succ m < succ n

lookup : {A : Set} {m n : Nat} → Vec A n → m < n → A
lookup (x :: xs) z<s = x 
lookup (x :: xs) (s<s m<n) = lookup xs m<n

-- Fin 0 ~ {} ~ {}
-- Fin 1 ~ {0} ~ {Fzero}
-- Fin 2 ~ {0, 1} ~ {Fzero, Fsucc Fzero}
-- Fin 3 ~ {0, 1, 2} ~ {Fzero, Fsucc Fzero, Fsucc Fsucc Fzero}
-- Fin 4 ~ {0, 1, 2, 3} ~ {Fzero, Fsucc Fzero, Fsucc Fsucc Fzero, Fsucc Fsucc Fsucc Fzero}

data Fin : Nat → Set where
    Fzero : {n : Nat} → Fin (succ n)
    Fsucc : {n : Nat} → Fin n → Fin (succ n)

_[_] : {A : Set} {n : Nat} → Vec A n → Fin n → A
(x :: xs) [ Fzero ] = x
(x :: xs) [ Fsucc i ] = xs [ i ]

data Exp : Nat → Set where
    loc : {n : Nat} → Fin n → Exp n
    nat : {n : Nat} → Nat → Exp n
    _+_ : {n : Nat} → Exp n → Exp n → Exp n
    _-_ : {n : Nat} → Exp n → Exp n → Exp n
    _*_ : {n : Nat} → Exp n → Exp n → Exp n

evalExp : {n : Nat} → Vec Nat n → Exp n → Nat
evalExp st (loc l) = st [ l ]
evalExp st (nat m) = m
evalExp st (e₁ + e₂) = plus (evalExp st e₁) (evalExp st e₂)
evalExp st (e₁ - e₂) = {!   !}
evalExp st (e₁ * e₂) = times (evalExp st e₁) (evalExp st e₂)

data BExp : Nat → Set where
    bool : {n : Nat} → Bool → BExp n
    equal : {n : Nat} → Exp n → Exp n → BExp n
    less : {n : Nat} → Exp n → Exp n → BExp n
    more : {n : Nat} → Exp n → Exp n → BExp n

data Cmd : Set where
    