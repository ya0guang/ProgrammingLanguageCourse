module pie2 where

open import Relation.Binary.PropositionalEquality
open import Data.Product hiding (map)

data ℕ : Set where
 zero : ℕ
 add1 : ℕ → ℕ

-- (List A) : U, A : U
data List (A : Set) : Set where
  nil : List A
  kons : A → List A → List A

-- you can introduce implicit arguments by {}s
data Vec (A : Set) : ℕ → Set where
  vecnil : Vec A zero
  veckons : {ℓ : ℕ} → A → Vec A ℓ → Vec A (add1 ℓ)

_+_ : ℕ → ℕ → ℕ
zero     + m = m
(add1 n) + m = add1 (n + m)

length : ∀ {A : Set} → List A → ℕ
length nil = zero
length (kons x l) = add1 (length l)

map : ∀ {A B : Set} → (f : A → B) → (l : List A) → List B
map f nil = nil
map f (kons x l) = kons (f x) (map f l)

map-same-len : ∀ {A B : Set} → (l : List A) → (f : A → B)
               → (length l) ≡ (length (map f l))
map-same-len nil f        = refl
{-
(length nil) ≡ (length (map f nil))
0 ≡ (length (map f nil))
0 ≡ (length nil)
0 ≡ 0
-}
map-same-len (kons x l) f = cong add1 (map-same-len l f) 
{-
(map-same-len l f) : (length l) ≡ (length (map f l))
-----------------------------------------
want: (add1 (length l)) ≡ (add1 (length (map f l)))
-}

_++_ : ∀ {A : Set} → (l₁ : List A) → (l₂ : List A) → List A
nil         ++ l₂ = l₂
(kons x l₁) ++ l₂ = kons x (l₁ ++ l₂)

append-assoc : ∀ {A : Set} → (l₁ l₂ l₃ : List A)
               → (l₁ ++ (l₂ ++ l₃)) ≡ ((l₁ ++ l₂) ++ l₃)
append-assoc nil l₂ l₃ = refl
{-
(l₂ ++ l₃) ≡ (l₂ ++ l₃)
-}
append-assoc (kons x l₁) l₂ l₃ = cong (λ d → kons x d) (append-assoc l₁ l₂ l₃)
{-
ih : (l₁ ++ (l₂ ++ l₃)) ≡ ((l₁ ++ l₂) ++ l₃)
----------------------------------
want : (kons x (l₁ ++ (l₂ ++ l₃))) ≡ (kons x ((l₁ ++ l₂) ++ l₃))
-}
  
Even : ℕ → Set
Even n = Σ[ half ∈ ℕ ](n ≡ half + half)
 
four-is-even : Even (add1 (add1 (add1 (add1 zero))))
four-is-even = (add1 (add1 zero)) , refl

+-comm : (a b : ℕ) → a + b ≡ b + a

+-asso : (a b c : ℕ) → (a + (b + c)) ≡ ((a + b) + c)

a+b+c+d=a+c+b+d : (a b c d : ℕ) → (a + b) + (c + d) ≡ (a + c) + (b + d)
a+b+c+d=a+c+b+d zero b c d with subst (λ hole → (b + (c + d)) ≡ hole) (+-asso b c d) refl
... | r₁ with subst (λ hole → (b + (c + d)) ≡ hole + d) (+-comm b c) r₁
... | r₂ with subst (λ hole → (b + (c + d)) ≡ hole) (sym (+-asso c b d)) r₂
... | r = r
{-
relf : (b + (c + d)) = (b + (c + d))

assoc, r₁: (b + (c + d)) = ((b + c) + d))
comm, r₂: (b + (c + d)) = ((c + b) + d))
assoc, r: (b + (c + d)) = (c + (b + d)))

want : (b + (c + d)) = (c + (b + d))
-}
a+b+c+d=a+c+b+d (add1 a) b c d = cong add1 (a+b+c+d=a+c+b+d a b c d)

{-
want : add1 ((a + b) + (c + d)) ≡ add1 ((a + c) + (b + d))
-}

even+even->even : ∀ {n m : ℕ} → Even n → Even m
                  → Even (n + m)
even+even->even {n} {m} (h₁ , n=h₁+h₁) (h₂ , m=h₂+h₂)
 = h₁ + h₂ , (subst (λ hole → n + m ≡ hole) (a+b+c+d=a+c+b+d h₁ h₁ h₂ h₂)
 (subst₂ (λ hole₁ hole₂ → n + m ≡ hole₁ + hole₂) n=h₁+h₁ m=h₂+h₂ refl))

{-
subst in Agda is replace in Pie
-}
{-
n = h₁ + h₁
m = h₂ + h₂
refl : n + m = n + m
(subst (λ hole → n + m = hole + m) n=h₁+h₁ refl) : n + m = (h₁ + h₁) + m
(subst (λ hole → n + m = _ + hole) m=h₂+h₂ r) : n + m = (h₁ + h₁) + (h₂ + h₂)
(subst (λ hole → n + m = hole) (a+b+c+d=a+c+b+d) r₁)
--------------------------------
want : n + m = ((h₁ + h₂) + (h₁ + h₂))
-}


