module TheMissingLinks where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; cong-app)
open Eq.≡-Reasoning
open import Data.Product public using (Σ; Σ-syntax; _×_; _,_; proj₁; proj₂; map₁; map₂)
open import Data.Sum
open import Level

-- Renaming Agda's "Set" to "Type" just to not confuse ourselves

Type : (ℓ : Level) → Set (suc ℓ)
Type ℓ = Set ℓ

Type₀ : Type (suc zero)
Type₀ = Type zero

Type₁ : Type (suc (suc zero))
Type₁ = Type (suc zero)

-- Notion of Isomorphism. More on Levels later

infix 0 _≃_
record _≃_ {l m} (A : Type l) (B : Type m) : Type (l ⊔ m) where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

-- Embedding

infix 0 _≲_
record _≲_ (A B : Type₀) : Type₀ where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

-- Type of some predicate
Pred : Type₀ → Type₁
Pred X = X → Type₀

-- Type of a subset
Subset : ∀ {X : Type₀} → Pred X → Type₀
Subset {X} P = Σ[ x ∈ X ] (P x)

-- Predicate on predicates
PredOnPred : Type₀ → Type₁
PredOnPred X = (X → Type₀) → Type₀

-- Set of subsets
SetOfSubs : {X : Type₀} → (PredOnPred X) → Type₁
SetOfSubs {X} ℙ = Σ[ P ∈ Pred X ] (ℙ P)

-- How do we say that something is present in a given set of subsets?
--
-- Probably give back the predicate and show all elements satisfying
-- it are isomorphic to S?
--
_∈s_ : {X : Type₀}
     → {ℙ : PredOnPred X}
     → (S : Type₀) → (SetOfSubs ℙ) → Type₁
_∈s_ {X} S ℙ =
  Σ[ P ∈ Pred X ]
  ( ∀ (x : X) → (P x ≃ S))

-- Type of a potentially infinite union of subsets of X indexed by
-- type J is a triple of index, type by that index (no proof of that),
-- and a proof that it's in the set of subsets
Union : {X : Type₀}
      → {ℙ : PredOnPred X}
      → (J : Type₀)
      → (𝐵 : SetOfSubs ℙ)
      → Type₁
Union J 𝐵 =
  Σ[ j ∈ J ]
  Σ[ Bⱼ ∈ Type₀ ]
  (Bⱼ ∈s 𝐵)

UnionTruncation
  : {X : Type₀}
  → {ℙ : PredOnPred X}
  → (J : Type₀)
  → (𝐵 : SetOfSubs ℙ)
  → Type₁
UnionTruncation J 𝐵 =
  (j : Union J 𝐵) → (k : Union J 𝐵) → (proj₁ j ≡ proj₁ k) → j ≡ k

-- "Topology without tears" 2.3.2 constructively
--
-- 2.3.2 Let (X, τ) be a topological space. A family B of open subsets
-- of X is a basis for τ if and only if for any point x belonging to
-- any open set U , there is a B ∈ B such that x ∈ B ⊆ U.
--
-- This only proves the second part (given ... proves that 𝐵 is a basis)
--
prop232
  : (X : Type₀)
  → {ℙ₁ : PredOnPred X}
  → {ℙ₂ : PredOnPred X}
  → (τ : SetOfSubs ℙ₁)  -- we don't use the whole structure of Topology here
  → (𝐵 : SetOfSubs ℙ₂)
  → (given₁ : ∀ (U : Type₀)
            → (U ≲ X)
            → (U ∈s τ)
            → (x : U)
            → Σ[ B ∈ Type₀ ]
              Σ[ _ ∈ (B ∈s 𝐵) ]
              Σ[ B≲U ∈ B ≲ U ]
              Σ[ b ∈ B ]
              ((_≲_.to B≲U b) ≡ x)
              )
  → (∀ (V : Type₀)
     → (V ≲ X)
     → V ∈s τ
     → UnionTruncation V 𝐵
     → Σ[ J ∈ Type₀ ]
       (V ≃ (Union J 𝐵))
    )
prop232 X τ 𝐵 given₁ V V≲X V∈sτ unionTruncation
  = V
  , record
    { to = λ v → let ( Bₓ , B∈s𝐵 , B≲U , b , b→v ) = given₁ V V≲X V∈sτ v
                  in v , Bₓ , B∈s𝐵
    ; from = λ{ (x , Bₓ , Bₓ∈s𝐵) → x}
    ; from∘to = λ x → refl
    ; to∘from = λ y → unionTruncation
                         ( proj₁ y
                         , proj₁ (given₁ V V≲X V∈sτ (proj₁ y))
                         , proj₁ (proj₂ (given₁ V V≲X V∈sτ (proj₁ y))))
                         y
                         refl
    }
