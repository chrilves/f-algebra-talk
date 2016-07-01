(**

 Preuve que pour les algèbres:

 MonoidInj[I, A] {
   0   : A
   +   : A -> A -> A
   inj : I -> A

   avec (A, 0, +) monoid sur A
        et inj une injection de I sur A
 }

 et

 Liste[A] {
   0    : A
   cons : I -> A
 }


 le morphisme qui pose:
   0 = 0
   cons(x,l) = inj(x) + l

 n'est pas injectif et donc qu'il n'existe pas d'isomorphisme
 entre ces deux algèbres tel que cons(x,l) = inj(x) + l .
*)

Inductive Option (A : Set) : Set :=
  | None : Option A
  | Some : A -> Option A
.

Arguments None {A}.
Arguments Some {A} _.


Inductive Liste (A : Set) : Set :=
  | Vide : Liste A
  | Cons : Option A -> Liste A -> Liste A
.

Arguments Vide {A}.
Arguments Cons {A} _ _.


(* Definition des algèbres de liste sur A *)
Record ListeAlg (L : Set -> Set) (A : Set) : Set := mkListeAlg {
  vide : L A;
  cons : A -> L A -> L A
}.

Arguments mkListeAlg {_} {_} _ _.
Arguments vide {_} {_} _.
Arguments cons {_} {_} _ _ _.

(* Egalisté entre extensionelle entre algèbres de liste *)
Record ListeAlgEq {L : Set -> Set} {A : Set} (l1 l2 : ListeAlg L A) : Set := mkListeAlgEq {
  eq_vide : vide l1 = vide l2 ;
  eq_cons : forall (a : A) (x : L A), cons l1 a x = cons l2 a x
}.

(* Monoid sur I *)
Record MonoidInj (M : Set -> Set) (A : Set) : Set := mkMonoidInj {
  zero : M A;
  inj  : A -> M A;
  plus : M A -> M A -> M A;

  neutre_gauche : forall (x : M A), plus zero x = x ;
  neutre_droit  : forall (x : M A), plus x zero = x ;
  assoc         : forall (x y z : M A), plus (plus x y) z = plus x (plus y z)
}.

Arguments mkMonoidInj {_} {_} _ _ _ _ _ _ .
Arguments zero {_} {_} _.
Arguments inj  {_} {_} _ _.
Arguments plus {_} {_} _ _ _.
Arguments neutre_gauche {_} {_} _ _.
Arguments neutre_droit  {_} {_} _ _.
Arguments assoc         {_} {_} _ _ _ _.

(* Chaque monoid sur I forme une liste d'algèbre sur I *)
Definition MonoidInjToListeAlg {M : Set -> Set} {A : Set} (m : MonoidInj M A) : ListeAlg M A :=
  mkListeAlg (zero m) (fun (a : A) (ma : M A) => plus m (inj m a) ma).


Fixpoint concat {A : Set} (g d : Liste A) : Liste A :=
  match g with
    | Vide     => d
    | Cons a x => Cons a (concat x d)
  end.

Theorem concat_neutre_gauche : forall {A : Set} (l : Liste A), concat Vide l = l.
Proof.
  intros A l. simpl. reflexivity.
Qed.

Theorem concat_neutre_droit : forall {A : Set} (l : Liste A), concat l Vide = l.
Proof.
  intros A l. simpl. induction l as [|a l' IHl'].
  - reflexivity.
  - simpl. rewrite IHl'. reflexivity.
Qed.

Theorem concat_assoc : forall {A : Set} (g d k : Liste A), concat (concat g d) k = concat g (concat d k).
Proof.
 intros A g. induction g as [| a l IHg'].
 - simpl. reflexivity.
 - intros d k. simpl. rewrite <- IHg'. reflexivity.
Qed.


Fixpoint hasNone {A : Set} (g : Liste A) : bool :=
  match g with
    | Vide            => false
    | Cons  None    _ => true
    | Cons (Some _) x => hasNone x
  end.

Require Import Coq.Bool.Bool.
Require Import Coq.Setoids.Setoid.


Fixpoint concatAbsorb {A : Set} (g d : Liste A) : Liste A :=
  match d with
    | Vide => g
    | _    => match g with
                | Vide            => d
                | Cons (Some a) l => Cons (Some a) (concatAbsorb l d)
                | Cons  None    _ => Cons None Vide
              end
  end.

Theorem concatAbsorb_neutre_gauche : forall {A : Set} (l : Liste A), concatAbsorb Vide l = l.
Proof.
  intros A l. destruct l ; simpl; reflexivity.
Qed.

Theorem concatAbsorb_neutre_droit : forall {A : Set} (l : Liste A), concatAbsorb l Vide = l.
Proof.
  intros A l. destruct l ; simpl ; reflexivity.
Qed.


Lemma concatAbsorb_None : forall {A : Set} (g d : Liste A) (x : Option A), concatAbsorb (Cons None g) (Cons x d) = Cons None Vide.
Proof.
  intro A. destruct d ; intro x ; simpl ; reflexivity.
Qed.

Lemma concatAbsorb_Some : forall {A :Set} (g d : Liste A) (x : A), concatAbsorb (Cons (Some x) g) d = Cons (Some x) (concatAbsorb g d).
Proof.
  intros A g d x.
  simpl.
  destruct d.
  - rewrite concatAbsorb_neutre_droit. reflexivity.
  - reflexivity.
Qed.

Theorem concatAbsorb_assoc : forall {A : Set} (g d k : Liste A), concatAbsorb (concatAbsorb g d) k = concatAbsorb g (concatAbsorb d k).
Proof.
  intros A g. induction g as [|ag g' IHg'] ; intros d k.
  - rewrite !concatAbsorb_neutre_gauche. reflexivity.
  - destruct k eqn:Hk.
    + rewrite !concatAbsorb_neutre_droit. reflexivity.
    + destruct d eqn:Hd.
      * rewrite !concatAbsorb_neutre_droit.
        rewrite !concatAbsorb_neutre_gauche.
        reflexivity.
      * { destruct ag eqn:Hag.
          * rewrite !concatAbsorb_None.
            destruct o0 ; simpl ; try reflexivity.
          * rewrite !concatAbsorb_Some.
            rewrite IHg'.
            reflexivity.
        }
Qed.

Definition MonoidInjConcat (A : Set) : MonoidInj Liste A :=
  mkMonoidInj Vide
              (fun (a:A) => Cons (Some a) Vide)
              concat
              concat_neutre_gauche
              concat_neutre_droit
              concat_assoc
.
 

Definition MonoidInjAbsorb (A : Set) : MonoidInj Liste A :=
  mkMonoidInj Vide
              (fun (a:A) => Cons (Some a) Vide)
              concatAbsorb
              concatAbsorb_neutre_gauche
              concatAbsorb_neutre_droit
              concatAbsorb_assoc
.

Theorem meme_liste_alg : forall A : Set, ListeAlgEq (MonoidInjToListeAlg (MonoidInjConcat A))
                                                    (MonoidInjToListeAlg (MonoidInjAbsorb A)).
Proof.
  intro A. apply mkListeAlgEq ; simpl.
  - reflexivity.
  - destruct x ; reflexivity.
Qed.

Theorem diff_monoid_inj : forall (A : Set) (a : A),
    let gauche := Cons  None    Vide in
    let droit  := Cons (Some a) Vide in
    plus (MonoidInjConcat A) gauche droit = plus (MonoidInjAbsorb A) gauche droit -> False.
Proof.
  intros A a.
  simpl.
  intro H.
  discriminate H.
Qed.