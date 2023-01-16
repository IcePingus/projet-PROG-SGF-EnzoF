-- paramètres génériques:
generic

-- type T des valeurs de l'arbre n-aire
type T is private;

-- partie spécifications du paquetage
package arbre_nr is

-- type de l'arbre
type arb_nr;

-- enregistrement type noeud de arbre (arb_nr pointe vers noeud)
type noeud;
type arb_nr is access noeud;
type noeud is record
   val: T; 
   premier_fils: arb_nr;
   frere: arb_nr;
   pere: arb_nr;
end record; 

---------------
-- exception 
---------------

-- arbre vide
arbre_vide : exception;
-- pas de parenté  
parente_vide : exception;

---------------
-- getters 
---------------

function An_Get_PremierFils(arbre : in arb_nr) return arb_nr;
function An_Get_Pere(arbre : in arb_nr) return arb_nr;
function An_Get_Frere(arbre : in arb_nr) return arb_nr;
function An_Get_Valeur(arbre : in arb_nr) return T;

---------------
-- consultation 
---------------

function An_Vide(arbre : in arb_nr) return boolean;
function An_Fils(arbre : in arb_nr; numero_fils : in integer) return arb_nr;
function An_Nombre_Fils(arbre : in arb_nr) return integer;

---------------
-- modification
---------------

procedure An_Inserer_Fils(arbre : in out arb_nr; arbre_fils : in out arb_nr);
procedure An_Supprimer_Fils(arbre : in out arb_nr ; numero_fils : in integer);

end arbre_nr;