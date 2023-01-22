---------------------------------------------------------------------------------------------
-- Nom du fichier : arbre_nr.ads
-- Fonction       : Specification d'un composant de manipulation des arbres à n branches (arbres_nr)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

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

----------------------------------------------------------------
-- nom : An_Get_PremierFils
-- sémantique : récupérer le premier fils d'un arbre
-- paramètres : arbre : arbre dont on va récupérer son fils
-- retourne arb_nr : le premier fils d'un arbre ou null
----------------------------------------------------------------
function An_Get_PremierFils(arbre : in arb_nr) return arb_nr;

----------------------------------------------------------------
-- nom : An_Get_Pere
-- sémantique : récupérer le père d'un arbre
-- paramètres : arbre : arbre dont on va récupérer son père
-- retourne arb_nr : le père d'un arbre ou null
----------------------------------------------------------------
function An_Get_Pere(arbre : in arb_nr) return arb_nr;

----------------------------------------------------------------
-- nom : An_Get_Frere
-- sémantique : récupérer le frère d'un arbre
-- paramètres : arbre : arbre dont on va récupérer son frère
-- retourne arb_nr : le frère d'un arbre ou null
----------------------------------------------------------------
function An_Get_Frere(arbre : in arb_nr) return arb_nr;

----------------------------------------------------------------
-- nom : An_Get_Valeur
-- sémantique : récupérer la valeur d'un arbre
-- paramètres : arbre : arbre dont on va récupérer sa valeur
-- retourne T : la valeur d'un arbre
----------------------------------------------------------------
function An_Get_Valeur(arbre : in arb_nr) return T;

---------------
-- consultation 
---------------

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : An_Vide
---- sémantique : détecte si un arbre est vide ou non
---- type-retour : boolean (arbre vide ou non)
---- paramètres: arbre in arb_nr
--------------------------------------------------------------------
function An_Vide(arbre : in arb_nr) return boolean;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : An_Fils 
---- sémantique : retourne l'arbre n-ième fils d'un arbre
---- paramètres : arbre in arb_nr ; numero_fils in integer : le n-ième fils
---- type-retour : arb_nr (l'arbre fils recherché
---- post-conditions: si l'arbre est vide, on leve l'exception arbre_vide ; si le nième fils n'existe pas, on leve l'exception parente_vide
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
function An_Fils(arbre : in arb_nr; numero_fils : in integer) return arb_nr;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : An_Nombre_Fils
---- sémantique : donne le nombre de fils au premier niveau d'un arbre
---- paramètres: arbre in arb_nr
---- type-retour : integer (nombre de fils) 
---- post-conditions: si l'arbre est vide, on leve l'exception arbre_vide
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
function An_Nombre_Fils(arbre : in arb_nr) return integer;

---------------
-- modification
---------------

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : An_Inserer_Fils
---- sémantique : insère un arbre sans frère en position de premier fils d'un arbre a. L'ancien fils de a devient le premier frère de l'arbre inséré
---- paramètres: arbre in out arb_nr (arbre de départ) ; arbre_fils in out arb_nr (arbre à insérer) 
---- post-conditions: si l'arbre est vide, on lève l'exception arbre_vide
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure An_Inserer_Fils(arbre : in out arb_nr; arbre_fils : in out arb_nr);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : An_Supprimer_Fils
---- sémantique : supprime le n-ième fils d'un arbre a : dans le cas où l'on supprime le premier fils de a, le premier frère du fils supprimé devient le fils de a
---- paramètres: arbre in out arb_nr ; numero_fils in integer (numéro du fils à supprimer)
---- post-conditions: si l'arbre est vide, on lève l'exception arbre_vide
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure An_Supprimer_Fils(arbre : in out arb_nr ; numero_fils : in integer);

end arbre_nr;