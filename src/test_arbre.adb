with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Assertions; use Ada.Assertions;
with arbre_nr;

-- https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-4-5.html
with ada.strings.unbounded;
use ada.strings.unbounded;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure test_arbre is

package P_arbre is new arbre_nr(T => Integer); use P_arbre;

arbre1 : arb_nr;
arbre2 : arb_nr;
arbre3 : arb_nr;
arbre4 : arb_nr;
arbre5 : arb_nr;

begin
    arbre1 := new noeud'(1, null, null, null);
    arbre2 := new noeud'(2, null, null, arbre1);
    arbre3 := new noeud'(-5, arbre2, arbre1, null);

    ----------------------- TEST GETTER

    -- Test An_Get_Pere : récupérer le père d'un arbre
    Assert(An_Get_Pere(arbre1) = null);
    Assert(An_Get_Pere(arbre2) = arbre1);
    
    -- Test An_Get_Premier_Fils : récupérer le premier fils d'un arbre
    Assert(An_Get_PremierFils(arbre1) = null);
    Assert(An_Get_PremierFils(arbre2) = null);
    Assert(An_Get_PremierFils(arbre3) = arbre2);

    -- Test An_Get_Frere : récupérer le frère d'un arbre
    Assert(An_Get_Frere(arbre1) = null);
    Assert(An_Get_Frere(arbre2) = null);
    Assert(An_Get_Frere(arbre3) = arbre1);

    -- Test An_Get_Valeur : récupérer la valeur d'un arbre
    Assert(An_Get_Valeur(arbre1) = 1);
    Assert(An_Get_Valeur(arbre2) = 2);
    Assert(An_Get_Valeur(arbre3) = -5);

    -- Test An_Vide : savoir si un arbre est vide ou non
    Assert(An_Vide(arbre3) = false);
    Assert(An_Vide(arbre4) = true);

    -- si pas de fils retourne 1 sinon retourne le nombre de fils
    Assert(An_Nombre_Fils(arbre1) = 1);
    Assert(An_Nombre_Fils(arbre2) = 1);
    Assert(An_Nombre_Fils(arbre3) = 1);

    arbre4 := new noeud'(4, null, null, null);
    arbre5 := new noeud'(5, null, null, null);

    An_Inserer_Fils(arbre1, arbre4);
    Assert(An_Nombre_Fils(arbre1) = 1);
    An_Inserer_Fils(arbre1, arbre5);
    Assert(An_Nombre_Fils(arbre1) = 2);

    Assert(An_Fils(arbre1, 1) = arbre5);
    Assert(An_Fils(arbre1, 2) = arbre4);
    
    Assert(An_Fils(arbre1, 1) /= arbre4);
    Assert(An_Fils(arbre1, 2) /= arbre5);

    -- Les tests seront expliques plus en details sur le rapport remis avec le projet
end;