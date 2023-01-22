---------------------------------------------------------------------------------------------
-- Nom du fichier : test_sgf.adb
-- Fonction       : Fichier de test sur sgf
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

-- import du package sgf
with sgf; use sgf;
with repertoire; use repertoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Assertions; use Ada.Assertions;
with arbre_nr;

-- https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-4-5.html
with ada.strings.unbounded;
use ada.strings.unbounded;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure test_sgf is

    use P_arbre;

    arbre_actuel : P_arbre.arb_nr;
    arbre_temp : P_arbre.arb_nr;

begin

    -- initialisation du sgf
    initialisation_sgf(arbre_actuel);

    --------------------------------------------------------------------------------------------------------------------------------------
    -- Test de la fonction recuperer_nom_repertoire (permet de récuperer le nom du répertoire dans une chaîne de caractère envoyé (parse)
    --------------------------------------------------------------------------------------------------------------------------------------
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String(".")) = To_Unbounded_String("."));
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String(" ")) = To_Unbounded_String(" "));
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String("fichier1")) = To_Unbounded_String("fichier1"));
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String("dossier1/dossier2/fichier")) = To_Unbounded_String("fichier"));
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String("/dossier1/dossier2/fichier")) = To_Unbounded_String("fichier"));
    Assert(recuperer_nom_repertoire(arbre_actuel,To_Unbounded_String("/dossier1/dossier2/")) = To_Unbounded_String("dossier2"));

    -- création des fichiers pour le prochain test
    creer_repertoire(arbre_actuel, To_Unbounded_String("home"), 0, 777, false);
    creer_repertoire(arbre_actuel, To_Unbounded_String("usr"), 0, 777, false);
    creer_repertoire(arbre_actuel, To_Unbounded_String("dossier3"), 0, 777, false);
    
    creer_repertoire(arbre_actuel, To_Unbounded_String("f1"), 0, 777, true);
    creer_repertoire(arbre_actuel, To_Unbounded_String("f2"), 0, 777, true);
    creer_repertoire(arbre_actuel, To_Unbounded_String("f3"), 0, 777, true);

    ---------------------------------------------------------------------------------------------------------------------
    -- Test de la fonction rechercher_par_nom (vérifie si un répertoire recherché existe et retourne sa position ou null) 
    ---------------------------------------------------------------------------------------------------------------------
    -- sachant que quand on crée un répertoire la position de chaque répertoire incrémente de 1

    -- Test dossier
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("enzo"), false) = 0);
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("home"), false) = 6);
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("dossier3"), false) = 4);

    -- Test fichier
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("fichier"), true) = 0);
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("f1"), true) = 3);
    Assert(rechercher_par_nom(arbre_actuel, To_Unbounded_String("f3"), true) = 1);

    ---------------------------------------------------------------------------------------------------------------------
    -- Test de la fonction verifier_destionation (vérifie si la destination existe et retourne l'arbre (pointeur) correspondant ou null)
    ---------------------------------------------------------------------------------------------------------------------

    -- Test sans supprimer le dernier mot-clé et en partant de la racine
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home/dossier/dossier2"), false) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home/dossier"), false) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home"), false) = An_Fils(arbre_actuel, 6));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/home"), false) = An_Fils(arbre_actuel, 6));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("f1"), false) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/home"), false) = An_Fils(arbre_actuel, 6));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("."), false) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/"), false) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../"), false) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String(".."), false) = null);

    -- Test en supprimant le dernier mot-clé et en partant de la racine  
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home/dossier/dossier2"), true) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home/dossier"), true) = An_Fils(arbre_actuel, 6));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/home"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("f1"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/home"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("."), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("/"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../"), true) = arbre_actuel);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String(".."), true) = arbre_actuel);

    -- Se déplacer dans un répertoire 'home' pour faire de nouveaux tests
    arbre_temp := arbre_actuel;
    changer_direction_repertoire(arbre_actuel, To_Unbounded_String("home"));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("home"), false) = null);

    -- Tests en partant du répertoire '/home' sans supprimer le dernier mot-clé
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../home"), false) = An_Fils(arbre_temp, 6));
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../"), false) = arbre_temp);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String(".."), false) = arbre_temp);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../../"), false) = null);
    Assert(verifier_destination(arbre_actuel, To_Unbounded_String("../.."), false) = null);

    -- Les tests seront expliques plus en details sur le rapport remis avec le projet
end;