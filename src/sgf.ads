---------------------------------------------------------------------------------------------
-- Nom du fichier : sgf.ads
-- Fonction       : Specification d'un composant de Système de Gestion de Fichiers (sgf)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with repertoire; use repertoire;
with arbre_nr;

-- https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-4-5.html
with ada.strings.unbounded;
use ada.strings.unbounded;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package sgf is

package P_arbre is new arbre_nr(T => T_Repertoire); use P_arbre;
type tableau_unbounded is array (0..9) of unbounded_String;
taille_actuel : Integer;

-- exception à propos d'un chemin inexistant
erreur_chemin : exception;
-- exception à propos de l'espace mémoire
erreur_taille : exception;
-- exception à propos d'un fichier qui n'existe pas
fichier_non_existant : exception;
-- exception à propos d'un dossier qui n'existe pas
dossier_non_existant : exception;
-- exception à propos d'un fichier qui existe déjà
fichier_deja_existant : exception;
-- exception à propos d'un dossier qui existe déjà
dossier_deja_existant : exception;
-- exception liée au fait qu'on ne puisse pas supprimer un dossier
erreur_suppression_dossier : exception;
-- exception liée au fait qu'une taille ne peut pas être négative
taille_negative : exception;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : rechercher_par_nom
---- sémantique : cherche si un répertoire 'nom_repertoire' existe dans le répertoire courant
---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; nom_repertoire unbounded_String : nom du répertoire répertoire recherché ; est_fichier boolean : le répertoire recherché est un fichier ou non
---- type-retour : integer (index du répertoire recherché dans l'arbre actuel)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
function rechercher_par_nom(arbre : arb_nr; nom_repertoire : unbounded_String; est_fichier : boolean) return Integer;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : recuperer_nom_repertoire
---- sémantique : permet de récupérer à partir de la 'destination_repertoire' le nom d'un répertoire (récupère le dernier paramètre délimité des '/' (ne vérifie pas si le répertoire existe ou non
---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; destination_repertoire unbounded_String : destination du répertoire recherché
---- type-retour : unbounded_String (nom du répertoire recherché à partir de destination_repertoire (dernière délimitation du '/'))
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
function recuperer_nom_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String) return unbounded_String;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : verifier_destination
---- sémantique : permet de récupérer à partir de la 'destination_repertoire' le nom d'un répertoire (récupère le dernier paramètre délimité des '/' (ne vérifie pas si le répertoire existe ou non
---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; destination_repertoire unbounded_String : destination du répertoire recherché ; supprimer_dernier_mot_cle Boolean : supprimer ou non la dernière délimitation
---- type-retour : arb_nr (arbre correspondant à la destination de 'destination_repertoire')
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
function verifier_destination(arbre : arb_nr; destination_repertoire : unbounded_String; supprimer_dernier_mot_cle : boolean) return arb_nr;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : initialisation_sgf
---- sémantique : permet d'initialiser un sgf (start ou reset)
---- parametres : un_arbre arb_nr : un arbre de T_Repertoire (arbre de départ du sgf)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure initialisation_sgf(un_arbre : OUT arb_nr);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : repertoire_courant
---- sémantique : afficher le répertoire courant (commande pwd)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure repertoire_courant(arbre : arb_nr);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : creer_repertoire
---- sémantique : crée un répertoire à la destination 'destination_repertoire' (commande touch et mkdir)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie ; taille Natural : taille du nouveau répertoire ; droits Natural : droits du nouveau répertoire ; estFichier : le nouveau répertoire est un fichier ou non (=dossier)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure creer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; taille : Natural; droits : Natural; estFichier : boolean);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : modifier_taille_fichier
---- sémantique : modifie la taille d'un fichier à la 'destination_repertoire' (commande size)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie ; taille Natural : nouvelle taille du répertoire
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure modifier_taille_fichier(arbre : arb_nr; destination_repertoire : unbounded_String; taille : Integer);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : changer_direction_repertoire
---- sémantique : permet de se déplacer dans 'destination_repertoire' (commande cd)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure changer_direction_repertoire(arbre : IN OUT arb_nr; destination_repertoire : unbounded_String);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : afficher_contenu_repertoire
---- sémantique : permet d'afficher le contenu d'un répertoire à la destination 'destination_dossier' (commande ls)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_dossier unbounded_String : destination du dossier saisie ; repertoire_courant boolean : recherche faite sur le répertoire courant ou non
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure afficher_contenu_repertoire(arbre : arb_nr; destination_dossier : unbounded_String; repertoire_courant : boolean);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : afficher_tous_les_repertoires
---- sémantique : permet d'afficher le contenu du répertoire et de tous ses sous-répertoires à la destination 'destination_dossier' (commande ls -r)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_dossier unbounded_String : destination du dossier saisie ; repertoire_courant boolean : recherche faite sur le répertoire courant ou non
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure afficher_tous_les_repertoires(arbre: in arb_nr; destination_dossier : unbounded_String ; repertoire_courant : boolean);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : supprimer_repertoire
---- sémantique : permet de supprimer un répertoire à la destination 'destination_repertoire' (commande rm ou rm -r)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie ; estFichier : le nouveau répertoire est un fichier ou non (=dossier)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure supprimer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; estFichier : boolean);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : deplacement_copie_fichier
---- sémantique : permet de copier ou déplacer un fichier de la destination 'destination_source' à la destination 'destination cible' (commande cp ou mv)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_source unbounded_String : destination du répertoire source ; destination_cible unbounded_String : destination du répertoire cible ; déplacement Boolean (est-ce un déplacement ou une copie)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure deplacement_copie_fichier(arbre : arb_nr; destination_source : unbounded_String; destination_cible : unbounded_String; deplacement : boolean);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---- nom : archiver_dossier
---- sémantique : permet d'archiver des dossiers et donc de créer un fichier compressé de ce dossier (commande tar)
---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure archiver_dossier(arbre : arb_nr; destination_repertoire : unbounded_string);

end sgf;