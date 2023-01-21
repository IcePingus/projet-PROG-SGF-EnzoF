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

erreur_chemin : exception;
erreur_taille : exception;
fichier_non_existant : exception;
dossier_non_existant : exception;
fichier_deja_existant : exception;
dossier_deja_existant : exception;
erreur_suppression_dossier : exception;
taille_negative : exception;
taille_actuel : Integer;

function rechercher_par_nom(arbre : arb_nr; nom_repertoire : unbounded_String; est_fichier : boolean) return Integer;
function recuperer_nom_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String) return unbounded_String;
function verifier_destination(arbre : arb_nr; destination_repertoire : unbounded_String; supprimer_dernier_mot_cle : boolean) return arb_nr;
procedure initialisation_sgf(un_arbre : OUT arb_nr);
procedure repertoire_courant(arbre : arb_nr);
procedure creer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; taille : Natural; droits : Natural; estFichier : boolean);
procedure modifier_taille_fichier(arbre : arb_nr; destination_repertoire : unbounded_String; taille : Integer);
procedure changer_direction_repertoire(arbre : IN OUT arb_nr; destination_repertoire : unbounded_String);
procedure afficher_contenu_repertoire(arbre : arb_nr; destination_dossier : unbounded_String; repertoire_courant : boolean);
procedure afficher_tous_les_repertoires(arbre: in arb_nr; destination_dossier : unbounded_String ; repertoire_courant : boolean);
procedure supprimer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; estFichier : boolean);
procedure deplacement_copie_fichier(arbre : arb_nr; destination_source : unbounded_String; destination_cible : unbounded_String; deplacement : boolean);
procedure archiver_dossier(arbre : arb_nr; destination_repertoire : unbounded_string);

end sgf;