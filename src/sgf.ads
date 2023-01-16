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
subtype T_Arbre is P_arbre.arb_nr;
type tableau_unbounded is array (0..9) of unbounded_String ;

function rechercher_par_nom(arbre : T_Arbre; nom_repertoire : unbounded_String; est_fichier : boolean) return Integer;
function recuperer_nom_repertoire(arbre : T_Arbre; destination_repertoire : unbounded_String) return unbounded_String;
function verifier_destination(arbre : T_Arbre; destination_repertoire : unbounded_String; supprimer_dernier_mot_cle : boolean) return T_Arbre;
procedure initialisation_sgf(un_arbre : OUT T_Arbre);
procedure repertoire_courant(arbre : T_Arbre);
procedure creer_repertoire(arbre : T_Arbre; destination_repertoire : unbounded_String; taille : Natural; droits : Natural; estFichier : boolean);
procedure modifier_taille_fichier(arbre : T_Arbre; destination_repertoire : unbounded_String; taille : Integer);
procedure changer_direction_repertoire(arbre : IN OUT T_Arbre; destination_repertoire : unbounded_String);
procedure afficher_contenu_repertoire(arbre : T_Arbre; destination_dossier : unbounded_String; repertoire_courant : boolean);
procedure afficher_tous_les_repertoires(arbre: in T_Arbre; destination_dossier : unbounded_String ; repertoire_courant : boolean);
procedure supprimer_repertoire(arbre : T_Arbre; destination_repertoire : unbounded_String; estFichier : boolean);
procedure deplacement_copie_fichier(arbre : T_Arbre; destination_source : unbounded_String; destination_cible : unbounded_String; deplacement : boolean);
procedure archiver_dossier(arbre : T_Arbre; destination_repertoire : unbounded_string);

end sgf;