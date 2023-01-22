---------------------------------------------------------------------------------------------
-- Nom du fichier : repertoire.ads
-- Fonction       : Spécifications du modèle Répertoire (repertoire)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

-- https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-4-5.html
with ada.strings.unbounded;
use ada.strings.unbounded;

package repertoire is

    type T_Repertoire is record 
        nom : unbounded_string;
        taille : Integer;
        droits : Natural;
        estFichier : Boolean;
    end record;

    --------------------------------------------------------------------------------------
    -- nom : Rep_Get_Nom
    -- sémantique : récupère le nom du répertoire
    -- paramètres : repertoire IN le répertoire
    -- retourne le nom du répertoire
    --------------------------------------------------------------------------------------
    function Rep_Get_Nom(repertoire : in T_Repertoire) return unbounded_string;

    --------------------------------------------------------------------------------------
    -- nom : Rep_Get_Taille
    -- sémantique : récupère la taille du répertoire
    -- paramètres : repertoire IN le répertoire
    -- retourne la taille du répertoire
    --------------------------------------------------------------------------------------
    function Rep_Get_Taille(repertoire : in T_Repertoire) return Integer;

    --------------------------------------------------------------------------------------
    -- nom : Rep_Get_Droits
    -- sémantique : récupère les droits du répertoire
    -- paramètres : repertoire IN le répertoire
    -- retourne les droits du répertoire
    --------------------------------------------------------------------------------------
    function Rep_Get_Droits(repertoire : in T_Repertoire) return Natural;
    
    --------------------------------------------------------------------------------------
    -- nom : Rep_Get_estFichier
    -- sémantique : permet de savoir si le répertoire est un fichier ou non (non = dossier)
    -- paramètres : repertoire IN le répertoire
    -- retourne le boolean estFichier du répertoire
    --------------------------------------------------------------------------------------
    function Rep_Get_estFichier(repertoire : in T_Repertoire) return Boolean;

end repertoire;