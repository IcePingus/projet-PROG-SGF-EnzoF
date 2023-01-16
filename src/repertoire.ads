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

    function Rep_Get_Nom(repertoire : in T_Repertoire) return unbounded_string;
    function Rep_Get_Taille(repertoire : in T_Repertoire) return Integer;
    function Rep_Get_Droits(repertoire : in T_Repertoire) return Natural;
    function Rep_Get_estFichier(repertoire : in T_Repertoire) return Boolean;
    
    procedure Rep_Set_Taille(repertoire : in out T_Repertoire; taille : in Integer);

end repertoire;