with text_io;use text_io;
package body repertoire is

    function Rep_Get_Nom(repertoire : in T_Repertoire) return unbounded_string is
    begin
        return repertoire.nom;
    end;

    function Rep_Get_Taille(repertoire : in T_Repertoire) return Integer is
    begin
        return repertoire.taille;
    end;

    function Rep_Get_Droits(repertoire : in T_Repertoire) return Natural is
    begin
        return repertoire.droits;
    end;

    function Rep_Get_estFichier(repertoire : in T_Repertoire) return Boolean is
    begin
        return repertoire.estFichier;
    end;

    procedure Rep_Set_Taille(repertoire : in out T_Repertoire; taille : in Integer) is
    begin
        repertoire.taille := taille;
    end Rep_Set_Taille;


end repertoire;