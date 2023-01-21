with text_io;use text_io;
package body sgf is

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : rechercher_par_nom
    ---- sémantique : cherche si un répertoire 'nom_repertoire' existe dans le répertoire courant
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; nom_repertoire unbounded_String : nom du répertoire répertoire recherché ; est_fichier boolean : le répertoire recherché est un fichier ou non
    ---- type-retour : integer (index du répertoire recherché dans l'arbre actuel)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    function rechercher_par_nom(arbre : arb_nr; nom_repertoire : unbounded_String; est_fichier : boolean) return Integer is
        arbreTemp : arb_nr;
        compteur : Integer := 1;
    begin
        arbreTemp := arbre;
        if (An_Get_PremierFils(arbreTemp) /= null) then
            arbreTemp := An_Get_PremierFils(arbreTemp);
            if (Rep_Get_Nom(An_Get_Valeur(arbreTemp)) = nom_repertoire and Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = est_fichier) then
                return 1;
            else
                while (An_Get_Frere(arbreTemp) /= null) loop
                    arbreTemp := An_Get_Frere(arbreTemp);
                    compteur := compteur +1;
                    if (Rep_Get_Nom(An_Get_Valeur(arbreTemp)) = nom_repertoire and Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = est_fichier) then
                        return compteur;
                    end if;
                end loop;
                return 0;
            end if;
        else
            return 0;
        end if;
    end;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : recuperer_nom_repertoire
    ---- sémantique : permet de récupérer à partir de la 'destination_repertoire' le nom d'un répertoire (récupère le dernier paramètre délimité des '/' (ne vérifie pas si le répertoire existe ou non
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; destination_repertoire unbounded_String : destination du répertoire recherché
    ---- type-retour : unbounded_String (nom du répertoire recherché à partir de destination_repertoire (dernière délimitation du '/'))
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    function recuperer_nom_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String) return unbounded_String is
    nombre_parametre : Integer;
    parametres : tableau_unbounded;
    indice_caractere : Integer;
    begin

        for i in 0..9 loop
            parametres(i) := To_Unbounded_String("");
        end loop;

        -- indice_caractere fait référence à l'indice du caractère que nous parcourons par rapport à destination_repertoire pour vérifier s'il y a un '/'
        -- nombre_parametre récupère le nombre de paramètres après avoir délimiter les '/'
        indice_caractere := 1;
        nombre_parametre := 0;

        -- boucle permettant de délimiter les '/'
        while (indice_caractere <= Length(destination_repertoire)) loop
            if (Element(destination_repertoire, indice_caractere) = '/') then
                -- si un '/' est trouvé le nombre de paramètres est incrémenté
                nombre_parametre := nombre_parametre + 1;
            else
                -- sinon on ajoute la lettre placé à l'indice_caractere à la unbounded string parametre(nombre_parametre)
                parametres(nombre_parametre) := parametres(nombre_parametre) & Element(destination_repertoire, indice_caractere);
            end if;
            -- incrémenter l'indice_caractere pour faire la vérification au prochain caractère
            indice_caractere := indice_caractere + 1;
        end loop;

        -- si le dernier paramètre est vide, décrémenter le nombre de paramètres (peut arriver si le dernier caractère est un '/')
        if parametres(nombre_parametre) = "" then
            nombre_parametre := nombre_parametre -1;
        end if;

        return parametres(nombre_parametre);
    end;


    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : verifier_destination
    ---- sémantique : permet de récupérer à partir de la 'destination_repertoire' le nom d'un répertoire (récupère le dernier paramètre délimité des '/' (ne vérifie pas si le répertoire existe ou non
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (répertoire courant) ; destination_repertoire unbounded_String : destination du répertoire recherché ; supprimer_dernier_mot_cle Boolean : supprimer ou non la dernière délimitation
    ---- type-retour : arb_nr (arbre correspondant à la destination de 'destination_repertoire')
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    function verifier_destination(arbre : arb_nr; destination_repertoire : unbounded_String; supprimer_dernier_mot_cle : boolean) return arb_nr is
    position_repertoire : Integer;
    arbre_temp : arb_nr;
    index_depart : Integer;
    indiceParam : Integer;
    params : tableau_unbounded;
    indice : Integer;

    begin
        -- index_depart depend de si l'utilisateur recherche en référence absolue ou relatif
        -- le parser recupere les valeurs entre chaque '/' il faut donc que l'index de départ soit à 0 si nous avons une référence relatif sinon 1 en référence absolue
        index_depart := 0;
        -- boucle pour initialiser le tableau de unbouded string qui permettra de récupérer les chaînes de caractères entre les '/'
        for i in 0..9 loop
            params(i) := To_Unbounded_String("");
        end loop;

        -- indice fait référence à l'indice du caractère que nous parcourons par rapport à destination_repertoire pour vérifier s'il y a un '/'
        -- indice param récupère le nombre de paramètres après avoir délimiter les '/'
        indice := 1;
        indiceParam := 0;

        -- boucle permettant de délimiter les '/'
        while (indice <= Length(destination_repertoire)) loop
            if (Element(destination_repertoire, indice) = '/') then
                -- si un '/' est trouvé le nombre de paramètres est incrémenté
                indiceParam := indiceParam + 1;
            else
                -- sinon on ajoute la lettre à la unbounded string placé à l'indice paramètre actuel
                params(indiceParam) := params(indiceParam) & Element(destination_repertoire, indice);
            end if;
            -- incrémenter l'indice pour faire la vérification au prochain caractère
            indice := indice + 1;
        end loop;

        -- si le dernier paramètre est vide, décrémenter le nombre de paramètres (peut arriver si le dernier caractère est un '/')
        if params(indiceParam) = "" then
            indiceParam := indiceParam -1;
        end if;

        -- si l'on cherche à faire la vérification en supprimant le dernier terme, on décrémente l'indice maximale de paramètres (utile pour la création d'un fichier/dossier par exemple)
        if supprimer_dernier_mot_cle = true then
            indiceParam := indiceParam -1;
        end if;

        arbre_temp := arbre;

        -- si le premier caractère de l'unbounded string est un '/' alors remonter au répertoire racine (root) et modifier l'index de départ à 1
        if (Element(destination_repertoire, 1) = '/') then
            index_depart := 1;
            while An_Get_Pere(arbre_temp) /= null loop
                arbre_temp := An_Get_Pere(arbre_temp);
            end loop;
        end if;
    
        -- boucle pour accéder à la destination souhaité par l'utilisateur, si un dossier cherché n'existe pas, on retourne null sinon on retourne l'arbre à la bonne destination
        for i in index_depart..indiceParam loop
            -- si le caractère est ".." on accède au père de l'arbre actuel, si il n'existe pas (cas du répertoire root), on retourne null
            if params(i) = ".." then
                if An_Get_Pere(arbre_temp) /= null then
                    arbre_temp := An_Get_Pere(arbre_temp);
                else
                    return null;
                end if; 
            -- si le caractère est différent de ".." et de "." (ne rien faire dans ce cas) on vérifie si le répertoire recherché existe avec rechercher_par_nom (0 si il existe pas), si il existe, on accède à l'arbre recherché, sinon on retourne null (dossier n'existe pas)
            elsif params(i) /= "." then
                position_repertoire := rechercher_par_nom(arbre_temp, params(i), false);
                if position_repertoire /= 0 then
                    arbre_temp := An_Fils(arbre_temp, position_repertoire);
                else
                    return null;
                end if;
            end if;
        end loop;

        -- si null n'a pas été retourné à la fin de la boucle c'est que le chemin proposé par l'utilisateur est bon, on retourne l'arbre au bon répertoire pour lui indiquer
        return arbre_temp;
    end;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : initialisation_sgf
    ---- sémantique : permet d'initialiser un sgf (start ou reset)
    ---- parametres : un_arbre arb_nr : un arbre de T_Repertoire (arbre de départ du sgf)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure initialisation_sgf(un_arbre : OUT arb_nr) is
    un_repertoire : T_Repertoire;
   
    begin
        taille_actuel := 0;
        un_repertoire := (To_Unbounded_String("root"), 0, 777, false);
        un_arbre := new noeud'(un_repertoire, null, null, null);
    end initialisation_sgf;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : repertoire_courant
    ---- sémantique : afficher le répertoire courant (commande pwd)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure repertoire_courant(arbre : arb_nr) is
    pwd : unbounded_string;
    arbre_temp : arb_nr;
    begin
        arbre_temp := arbre;
        pwd := Rep_Get_Nom(An_Get_Valeur(arbre_temp));
        while (An_Get_Pere(arbre_temp) /= null) loop
            arbre_temp := An_Get_Pere(arbre_temp);
            Set_Unbounded_String(pwd, to_String(Rep_Get_Nom(An_Get_Valeur(arbre_temp))) & "/" & to_String(pwd));
        end loop;
        Set_Unbounded_String(pwd, "/" & to_String(pwd));
        put_line(pwd);
    end repertoire_courant;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : creer_repertoire
    ---- sémantique : crée un répertoire à la destination 'destination_repertoire' (commande touch et mkdir)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie ; taille Natural : taille du nouveau répertoire ; droits Natural : droits du nouveau répertoire ; estFichier : le nouveau répertoire est un fichier ou non (=dossier)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure creer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_string; taille : Natural; droits : Natural; estFichier : boolean) is
    
    arbre_pere : arb_nr;
    un_repertoire : T_Repertoire;
    arbre_nouveau_repertoire : arb_nr;
    nom_repertoire : Unbounded_String;

    begin

        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            if (rechercher_par_nom(arbre_pere, nom_repertoire, estFichier) = 0) then
                if taille_actuel + taille < 1000000000 then
                    un_repertoire := (nom_repertoire, 0, droits, estFichier);
                    arbre_nouveau_repertoire := new noeud'(un_repertoire, null, null, arbre_pere);
                    An_Inserer_Fils(arbre_pere, arbre_nouveau_repertoire);
                    if (estFichier = true) then
                        modifier_taille_fichier(arbre, destination_repertoire, taille);
                    end if;
                    if (estFichier = true) then
                        put_line("Le fichier "& nom_repertoire &" a ete cree avec succes");
                    else 
                        put_line("Le dossier "& nom_repertoire &" a ete cree avec succes");
                    end if;
                else
                    raise erreur_taille;
                end if;
            else
                if (estFichier = true) then
                    raise fichier_deja_existant;
                else 
                    raise dossier_deja_existant;
                end if;
            end if;
        else
            raise erreur_chemin;
        end if;
    exception
        when erreur_taille => put_line("Erreur : Impossible d'executer la commande, plus de place sur le disque dur"); 
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas"); 
        when fichier_deja_existant => put_line("Erreur : Le fichier "& nom_repertoire &" existe deja dans le repertoire courant");
        when dossier_deja_existant => put_line("Erreur : Le dossier "& nom_repertoire &" existe deja dans le repertoire courant");
    end creer_repertoire;

    procedure modifier_taille_fichier(arbre : arb_nr; destination_repertoire : unbounded_String; taille : Integer) is
    arbre_pere : arb_nr;
    arbre_temp : arb_nr;
    nom_repertoire : Unbounded_String;
    position_repertoire : Integer;
    ancienne_taille : Natural;
    comparaison_taille : Integer;
    begin
        if taille < 0 then
            raise taille_negative;
        end if;
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            position_repertoire := rechercher_par_nom(arbre_pere, nom_repertoire, true);
            if position_repertoire /= 0 then
                ancienne_taille := Rep_Get_Taille(An_Get_Valeur(An_Fils(arbre_pere, position_repertoire)));
                comparaison_taille := taille - ancienne_taille;
                arbre_temp := arbre_pere;
                if taille_actuel + comparaison_taille > 1000000000 then
                    raise erreur_taille;
                end if; 
                taille_actuel := taille_actuel + comparaison_taille;
                -- modifier la taille de chaque répertoire jusqu'à la racine
                while An_Get_Pere(arbre_temp) /= null loop
                    arbre_temp.all.val.taille := Rep_Get_Taille(An_Get_Valeur(arbre_temp)) + comparaison_taille;
                    arbre_temp := An_Get_Pere(arbre_temp);
                end loop;
                arbre_pere := An_Fils(arbre_pere, position_repertoire);
                arbre_pere.all.val.taille := taille;
            else
                raise fichier_non_existant;
            end if;
        else
            raise erreur_chemin;
        end if;

    exception
        when taille_negative => put_line("Erreur : La taille ne peut pas etre negative");
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !"); 
        when erreur_taille => put_line("Erreur : Impossible d'executer la commande, plus de place sur le disque dur"); 
        when fichier_non_existant => put_line("Erreur : Le fichier '"& nom_repertoire &"' n'existe pas dans le dossier " & Rep_Get_nom(An_Get_Valeur(arbre_pere)));
    end modifier_taille_fichier;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : changer_direction_repertoire
    ---- sémantique : permet de se déplacer dans 'destination_repertoire' (commande cd)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure changer_direction_repertoire(arbre : IN OUT arb_nr; destination_repertoire : unbounded_String) is
    arbre_temp : arb_nr;

    begin
        arbre_temp := verifier_destination(arbre, destination_repertoire, false);

        if arbre_temp /= null then
            arbre := arbre_temp;
        else
            raise erreur_chemin;
        end if;
    exception
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !") ; 
    end changer_direction_repertoire;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : afficher_contenu_repertoire
    ---- sémantique : permet d'afficher le contenu d'un répertoire à la destination 'destination_dossier' (commande ls)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_dossier unbounded_String : destination du dossier saisie ; repertoire_courant boolean : recherche faite sur le répertoire courant ou non
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure afficher_contenu_repertoire(arbre : arb_nr; destination_dossier : unbounded_String; repertoire_courant : boolean) is
    arbreTemp : arb_nr;
    mise_en_forme : Natural;    
    arbre_temp : arb_nr;

    begin
        if repertoire_courant = false and destination_dossier /= "" then
            arbre_temp := verifier_destination(arbre, destination_dossier, false);
        else
            arbre_temp := arbre;
        end if;

        if arbre_temp /= null then
            if (An_Get_PremierFils(arbre_temp) /= null) then      
                put_line("Name                    Length           Rights          File");
                put_line("===============================================================");
                arbreTemp := An_Get_PremierFils(arbre_temp);
                mise_en_forme := 25 - length(Rep_Get_Nom(An_Get_Valeur(arbreTemp)));
                put(Rep_Get_Nom(An_Get_Valeur(arbreTemp))); put(mise_en_forme *" "); put(Integer'image(Rep_Get_Taille(An_Get_Valeur(arbreTemp)))); put("             "); put(Integer'image(Rep_Get_Droits(An_Get_Valeur(arbreTemp))));put("           ");
                if(Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = true) then
                    put("Fichier");
                else
                    put("Dossier");
                end if;
                while (An_Get_Frere(arbreTemp) /= null) loop
                    new_line;
                    arbreTemp := An_Get_Frere(arbreTemp);
                    mise_en_forme := 25 - length(Rep_Get_Nom(An_Get_Valeur(arbreTemp)));
                    put(Rep_Get_Nom(An_Get_Valeur(arbreTemp))); put(mise_en_forme *" "); put(Integer'image(Rep_Get_Taille(An_Get_Valeur(arbreTemp)))); put("             "); put(Integer'image(Rep_Get_Droits(An_Get_Valeur(arbreTemp))));put("           ");
                    if(Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = true) then
                        put("Fichier");
                    else
                        put("Dossier");
                    end if;
                end loop;
                new_line;
            else
                put_line("Le dossier " & Rep_Get_nom(An_Get_Valeur(arbre_temp)) & " ne possede pas de repertoires");
            end if;
        else
            raise erreur_chemin;
        end if;

    exception
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !") ; 
    end afficher_contenu_repertoire;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : afficher_tous_les_repertoires
    ---- sémantique : permet d'afficher le contenu du répertoire et de tous ses sous-répertoires à la destination 'destination_dossier' (commande ls -r)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_dossier unbounded_String : destination du dossier saisie ; repertoire_courant boolean : recherche faite sur le répertoire courant ou non
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure afficher_tous_les_repertoires(arbre: in arb_nr;  destination_dossier : unbounded_String; repertoire_courant : boolean) is
        arbre_temp: arb_nr;
        nbEspaces: Integer := 0;
        arbre_temp2 : arb_nr ;
    begin
        
        if repertoire_courant = true and destination_dossier /= "" then
            arbre_temp2 := verifier_destination(arbre, destination_dossier, false);
            if arbre_temp2 = null then
                raise erreur_chemin;
            end if;
        else
            arbre_temp2 := arbre;
        end if;
        arbre_temp := An_Get_Pere(arbre_temp2);

        if (arbre_temp2 /= null) then
            if (Rep_Get_nom(An_Get_Valeur(arbre_temp2)) = "root" and Rep_Get_estFichier(An_Get_Valeur(arbre_temp2)) = false) then
                Put_Line("/");
            else
                while (An_Get_Pere(arbre_temp) /= null) loop
                    arbre_temp := An_Get_Pere(arbre_temp);
                    nbEspaces := nbEspaces + 3;
                end loop;
                if (Rep_Get_estFichier(An_Get_Valeur(arbre_temp2)) = false) then
                    for i in 1..nbEspaces loop
                        Put(" ");
                    end loop;
                    Put_Line(" \- " & Rep_Get_nom(An_Get_Valeur(arbre_temp2)));
                else
                    for i in 1..nbEspaces loop
                        Put(" ");
                    end loop;
                    Put_Line(" |- " & Rep_Get_nom(An_Get_Valeur(arbre_temp2)));
                end if;
            end if;
            if (An_Get_PremierFils(arbre_temp2) /= null) then
                afficher_tous_les_repertoires(An_Get_PremierFils(arbre_temp2), To_Unbounded_String(""), false);
            end if;
            if (An_Get_Frere(arbre_temp2) /= null) then
                afficher_tous_les_repertoires(An_Get_Frere(arbre_temp2), To_Unbounded_String(""), false);
            end if;
        end if;

    exception
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !") ; 
    end afficher_tous_les_repertoires;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : supprimer_repertoire
    ---- sémantique : permet de supprimer un répertoire à la destination 'destination_repertoire' (commande rm ou rm -r)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire saisie ; estFichier : le nouveau répertoire est un fichier ou non (=dossier)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure supprimer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; estFichier : boolean) is
    arbre_pere : arb_nr;
    verification_arbre : arb_nr; 
    nom_repertoire : Unbounded_String;
    position_repertoire : Integer;
    taille_repertoire : Integer;
    begin

        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            position_repertoire := rechercher_par_nom(arbre_pere, nom_repertoire, estFichier);
            if position_repertoire /= 0 then
                if (estFichier = true) then
                    modifier_taille_fichier(arbre, destination_repertoire, 0);
                    An_Supprimer_Fils(arbre_pere, position_repertoire);
                    put_line("Le fichier "& nom_repertoire &" a ete supprime avec succes");
                else
                    verification_arbre := arbre;
                    while (An_Get_Pere(verification_arbre) /= null) loop
                        if verification_arbre = An_Fils(arbre_pere, position_repertoire) then
                            raise erreur_suppression_dossier;
                        end if;
                        verification_arbre := An_Get_Pere(verification_arbre);
                    end loop;
                    taille_repertoire := An_Fils(arbre_pere, position_repertoire).val.taille;
                    verification_arbre := arbre_pere;
                    verification_arbre.all.val.taille := verification_arbre.all.val.taille - taille_repertoire;
                    while An_Get_Pere(verification_arbre) /= null loop
                        verification_arbre := An_Get_Pere(verification_arbre);
                        verification_arbre.all.val.taille := verification_arbre.all.val.taille - taille_repertoire;
                    end loop;
                    An_Supprimer_Fils(arbre_pere, position_repertoire);
                    put_line("Le dossier "& nom_repertoire &" a ete supprime avec succes");
                end if;
            else
                if (estFichier = true) then
                    raise fichier_non_existant;
                else
                    raise dossier_non_existant;
                end if;
            end if;
        else
            raise erreur_chemin;
        end if;

    exception
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !"); 
        when fichier_non_existant => put_line("Le fichier "& nom_repertoire &" a supprimer n'existe pas dans le dossier " & Rep_Get_nom(An_Get_Valeur(arbre_pere)));
        when dossier_non_existant => put_line("Le dossier "& nom_repertoire &" a supprimer n'existe pas dans le dossier " & Rep_Get_nom(An_Get_Valeur(arbre_pere)));
        when erreur_suppression_dossier => put_line("Erreur : Impossible de supprimer un dossier dans lequel vous etes actuellement");
    end supprimer_repertoire;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : deplacement_copie_fichier
    ---- sémantique : permet de copier ou déplacer un fichier de la destination 'destination_source' à la destination 'destination cible' (commande cp ou mv)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_source unbounded_String : destination du répertoire source ; destination_cible unbounded_String : destination du répertoire cible ; déplacement Boolean (est-ce un déplacement ou une copie)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure deplacement_copie_fichier(arbre : arb_nr; destination_source : unbounded_String; destination_cible : unbounded_String; deplacement : boolean) is
    arbre_source : arb_nr;
    arbre_cible : arb_nr; 
    nom_repertoire : unbounded_String;
    position_repertoire : Integer;
    un_repertoire : T_Repertoire;
    arbre_nouveau_repertoire : arb_nr;
    taille_repertoire : Integer;
    slash_present : boolean;
    indice : integer;
    begin

        indice := 1;
        slash_present := false;

        while (indice <= Length(destination_cible)) loop
            if (Element(destination_cible, indice) = '/') then
                slash_present := true;
                exit;
            end if;
            -- incrémenter l'indice pour faire la vérification au prochain caractère
            indice := indice + 1;
        end loop;
        arbre_source := verifier_destination(arbre, destination_source, true);
        if arbre_source /= null then
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_source);
            position_repertoire := rechercher_par_nom(arbre_source, nom_repertoire, true);
            if position_repertoire /= 0 then
                arbre_cible := verifier_destination(arbre, destination_cible, false);
                if arbre_cible = null and slash_present = true then
                    arbre_cible := verifier_destination(arbre, destination_cible, true);
                    nom_repertoire := recuperer_nom_repertoire(arbre, destination_cible);
                end if;
                if arbre_cible /= null then
                    if (rechercher_par_nom(arbre_cible, nom_repertoire, true) = 0) then
                        if deplacement = false or (deplacement = true and taille_actuel + arbre_source.all.val.taille < 1000000000) then
                            un_repertoire := (nom_repertoire, 0, Rep_Get_Droits(An_Get_Valeur(arbre_source)), true);
                            taille_repertoire := Rep_Get_Taille(An_Get_Valeur(An_fils(arbre_source, position_repertoire)));
                            if deplacement = true then
                                modifier_taille_fichier(arbre, destination_source, 0);
                                An_Supprimer_Fils(arbre_source, position_repertoire);
                            end if;
                            arbre_nouveau_repertoire := new noeud'(un_repertoire, null, null, arbre_cible);
                            An_Inserer_Fils(arbre_cible, arbre_nouveau_repertoire);
                            modifier_taille_fichier(arbre_cible, nom_repertoire, taille_repertoire);
                            if deplacement = true then
                                put_line("Le fichier "& nom_repertoire &" a ete deplace avec succes dans le dossier " & Rep_Get_Nom(An_Get_Valeur(arbre_cible)));
                            else
                                put_line("Le fichier "& nom_repertoire &" a ete cree via copie avec succes dans le dossier " & Rep_Get_Nom(An_Get_Valeur(arbre_cible)));
                            end if;
                        else
                            raise erreur_taille;
                        end if;
                    else
                        raise fichier_deja_existant;
                    end if;
                else
                    nom_repertoire := destination_cible;
                    raise erreur_chemin;
                end if;
            else
                raise fichier_non_existant;
            end if;
        else
            nom_repertoire := destination_source;
            raise erreur_chemin;
        end if;
    exception
        when erreur_chemin => put_line("Erreur : Le chemin '" & nom_repertoire & "' n'existe pas !");
        when erreur_taille => put_line("Erreur : Impossible d'executer la commande, plus de place sur le disque dur"); 
        when fichier_deja_existant => put_line("Le fichier " & nom_repertoire &" existe deja dans le repertoire " & Rep_Get_Nom(An_Get_Valeur(arbre_cible)));
        when fichier_non_existant => put_line("OOOOOOOOOOOOOOLe fichier " & nom_repertoire & " n'existe pas dans le dossier " & Rep_Get_Nom(An_Get_Valeur(arbre_source)));
    end deplacement_copie_fichier;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : archiver_dossier
    ---- sémantique : permet d'archiver des dossiers et donc de créer un fichier compressé de ce dossier (commande tar)
    ---- parametres : arbre arb_nr : arbre de T_Repertoire (repertoire courant) ; destination_repertoire unbounded_String : destination du répertoire
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure archiver_dossier(arbre : arb_nr; destination_repertoire : unbounded_string) is
    arbre_pere : arb_nr;
    nom_repertoire : Unbounded_String;
    un_repertoire : T_Repertoire;
    position_repertoire_fichier : Integer;
    position_repertoire_dossier : Integer;
    arbre_dossier : arb_nr;
    arbre_nouveau_fichier : arb_nr;
    begin
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            nom_repertoire := recuperer_nom_repertoire(arbre_pere, destination_repertoire);
            position_repertoire_dossier := rechercher_par_nom(arbre_pere, nom_repertoire, false);
            if position_repertoire_dossier /= 0 then
                Set_Unbounded_String(nom_repertoire, to_String(nom_repertoire) & ".zip");
                position_repertoire_fichier := rechercher_par_nom(arbre_pere, nom_repertoire, true);
                if position_repertoire_fichier = 0 then
                    arbre_dossier := An_Fils(arbre_pere, position_repertoire_dossier);
                    if taille_actuel + arbre_dossier.all.val.taille < 1000000000 then                    
                        un_repertoire := (nom_repertoire, 0, arbre_dossier.all.val.droits, true);
                        arbre_nouveau_fichier := new noeud'(un_repertoire, null, null, arbre_pere);
                        An_Inserer_Fils(arbre_pere, arbre_nouveau_fichier);
                        modifier_taille_fichier(arbre_pere, nom_repertoire, arbre_dossier.all.val.taille);
                    else
                        raise erreur_taille;
                    end if;
                else
                    raise fichier_deja_existant;
                end if;
            else
                raise dossier_non_existant;
            end if;
        else
            raise erreur_chemin;
        end if;

    exception
        when erreur_chemin => put_line("Erreur : Le chemin '" & destination_repertoire & "' n'existe pas !");
        when erreur_taille => put_line("Erreur : Impossible d'executer la commande, plus de place sur le disque dur"); 
        when fichier_deja_existant => put_line("Le fichier " & nom_repertoire &" existe deja dans le repertoire");
        when dossier_non_existant => put_line("Le dossier " & nom_repertoire & " n'existe pas dans le dossier");
    end;
    
end sgf;