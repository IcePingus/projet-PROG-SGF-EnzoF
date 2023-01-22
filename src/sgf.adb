---------------------------------------------------------------------------------------------
-- Nom du fichier : sgf.adb
-- Fonction       : Corps d'un composant de Système de Gestion de Fichiers (sgf)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

with text_io;use text_io;
package body sgf is

    function rechercher_par_nom(arbre : arb_nr; nom_repertoire : unbounded_String; est_fichier : boolean) return Integer is
        arbreTemp : arb_nr;
        compteur : Integer := 1;
    begin
        arbreTemp := arbre;
        -- vérifier si l'arbre a au moins un fils
        if (An_Get_PremierFils(arbreTemp) /= null) then
            arbreTemp := An_Get_PremierFils(arbreTemp);
            -- vérifier si le nom du répertoire est le même que le nom du répertoire du premier fils
            if (Rep_Get_Nom(An_Get_Valeur(arbreTemp)) = nom_repertoire and Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = est_fichier) then
                return 1;
            else
                -- boucle sur les frères et vérifier si le nom du répertoire est le même que celui d'un de ses frères
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

    procedure initialisation_sgf(un_arbre : OUT arb_nr) is
    un_repertoire : T_Repertoire;
   
    begin
        taille_actuel := 0;
        un_repertoire := (To_Unbounded_String("root"), 0, 777, false);
        un_arbre := new noeud'(un_repertoire, null, null, null);
    end initialisation_sgf;

    procedure repertoire_courant(arbre : arb_nr) is
    pwd : unbounded_string;
    arbre_temp : arb_nr;
    begin
        arbre_temp := arbre;
        pwd := Rep_Get_Nom(An_Get_Valeur(arbre_temp));
        -- afficher les noms des répertoires père et concaténer en délimitant par un "/"
        while (An_Get_Pere(arbre_temp) /= null) loop
            arbre_temp := An_Get_Pere(arbre_temp);
            -- concaténation
            Set_Unbounded_String(pwd, to_String(Rep_Get_Nom(An_Get_Valeur(arbre_temp))) & "/" & to_String(pwd));
        end loop;
        -- concaténation
        Set_Unbounded_String(pwd, "/" & to_String(pwd));
        put_line(pwd);
    end repertoire_courant;

    procedure creer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_string; taille : Natural; droits : Natural; estFichier : boolean) is
    
    arbre_pere : arb_nr;
    un_repertoire : T_Repertoire;
    arbre_nouveau_repertoire : arb_nr;
    nom_repertoire : Unbounded_String;

    begin
        -- vérifier si la destination saisie par l'utilisateur existe
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            -- récupérer le nom du répertoire saisi par l'utilisateur
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            -- vérifier que le répertoire nom_repertoire n'existe pas dans la destination souhaité par l'utilisateur
            if (rechercher_par_nom(arbre_pere, nom_repertoire, estFichier) = 0) then
                -- vérifier si l'ajout du repertoire n'aura pas d'incidence sur la taille mémoire maximale
                if taille_actuel + taille < 1000000000 then
                    -- création du répertoire et ajout de l'arbre fils à la bonne destination
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
        -- vérifier que la taille est positive ou nulle
        if taille < 0 then
            raise taille_negative;
        end if;
        -- vérifier la destination
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            -- récupérer nom du répertoire de destination_repertoire
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            -- vérifier si le répertoire existe, si oui récupérer sa position
            position_repertoire := rechercher_par_nom(arbre_pere, nom_repertoire, true);
            if position_repertoire /= 0 then
                -- stocker l'ancienne taille, changer la taille de la mémoire disque
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

    procedure changer_direction_repertoire(arbre : IN OUT arb_nr; destination_repertoire : unbounded_String) is
    arbre_temp : arb_nr;

    begin
        -- vérifier la destination
        arbre_temp := verifier_destination(arbre, destination_repertoire, false);

        if arbre_temp /= null then
            -- changer la valeur de arbre par l'arbre avec la bonne destination
            arbre := arbre_temp;
        else
            raise erreur_chemin;
        end if;
    exception
        when erreur_chemin => put_line("Erreur : Le chemin specifie n'existe pas !") ; 
    end changer_direction_repertoire;

    procedure afficher_contenu_repertoire(arbre : arb_nr; destination_dossier : unbounded_String; repertoire_courant : boolean) is
    arbreTemp : arb_nr;
    mise_en_forme : Natural;    
    arbre_temp : arb_nr;

    begin
        -- si il ne s'agit pas du répertoire courant et que la destination_repertoire a été renseigné et n'est pas vide alors arbre à afficher et différent que arbre actuel sinon afficher arbre
        if repertoire_courant = false and destination_dossier /= "" then
            arbre_temp := verifier_destination(arbre, destination_dossier, false);
        else
            arbre_temp := arbre;
        end if;

        -- affichage du premier fils
        if arbre_temp /= null then
            if (An_Get_PremierFils(arbre_temp) /= null) then      
                put_line("Name                    Length           Rights          File");
                put_line("===============================================================");
                arbreTemp := An_Get_PremierFils(arbre_temp);
                -- mise_en_forme permet de mettre en forme pour que l'affichage se fasse en colonne indépendamment du nom du répertoire
                mise_en_forme := 25 - length(Rep_Get_Nom(An_Get_Valeur(arbreTemp)));
                put(Rep_Get_Nom(An_Get_Valeur(arbreTemp))); put(mise_en_forme *" "); put(Integer'image(Rep_Get_Taille(An_Get_Valeur(arbreTemp)))); put("             "); put(Integer'image(Rep_Get_Droits(An_Get_Valeur(arbreTemp))));put("           ");
                if(Rep_Get_estFichier(An_Get_Valeur(arbreTemp)) = true) then
                    put("Fichier");
                else
                    put("Dossier");
                end if;
                -- affichage de tous les frères du premier fils
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

    procedure supprimer_repertoire(arbre : arb_nr; destination_repertoire : unbounded_String; estFichier : boolean) is
    arbre_pere : arb_nr;
    verification_arbre : arb_nr; 
    nom_repertoire : Unbounded_String;
    position_repertoire : Integer;
    taille_repertoire : Integer;
    begin

        -- vérifier la destination
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            -- récupérer le nom_repertoire de destination_repertoire
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_repertoire);
            -- vérifier si le répertoire de nom nom_repertoire existe et si oui retourner sa position
            position_repertoire := rechercher_par_nom(arbre_pere, nom_repertoire, estFichier);
            if position_repertoire /= 0 then
                -- vérifier si le repertoire à supprimer et un fichier ou non
                if (estFichier = true) then
                    -- appel de la procédure pour modifier la taille à 0 puis supprimer le fichier
                    modifier_taille_fichier(arbre, destination_repertoire, 0);
                    An_Supprimer_Fils(arbre_pere, position_repertoire);
                    put_line("Le fichier "& nom_repertoire &" a ete supprime avec succes");
                else
                    -- verification_arbre permet de vérifier si l'arbre actuel où se trouve l'utilisateur n'est pas contenu dans l'arbre que l'on cherche à supprimer (si oui => lève l'exception : erreur_suppression_dossier)
                    verification_arbre := arbre;
                    while (An_Get_Pere(verification_arbre) /= null) loop
                        if verification_arbre = An_Fils(arbre_pere, position_repertoire) then
                            raise erreur_suppression_dossier;
                        end if;
                        verification_arbre := An_Get_Pere(verification_arbre);
                    end loop;
                    -- récupérer la taille du dossier et modifier la taille du répertoire père qui le contient
                    taille_repertoire := An_Fils(arbre_pere, position_repertoire).val.taille;
                    verification_arbre := arbre_pere;
                    verification_arbre.all.val.taille := verification_arbre.all.val.taille - taille_repertoire;
                    while An_Get_Pere(verification_arbre) /= null loop
                        verification_arbre := An_Get_Pere(verification_arbre);
                        verification_arbre.all.val.taille := verification_arbre.all.val.taille - taille_repertoire;
                    end loop;
                    -- supprimer le dossier
                    An_Supprimer_Fils(arbre_pere, position_repertoire);
                    put_line("Le dossier "& nom_repertoire &" a ete supprime avec succes");
                end if;
            else
                -- exceptions si le repertoire n'existe pas
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

        -- vérifie si un caractère / est présent dans la destination cible, cela peut signifier que l'utilisateur pourrait vouloir renommer le fichier à déplacer ou copié
        while (indice <= Length(destination_cible)) loop
            if (Element(destination_cible, indice) = '/') then
                slash_present := true;
                exit;
            end if;
            -- incrémenter l'indice pour faire la vérification au prochain caractère
            indice := indice + 1;
        end loop;

        -- vérifier la destination_source
        arbre_source := verifier_destination(arbre, destination_source, true);
        if arbre_source /= null then
            -- récupérer le nom du répertoire de destination_source
            nom_repertoire := recuperer_nom_repertoire(arbre, destination_source);
            -- vérifier si le répertoire de nom nom_repertoire existe et si oui retourner sa position
            position_repertoire := rechercher_par_nom(arbre_source, nom_repertoire, true);
            if position_repertoire /= 0 then
                -- vérifier la destination de destination_cible
                arbre_cible := verifier_destination(arbre, destination_cible, false);
                -- si arbre_cible ne correspond à aucun arbre et qu'un '/' est présent il pourrait s'agir d'une volonté de renommer le fichier, il faut revérifier en supprimant le dernier argument de destination_cible
                if arbre_cible = null and slash_present = true then
                    arbre_cible := verifier_destination(arbre, destination_cible, true);
                    nom_repertoire := recuperer_nom_repertoire(arbre, destination_cible);
                end if;
                -- vérification que arbre_cible existe
                if arbre_cible /= null then
                    -- vérifier qu'un fichier de nom_repertoire n'existe pas dans la destination ou nous souhaitons le copier/déplacer
                    if (rechercher_par_nom(arbre_cible, nom_repertoire, true) = 0) then
                        -- vérification liée à l'espace mémoire maximale du disque
                        if deplacement = false or (deplacement = true and taille_actuel + arbre_source.all.val.taille < 1000000000) then
                            -- création du répertoire
                            un_repertoire := (nom_repertoire, 0, Rep_Get_Droits(An_Get_Valeur(arbre_source)), true);
                            taille_repertoire := Rep_Get_Taille(An_Get_Valeur(An_fils(arbre_source, position_repertoire)));
                            -- si c'est un déplacement supprimer le fichier avant de le créer
                            if deplacement = true then
                                modifier_taille_fichier(arbre, destination_source, 0);
                                An_Supprimer_Fils(arbre_source, position_repertoire);
                            end if;
                            -- création du répertoire dans la destination_cible et création de l'arbre
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
        when fichier_non_existant => put_line("Le fichier " & nom_repertoire & " n'existe pas dans le dossier " & Rep_Get_Nom(An_Get_Valeur(arbre_source)));
    end deplacement_copie_fichier;

    procedure archiver_dossier(arbre : arb_nr; destination_repertoire : unbounded_string) is
    arbre_pere : arb_nr;
    nom_repertoire : Unbounded_String;
    un_repertoire : T_Repertoire;
    position_repertoire_fichier : Integer;
    position_repertoire_dossier : Integer;
    arbre_dossier : arb_nr;
    arbre_nouveau_fichier : arb_nr;
    begin
        -- vérifier la destination
        arbre_pere := verifier_destination(arbre, destination_repertoire, true);
        if arbre_pere /= null then
            -- récupérer le nom du répertoire de destination_repertoire
            nom_repertoire := recuperer_nom_repertoire(arbre_pere, destination_repertoire);
            -- vérifier si 
            position_repertoire_dossier := rechercher_par_nom(arbre_pere, nom_repertoire, false);
            -- vérifier si le répertoire de nom nom_repertoire existe et si oui retourner sa position
            if position_repertoire_dossier /= 0 then
                -- concaténation
                Set_Unbounded_String(nom_repertoire, to_String(nom_repertoire) & ".zip");
                -- vérifier si le répertoire de nom nom_repertoire (dossier archivé) existe et si oui retourner sa position
                position_repertoire_fichier := rechercher_par_nom(arbre_pere, nom_repertoire, true);
                if position_repertoire_fichier = 0 then
                    -- récupérer l'arbre correspondant au dossier
                    arbre_dossier := An_Fils(arbre_pere, position_repertoire_dossier);
                    -- vérification liée à la mémoire du disque par rapport à la création de l'archive
                    if taille_actuel + arbre_dossier.all.val.taille < 1000000000 then
                        -- création de l'archive compressé et l'ajouter à un arbre   
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