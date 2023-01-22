---------------------------------------------------------------------------------------------
-- Nom du fichier : menu.adb
-- Fonction       : Vue correspondant au menu utilisateur (terminal)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

-- import du package sgf
with sgf; use sgf;
with repertoire; use repertoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-4-5.html
with ada.strings.unbounded;
use ada.strings.unbounded;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure menu is

    -- commande saisie par l'utilisateur
    commande_saisie : unbounded_string;
    indice_caractere : Integer;
    -- tableau de chaîne de caractères
    type tableau_unbouded is array (0..9) of unbounded_String;
    -- nombre paramètres pour le parser
    nombre_parametre : Integer;
    -- un tableau de chaîne de caractères
    parametres : tableau_unbouded;
    -- arbre représentant le répertoire courant
    arbre_actuel : P_arbre.arb_nr;
    taille_fichier : Integer;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : creer_sgf
    ---- sémantique : permet la création du sgf en saississant la commande start
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure creer_sgf is
        parametres_saisie : unbounded_String;
    begin
        while(parametres_saisie /= "start") loop
        Put_Line("Vous pouvez creer un SGF en ecrivant start :");
        get_line(parametres_saisie);
        if (parametres_saisie = "start") then
            -- initialisation du sgf
            initialisation_sgf(arbre_actuel);
        end if;
        end loop;
    end;

    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---- nom : help
    ---- sémantique : permet d'afficher la liste des commandes disponibles de l'invite de commande
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    procedure help is
    begin
        Put_Line("Voici les differentes commandes que vous pouvez ecrire :");
        New_Line;
        Put_Line("=> cd : |destination| : Changer de direction de repertoire");New_Line;
        Put_Line("=> cp |destination_source|[nom_fichier] [destination_cible] : Copier un fichier");New_Line;
        Put_Line("=> ls |destination| : Afficher la liste des fichiers et dossiers");New_Line;
        Put_Line("=> ls -r |destination| : Afficher tous les repertoires et tous les sous-repertoires");New_Line;
        Put_Line("=> mkdir |destination|[nom_dossier] : Creer un dossier");New_Line;
        Put_Line("=> mv |destination_source|[nom_fichier] [destination_cible] : Deplacer un fichier");New_Line;
        Put_Line("=> pwd : Afficher le repertoire courant");New_Line;
        Put_Line("=> quit : Quitter l'interface");New_Line;
        Put_Line("=> reset : Creer un SGF avec que le repertoire racine");New_Line;
        Put_Line("=> rm |destination|[nom_fichier] : Supprimer un fichier");New_Line;
        Put_Line("=> rm -r |destination|[nom_dossier] : Supprimer un dossier");New_Line;
        Put_Line("=> size |destination|[nom_fichier] [taille_fichier] : Changer la taille d'un fichier");New_Line;
        Put_Line("=> tar |destination|[nom_dossier] : Archiver un dossier");New_Line;
        Put_Line("=> touch |destination|[nom_fichier] : Creer un fichier");
        Put_Line("============================================================================");
        Put_Line("Legende : |param| => parametre facultatif [param] => parametre obligatoire");
    end;

begin
    creer_sgf;
    
    while (commande_saisie /= "quit") loop
        new_line;
        put("> ");
        -- récupération de la saisie clavier
        get_line(commande_saisie);
        -- PARSER de la saisie clavier pour délimiter les espaces
        -- boucle pour initialiser le tableau de unbouded string qui permettra de récupérer les chaînes de caractères entre les espaces
        for i in 0..9 loop
            parametres(i) := To_Unbounded_String("");
        end loop;

        -- indice_caractere fait référence à l'indice du caractère que nous parcourons pour vérifier s'il y a un espace
        -- nombre_parametres récupère le nombre de paramètres après avoir délimiter les espaces, on commaence au parametre 0
        indice_caractere := 1;
        nombre_parametre := 0;

        -- boucle permettant de délimiter les espaces
        while (indice_caractere <= Length(commande_saisie)) loop
            if (Element(commande_saisie, indice_caractere) = ' ') then
                -- si un espace est trouvé le nombre de paramètres est incrémenté
                nombre_parametre := nombre_parametre + 1;
            else
                -- sinon on ajoute la lettre à la unbounded string placé à l'indice paramètres(nombre_parametres)
                parametres(nombre_parametre) := parametres(nombre_parametre) & Element(commande_saisie, indice_caractere);
            end if;
            -- incrémenter l'indice pour faire la vérification au prochain caractère
            indice_caractere := indice_caractere + 1;
        end loop;

        -- vérifier le premier paramètres (le premier argument de la commande saisie par l'utilisateur)
        -- vérifier à chaque fois si le nombre de paramètres est cohérent par rapport à la commande écrite sinon signaler à l'utilisateur comment mieux écrire la commande
        -- si la commande est correcte : appel à des fonctions du contrôleur SGF permettant de faire les actions souhaitées par l'utilisateur
        if parametres(0) = "reset" then
            if (nombre_parametre = 0) then
                initialisation_sgf(arbre_actuel);
                put_line("Reinitialisation du sgf avec succes");
            else
                put_line("Nombre de parametres incorrect, la commande correcte est 'reset'");
            end if;
        elsif parametres(0) = "pwd" then
            if (nombre_parametre = 0) then
                repertoire_courant(arbre_actuel);
            else
                put_line("Nombre de parametres incorrect, la commande correcte est 'pwd'");
            end if;
        elsif parametres(0) = "touch" then
            if (nombre_parametre = 1 and parametres(1) /= "") then
                creer_repertoire(arbre_actuel, parametres(1), 5, 777, true);
            else
                put_line("Nombre de parametres incorrect, la commande correcte est 'touch |destination|[nom_fichier]'");
            end if;
        elsif parametres(0) = "size" then
            if (nombre_parametre = 2 and parametres(1) /= "" and parametres(2) /= "") then
                if Integer'Value (To_String(parametres(2))) < 5 then
                    put_line("La taille d'un fichier doit etre minimum de 5");
                else
                    taille_fichier := Integer'Value (To_String (parametres(2)));
                    modifier_taille_fichier(arbre_actuel, parametres(1), taille_fichier);
                end if;
            else
                put_line("Nombre de parametres incorrect, la commande correcte est 'size |destination|[nom_fichier] [taille_fichier]'");
            end if;
        elsif parametres(0) = "mkdir" then
            if (nombre_parametre = 1 and parametres(1) /= "") then
                creer_repertoire(arbre_actuel, parametres(1), 0, 777, false);
            else
                put_line("Nombre de parametres incorrect, la commande correcte est 'mkdir |destination|[nom_dossier]'");
            end if;
        elsif parametres(0) = "cd" then
            if (nombre_parametre = 1 and parametres(1) /= "") then
                changer_direction_repertoire(arbre_actuel, parametres(1));
            elsif (nombre_parametre > 1) then
                put_line("Nombre de parametres incorrect, la commande correcte est 'cd [destination_repertoire]'");
            end if;
        elsif parametres(0) = "ls"  then
            if (nombre_parametre = 0) then
                afficher_contenu_repertoire(arbre_actuel, parametres(0), true);
            elsif (nombre_parametre = 1 and parametres(1) /= "-r") then
                afficher_contenu_repertoire(arbre_actuel,  parametres(1), false);
            elsif (nombre_parametre = 1 and parametres(1) = "-r") then
                afficher_tous_les_repertoires(arbre_actuel, To_Unbounded_String(""), false);
            elsif (nombre_parametre = 2 and parametres(1) = "-r") then
                afficher_tous_les_repertoires(arbre_actuel, parametres(2), true);
            else
                put_line("Commande incorrecte, la commande correcte est 'ls |destination|' ou 'ls -r |destination|'");
            end if;
        elsif parametres(0) = "rm" then
            if (nombre_parametre = 1 and parametres(1) /= "" and parametres(1) /= "-r") then
                supprimer_repertoire(arbre_actuel, parametres(1), true);
            elsif (nombre_parametre = 2 and parametres(1) = "-r" and parametres(2) /= "") then
                supprimer_repertoire(arbre_actuel, parametres(2), false);
            else
                put_line("Commande incorrecte, la commande correcte est 'rm |destination|[nom_fichier]' ou 'rm -r |destination|[nom_dossier]'");
            end if;
        elsif parametres(0) = "mv" then
            if (nombre_parametre = 2 and parametres(1) /= "" and parametres(2) /= "") then
                deplacement_copie_fichier(arbre_actuel, parametres(1), parametres(2), true);
            else 
                put_line("Commande incorrecte, la commande correcte est 'mv |destination_source|[nom_fichier] [destination_cible]'");
            end if;
        elsif parametres(0) = "cp" then
            if (nombre_parametre = 2 and parametres(1) /= "" and parametres(2) /= "") then
                deplacement_copie_fichier(arbre_actuel, parametres(1), parametres(2), false);
            else 
                put_line("Commande incorrecte, la commande correcte est 'cp |destination_source|[nom_fichier] [destination_cible]'");
            end if;
        elsif parametres(0) = "tar" then
            if (nombre_parametre = 1 and parametres(1) /= "") then
                archiver_dossier(arbre_actuel, parametres(1));
            else 
                put_line("Commande incorrecte, la commande correcte est 'tar [destination_dossier]'");
            end if;
        elsif parametres(0) = "help" then
            if (nombre_parametre = 0) then
                help;
            else
                put_line("Commande incorrecte, la commande correcte est 'help'");
            end if;
        elsif parametres(0) = "quit" then
            if (nombre_parametre = 0) then
                put_line("Aurevoir :( ...");
            else
                put_line("Commande incorrecte, la commande correcte est 'quit'");
            end if;
        else
            put_line("La commande '" & commande_saisie &"' n'est pas connu de notre systeme de gestion de fichiers, tapez la commande 'help' pour avoir la liste des commandes existantes");
        end if;
    end loop;
end menu;