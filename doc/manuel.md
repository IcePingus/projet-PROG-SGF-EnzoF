
# Manuel Utilisateur

Le **manuel utilisateur** décrit comment utiliser les programmes développés illustrés avec des copies d’écran.

## Pré-requis

 - Télécharger [GNAT](https://www.adacore.com/download), un compilateur ADA
 - Ajouter GNAT dans les variables d'environnement si ce n'est pas fait automatiquemennt
 - Cloner ce dépôt [Git](https://github.com/IcePingus/projet-PROG-SGF-EnzoF)

Maintenant que toutes ces étapes sont faites, vous avez de quoi compiler correctement le programme.

## Compilation

Ouvrir un **terminal de commandes** et se déplacer (cd) dans l'onglet src du projet git cloné puis exécuter la commande suivante :
> gnatmake .\menu.adb

Le programme a **compilé** correctement !
Pour **exécuter le programme** : il ne reste plus qu'à lancer la commande suivante :
> .\menu

## Utilisation programme

### Démarrage

Au démarrage du programme s'affiche le message suivant :
> Vous pouvez creer un SGF en ecrivant start

Cela signifie qu'il faut créer taper la commande 'start' afin de **créer un SGF** avec **un** dossier racine 'root'.
Tant que vous n'avez pas taper la commande **start**, l'interface vous redemandera d'écrire start.

Il sera possible de **réinitialiser** le SGF par la suite grâce à l'instruction 'reset' qui vous permettra de remplacer le SGF actuel par un nouveau SGF avec seulement le dossier racine 'root'.

### Fonctionnement SGF

Il faut savoir que le SGF possède **un répertoire courant** des répertoires (fichier ou dossier) un dossier peut contenir d'autres fichiers et d'autres dossiers.

Une **destination (chemin)** est un chemin d'accès à un répertoire et est délimitée par des '/' entre chaque répertoire et il peut être représenté de 2 manières distinctes :
- **Chemin absolu** : Le chemin absolu débute par un '/' et part du répertoire racine (exemple : *'/home/enzo/ada/tp'*).
- **Chemin relatif** : Le chemin relatif part du répertoire actuel du SGF (exemple : *'enzo/ada/tp'* avec comme répertoire actuel *'home'*).

De plus dans un chemin, le caractère '.' représente le **répertoire actuel** et '..' représente le **répertoire père** d'un répertoire.
> Aide : Si une incompréhension subsiste liée au fonctionnement des SGF accéder au [sujet de projet](https://moodle-n7.inp-toulouse.fr/pluginfile.php/98162/mod_resource/content/4/Project_PIM_SGF.pdf) qui l'explicite de manière plus détaillée.

### Liste des commandes :

- #### Accéder à son répertoire actuel (pwd)

  Cette commande permet **d'accéder au répertoire courant** du sgf.
  Elle s'utilise avec le mot-clé **pwd** et sans aucun paramètre.


  *Exemple :*
  ![Exemple commande pwd](https://zupimages.net/up/23/03/sia2.png)

- #### Déplacement répertoire actuel (cd)

  Cette commande permet de **changer le répertoire courant** au SGF à l'aide d'une destination.
  Elle s'utilise avec le mot-clé **cd** et avec **1** paramètre : destination_dossier qui représente un   chemin absolu ou relatif.
  
  *Exemple :*
  ![Exemple commande cd](https://zupimages.net/up/23/03/mcgg.png)
    
    **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
	Si le chemin n'existe pas, le programme affiche un message d'erreur
	
  *Exemple :*
  ![Exemple exception cd](https://zupimages.net/up/23/03/zfs7.png)

- ### Création d'un fichier (touch)

  Cette commande permet de **créer un fichier** dans une destination du SGF.
  Elle s'utilise avec le mot-clé **touch** et avec **1** paramètre : nom du fichier à créer (vous pouvez spécifier une destination précise où créer le fichier en mettant la destination où vous souhaiter le créer avant le nom du fichier délimité par un '/').

  *Exemple :*
![Exemple commande touch](https://zupimages.net/up/23/03/ktbb.png)
   
    **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
    Si un fichier du même nom existe déjà dans le répertoire où nous voulons le créer
	Si le chemin n'existe pas, le programme affiche un message d'erreur
	
  *Exemple :*
![Exemple exception touch](https://zupimages.net/up/23/03/kix7.png)
- ### Création d'un dossier (mkdir)

  Cette commande permet de **créer un dossier** dans une destination du SGF.
  Elle s'utilise avec le mot-clé **mkdir** et avec **1** paramètre : nom du dossier à créer (vous pouvez spécifier une destination précise où créer le fichier en mettant la destination où vous souhaiter le créer avant le nom du fichier délimité par un '/').

  *Exemple :*
![Exemple commande mkdir](https://zupimages.net/up/23/03/z16m.png)
    **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
    Si un dossier du même nom existe déjà dans le répertoire où nous voulons le créer
	Si le chemin n'existe pas, le programme affiche un message d'erreur
	
  *Exemple :*
![Exemple exception mkdir](https://zupimages.net/up/23/03/n52z.png)

- ### Afficher les répertoires contenus dans un répertoire (ls)

  Cette commande permet de **lister les répertoires** contenus dans un répertoire du SGF.
  Elle s'utilise avec le mot-clé **ls** et avec **0** ou **1** paramètre facultatif : nom du répertoire où l'on cherche à lister les répertoires, sinon affiche les répertoires contenus dans le répertoire courant.
  
  *Exemple :*
![Exemple commande ls](https://zupimages.net/up/23/03/wpx0.png)

  **Cas d'erreur :**
    Si le nombre de paramètres est supérieur à 1, le programme affiche un message d'erreur
	Si le chemin n'existe pas, le programme affiche un message d'erreur

  *Exemple :*
![Exemple exception ls](https://zupimages.net/up/23/03/p0k4.png)

- ### Afficher les répertoires et sous-répertoires dans un répertoire (ls -r)

  Cette commande permet de **lister les répertoires et sous répertoires** contenus dans un SGF.
  Elle s'utilise avec le mot-clé **ls -r** et avec **0** ou **1** paramètre facultatif : nom du répertoire où l'on cherche à lister les répertoires et sous répertoires, sinon affiche les répertoires et sous répertoires contenus dans le répertoire courant.
  
  *Exemple :*
![Exemple commande ls -r](https://zupimages.net/up/23/03/ooe3.png)

  **Cas d'erreur :**
    Si le nombre de paramètres est supérieur à 1, le programme affiche un message d'erreur
	Si le chemin n'existe pas, le programme affiche un message d'erreur

	*Exemple :*
![Exemple exception ls -r](https://zupimages.net/up/23/03/5bjx.png)

- ### Supprimer un fichier (rm)

  Cette commande permet de **supprimer un fichier** dans une destination du SGF.
  Elle s'utilise avec le mot-clé **rm** et avec **1** paramètre : nom du fichier à supprimer (vous pouvez spécifier une destination précise où supprimer le fichier en mettant la destination où vous souhaiter le supprimer avant le nom du fichier délimité par un '/').
  
  *Exemple :*
![Exemple commande rm](https://zupimages.net/up/23/03/7rsl.png)

  **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
    Si le fichier n'existe pas dans le répertoire où nous voulons le supprimer
	Si le chemin n'existe pas, le programme affiche un message d'erreur

  *Exemple :*
![Exemple exception rm](https://zupimages.net/up/23/03/ik86.png)

- ### Supprimer un dossier (rm -r)

  Cette commande permet de **supprimer un dossier** dans une destination du SGF.
  Elle s'utilise avec le mot-clé **rm -r** et avec **1** paramètre : nom du dossier à supprimer (vous pouvez spécifier une destination précise où supprimer le dossier en mettant la destination où vous souhaiter le supprimer avant le nom du dossier délimité par un '/').
  
  *Exemple :*
![Exemple commande rm -r](https://zupimages.net/up/23/03/9dty.png)

  **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
    Si le dossier n'existe pas dans le répertoire où nous voulons le supprimer
    Si on cherche à supprimer un dossier dans lequel le répertoire courant du SGF est contenu dans le répertoire à supprimer
	Si le chemin n'existe pas, le programme affiche un message d'erreur

  *Exemple :*
![Exemple exception rm -r](https://zupimages.net/up/23/03/r2i4.png)

- ### Changer la taille d'un fichier (size)

  Cette commande permet de **changer la taille d'un fichier**
  Elle s'utilise avec le mot-clé **size** et avec **2** paramètre : **nom du fichier** à changer de taille (vous pouvez spécifier une destination précise où créer le fichier en mettant la destination où vous souhaiter changer la taille d'un fichier avant le nom du fichier délimité par un '/ ') et la **nouvelle taille** avec un entier.

  *Exemple :*
![Exemple commande size](https://zupimages.net/up/23/03/j7oi.png)

  **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 2, le programme affiche un message d'erreur
    Si le fichier n'existe pas dans le répertoire où nous voulons changer sa taille
	Si le chemin n'existe pas, le programme affiche un message d'erreur
	Si la taille est un entier négatif
  
  *Exemple :*
  ![Exemple excepption size](https://zupimages.net/up/23/03/vx9c.png)

- ### Copier un fichier dans un autre répertoire (cp)
 
 Cette commande permet de **copier un fichier** dans un autre répertoire.
  Elle s'utilise avec le mot-clé **cp** et avec **2** paramètre : **nom du fichier** à copier (vous pouvez spécifier une destination précise où copier le dossier en mettant la destination où vous souhaiter le copier avant le nom du dossier délimité par un '/') ainsi que la **destination** où coller le fichier.
Il est éventuellement possible de **renommer le fichier copié** quand nous allons le coller en spécifiant un nouveau nom de fichier lors de la destination où coller le fichier en ajoutant un '/' et en écrivant le nom du nouveau fichier à la suite de celui-ci.

  *Exemple :*
  ![Exemple commande cp](https://zupimages.net/up/23/03/4veb.png)
  
  **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 2, le programme affiche un message d'erreur-
    Si le fichier à copier n'existe pas
	Si l'un des chemin n'existe pas, le programme affiche un message d'erreur
  
*Exemple :*
![Exemple exception cp](https://zupimages.net/up/23/03/865v.png)

- ### Déplacer un fichier dans un autre répertoire (mv)
 
 Cette commande permet de **déplacer un fichier** dans un autre répertoire.
  Elle s'utilise avec le mot-clé **mv** et avec **2** paramètre : **nom du fichier** à déplacer (vous pouvez spécifier une destination précise où déplacer le dossier en mettant la destination où vous souhaiter le déplacer avant le nom du dossier délimité par un '/') ainsi que la **destination** où coller le fichier.
Il est éventuellement possible de **renommer le fichier** quand nous allons le déplacer en spécifiant un nouveau nom de fichier lors de la destination où déplacer le fichier en ajoutant un '/' et en écrivant le nom du nouveau fichier à la suite de celui-ci (il est possible de renommer un fichier sans le déplacer en spécifiant la même destination).

  *Exemple :*
  ![Exemple commande mv](https://zupimages.net/up/23/03/5v7g.png)
  
  **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 2, le programme affiche un message d'erreur-
    Si le fichier à copier n'existe pas
	Si l'un des chemin n'existe pas, le programme affiche un message d'erreur
  
*Exemple :*
![Exemple exception mv](https://zupimages.net/up/23/03/nwyt.png)
 

- ### Archiver un dossier en un fichier (tar)

  Cette commande permet **d'archiver un dossier** (créer un fichier de même taille dans le répertoire père.
  Elle s'utilise avec le mot-clé **tar** et avec **1** paramètre : nom du dossier à archiver (vous pouvez spécifier une destination précise où archiver le dossier en mettant la destination où vous souhaiter l'archiver avant le nom du dossier délimité par un '/').

  *Exemple :*
![Exemple commande tar](https://zupimages.net/up/23/03/yfdm.png)

    **Cas d'erreur :**
    Si le nombre de paramètres n'est pas de 1, le programme affiche un message d'erreur
    Si le dossier n'existe déjà dans le répertoire où nous voulons l'archiver
	Si le chemin n'existe pas, le programme affiche un message d'erreur

  *Exemple :*
  ![Exemple exception tar](https://zupimages.net/up/23/03/7lr4.png)

- ### Réinitialiser le SGF (reset)

  Cette commande permet de **réinitialiser le SGF** (remplacer le SGF actuel par un nouveau contenant qu'un répertoire racine).
  Elle s'utilise avec le mot-clé **reset** et sans aucun paramètre.

  *Exemple :*
![Exemple commande reset](https://zupimages.net/up/23/03/54ms.png)

- ### Accéder à une page d'aide (help)

  Cette commande permet d'avoir **la liste des commandes existantes** du SGF.
  Elle s'utilise avec le mot-clé **help** et sans aucun paramètre.

  *Exemple :*
![Exemple commande help](https://zupimages.net/up/23/03/dipj.png)

- ### Quitter le SGF (quit)

  Cette commande permet de **quitter le SGF**.
  Elle s'utilise avec le mot-clé **quit** et sans aucun paramètre.

  *Exemple :*
![Exemple commande quit](https://zupimages.net/up/23/03/bm06.png)


## Informations importantes

- A la création d'un fichier, sa taille est de **5 Octects**
- La taille du disque est de **1 Méga-Octects**, à partir de cette limite atteinte il vous sera **impossible** de créer un nouveau répertoire, copier un nouveau fichier ou d'augmenter la taille d'un fichier (des **exceptions** peuvent se produire dans ces cas).
- Si une commande du SGF est mal tapée, il vous demandera de la réécrire et il conseillera la commande help pour avoir davantage d'informations sur les commandes existantes.