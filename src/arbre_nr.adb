---------------------------------------------------------------------------------------------
-- Nom du fichier : arbre_nr.adb
-- Fonction       : Corps d'un composant de manipulation des arbres à n branches (arbre nr)
-- Auteur         : Furriel Enzo
---------------------------------------------------------------------------------------------

with text_io;use text_io;
package body arbre_nr is

   function An_Get_PremierFils(arbre : in arb_nr) return arb_nr is
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      else 
         if arbre.premier_fils /= null then
            return arbre.premier_fils;
         else
            return null;
         end if;
      end if;
   end;

   function An_Get_Pere(arbre : in arb_nr) return arb_nr is
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      else 
         if arbre.pere /= null then
            return arbre.pere;
         else
            return null;
         end if;
      end if;
   end;

   function An_Get_Frere(arbre : in arb_nr) return arb_nr is
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
         else 
         if arbre.frere /= null then
            return arbre.frere;
         else
            return null;
         end if;
      end if;
   end;

   function An_Get_Valeur(arbre : in arb_nr) return T is
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      else
         return arbre.val;
      end if;
   end;
            
   function An_Vide(arbre : in arb_nr) return boolean is
   begin

      return (arbre = null);

   end An_Vide;

   function An_Fils(arbre : in arb_nr; numero_fils : in integer) return arb_nr is
   arbre_courant : arb_nr;

   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      else 
         arbre_courant := arbre.premier_fils;     
         for i in 1..numero_fils-1 loop
            arbre_courant := arbre_courant.frere;
         end loop;
         return arbre_courant; 
      end if;

   exception
      when constraint_error => raise parente_vide;
   end An_Fils;

   function An_Nombre_Fils(arbre : in arb_nr) return integer is

   nombre_fils : integer;
   arbre_courant : arb_nr;

   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      else 
         if arbre.premier_fils /= null then
            nombre_fils := 1; 
            arbre_courant := arbre.premier_fils; 
            while arbre_courant.frere /= null loop
               nombre_fils := nombre_fils +1;
               arbre_courant := arbre_courant.frere;
            end loop;
            return nombre_fils;
         else
            return 1; 
         end if;
      end if;  
   end An_Nombre_Fils;

   ----------------     
   -- Modification   
   ----------------  

   procedure An_Inserer_Fils(arbre : in out arb_nr ; arbre_fils : in out arb_nr ) is
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      -- si l'arbre à insérer est vide, on ne fait rien   
      else 
         if arbre_fils = null then
            null;
            -- 2 cas possibles: arbre a un fils ou n'a pas de fils  
         else 
            if arbre.premier_fils = null then
               arbre.premier_fils := arbre_fils;
            -- on insère arbre_fils en position de premier fils de arbre et l'ancien fils devient le premier frere du fils inséré
            else 
               arbre_fils.frere := arbre.premier_fils;
               arbre_fils.pere := arbre;
               arbre.premier_fils := arbre_fils;
            end if;
         end if;
      end if;
   end An_Inserer_Fils;

   procedure An_Supprimer_Fils(arbre : in out arb_nr ; numero_fils : in integer) is
   -- arbres sauvegardés: précédant et suivant le fils
   arbre_suivant : arb_nr; 
   arbre_precedent : arb_nr;
      
   begin
      if An_Vide(arbre) then
         raise arbre_vide;
      -- si le n-ième fils n'existe pas, on ne fait rien
      else 
         if numero_fils > An_nombre_fils(arbre) then 
            null;
         
         else
            -- si n=1, suppression du premier fils 
            if numero_fils = 1 then 
               arbre_suivant := arbre.premier_fils;
               arbre_suivant.pere := null;
                     arbre.premier_fils := arbre_suivant.frere;
            -- on sauvegarde le pointeur sur le n-1 ième fils et le pointeur sur le frere du fils à supprimer
            else
               arbre_precedent := An_Fils(arbre, numero_fils-1);
               arbre_suivant := arbre_precedent.frere.frere;
               arbre_precedent.frere.frere := null;
               arbre_precedent.frere.pere := null;
               arbre_precedent.frere := arbre_suivant;
            end if;
         end if;
      end if;
   end An_Supprimer_Fils;

end arbre_nr;