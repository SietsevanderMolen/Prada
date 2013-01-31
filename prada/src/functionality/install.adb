with Ada.Text_IO;
with AurReplies;
with Search;

package body Install is
   procedure Install
      (Query : Ada.Strings.Unbounded.Unbounded_String)
   is
      results : AurReplies.AurReply;
      Subs    : GNAT.String_Split.Slice_Set;
   begin
      results := Search.Search (Query);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
         ("Type number to install. Separate each number with a space.");
      Ada.Text_IO.Put ("Numbers: ");

      --  Split the string by spaces so we keep the numbers
      Subs := SplitInput (Ada.Strings.Unbounded.To_Unbounded_String
         (Ada.Text_IO.Get_Line));

      for I in 1 .. GNAT.String_Split.Slice_Count (Subs) loop
         --  Loop though the substrings
         declare
            Sub : constant String := GNAT.String_Split.Slice (Subs, I);
            --  Pull the next substring out into a string for easy handling.
         begin
            for i in results.getResults.First_Index ..
               results.getResults.Last_Index loop
               --  If this is a number we want to install, go for it
               if AurPackages.GetRefNum (results.getResults.Element (i))
                  = Integer'Value (Sub) then
                  InstallPackage (results.getResults.Element (i));
               end if;
            end loop;
         end;
      end loop;
   end Install;

   procedure InstallPackage
      (Pkg : AurPackages.AurPackage)
   is
   begin
      Ada.Text_IO.Put_Line ("Installing: "
         & Ada.Strings.Unbounded.To_String (Pkg.GetName));
   end InstallPackage;

   function SplitInput
      (input : Ada.Strings.Unbounded.Unbounded_String)
      return GNAT.String_Split.Slice_Set
   is
      Subs : GNAT.String_Split.Slice_Set;
      Data : constant String := Ada.Strings.Unbounded.To_String (input);
   begin
      GNAT.String_Split.Create
         (S          => Subs,
         From       => Data,
         Separators => " ",
         Mode       => GNAT.String_Split.Multiple);
      return Subs;
   end SplitInput;

end Install;
