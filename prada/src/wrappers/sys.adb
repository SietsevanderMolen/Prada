with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with POSIX.Files;
with POSIX.Permissions;
with POSIX.Process_Identification;
with POSIX.Process_Environment;

package body Sys is
   No_Permission_Error : exception;

   procedure Create_Directory
      (Path : String)
   is
      PosixPath : constant POSIX.POSIX_String :=
         POSIX.To_POSIX_String (Path);
      permissions : constant POSIX.Permissions.Permission_Set :=
         POSIX.Permissions.Owner_Permission_Set;
   begin
      POSIX.Files.Create_Directory (PosixPath, permissions);
   exception
      when Error : POSIX.POSIX_Error =>
         if Exception_Message (Error) = "FILE_EXISTS" then
            null;  -- No problem here, just continue
         elsif Exception_Message (Error) = "PERMISSION_DENIED" then
            raise No_Permission_Error
               with "No permission to create directory " & Path;
         end if;
   end Create_Directory;

   procedure Create_Directory_Recursive
      (Path : String)
   is
      Index_List : array (Path'Range) of Natural;
      Next_Index : Natural := Index_List'First;
   begin
      Index_List (Next_Index) := Path'First;
      while Index_List (Next_Index) < Path'Last loop
         Next_Index := Next_Index + 1;
         Index_List (Next_Index) :=
            1 + Index (Path (Index_List (Next_Index - 1) .. Path'Last), "/");
         if Index_List (Next_Index) = 1 then
            Index_List (Next_Index) := Path'Last + 2;
         end if;
         Create_Directory (Path (Index_List
            (Index_List'First) .. Index_List (Next_Index)
            - 2) & "/");
      end loop;
   end Create_Directory_Recursive;

   function FindUID return String
   is
      UserID  : constant POSIX.Process_Identification.User_ID :=
            POSIX.Process_Identification.Get_Real_User_ID;
      UserIDString : constant String :=
         POSIX.Process_Identification.Image (UserID);
   begin
      --  Have to trim a bit on the left due to the 'Image function.
      return Ada.Strings.Fixed.Trim (UserIDString, Ada.Strings.Left);
   end FindUID;

   function Get_Temp_Dir
      return String
   is
      IsSet  : constant Boolean :=
         POSIX.Process_Environment.Is_Environment_Variable ("TMP");
      TmpVal : constant POSIX.POSIX_String :=
         POSIX.Process_Environment.Environment_Value_Of ("TMP");
   begin
      if IsSet then
         return POSIX.To_String (TmpVal);
      else
         return "/tmp";
      end if;
   end Get_Temp_Dir;

   procedure Remove_Directory
      (Path : String)
   is
      PosixPath : constant POSIX.POSIX_String :=
         POSIX.To_POSIX_String (Path);
   begin
      POSIX.Files.Remove_Directory (PosixPath);
   exception
      when POSIX.POSIX_Error =>
         null;  -- Directory didn't exist, no problem, continuing
   end Remove_Directory;
end Sys;
