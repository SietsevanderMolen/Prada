with Ada.Strings.Fixed;
with POSIX.Files;
with POSIX.Permissions;
with POSIX.Process_Identification;

package body Sys is
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
      when POSIX.POSIX_Error =>
         null;  -- Directory didn't exist, no problem, continuing
   end Create_Directory;

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
   begin
      return "/tmp";
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
